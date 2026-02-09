const std = @import("std");
const Rom = @import("rom.zig").Rom;
const CPU = @import("cpu.zig").CPU;
const PPU = @import("ppu.zig").PPU;
const APU = @import("apu.zig").APU;

pub const Bus = struct {
    cpu: CPU,

    rom: ?*Rom,

    /// CPU ram
    ram: [2048]u8,
    /// PRG RAM ($6000-$7FFF)
    prg_ram: [8192]u8,
    ppu: PPU,
    apu: APU,

    cycles: u32,

    controllers: [2]u8,
    controllers_cache: [2]u8,

    // OAM DMA state
    dma_active: bool,
    dma_page: u8,
    dma_byte: u16,
    dma_data: u8,
    dma_dummy: bool,

    // DMC cycle steal state
    dmc_stall: u8,

    pub fn init() Bus {
        return Bus{
            .rom = null,
            .ram = std.mem.zeroes([2048]u8),
            .prg_ram = std.mem.zeroes([8192]u8),
            .cpu = CPU.init(),
            .ppu = PPU.init(),
            .apu = APU.init(),
            .cycles = 0,
            .controllers = .{ 0, 0 },
            .controllers_cache = .{ 0, 0 },
            .dma_active = false,
            .dma_page = 0,
            .dma_byte = 0,
            .dma_data = 0,
            .dma_dummy = true,
            .dmc_stall = 0,
        };
    }

    pub fn load_rom(self: *Bus, rom: *Rom) void {
        self.rom = rom;
        self.ppu.load_rom(rom);
    }

    pub fn write(self: *Bus, k: u16, v: u8) void {
        if (self.rom.?.write_prg(k, v)) {

            // ROM handled write

        } else if (k >= 0x0000 and k <= 0x1fff) {
            self.ram[k & 0x07ff] = v;
        } else if (k >= 0x2000 and k <= 0x3fff) {
            self.ppu.cpu_write(k & 0x0007, v);
        } else if (k >= 0x4000 and k <= 0x4013) {
            self.apu.cpu_write(k, v);
        } else if (k == 0x4014) {
            // OAM DMA — trigger DMA transfer
            self.dma_active = true;
            self.dma_page = v;
            self.dma_byte = 0;
            self.dma_dummy = true;
        } else if (k == 0x4015) {
            self.apu.cpu_write(k, v);
        } else if (k == 0x4016) {
            self.controllers_cache[0] = self.controllers[0];
        } else if (k == 0x4017) {
            // $4017 write goes to APU frame counter
            self.apu.cpu_write(k, v);
        } else if (k >= 0x4018 and k <= 0x5FFF) {
            // Open bus - ignore writes
        } else if (k >= 0x6000 and k <= 0x7FFF) {
            self.prg_ram[k - 0x6000] = v;
        } else {
            std.debug.print("Unmapped write bus {x}", .{k});
            unreachable;
        }
    }

    pub fn read(self: *Bus, k: u16) u8 {
        if (self.rom.?.read_prg(k)) |res| {
            return res;
        } else if (k >= 0x0000 and k <= 0x1fff) {
            return self.ram[k & 0x07ff];
        } else if (k >= 0x2000 and k <= 0x3fff) {
            return self.ppu.cpu_read(k & 0x0007);
        } else if (k == 0x4015) {
            return self.apu.cpu_read(k);
        } else if (k >= 0x4000 and k <= 0x4014) {
            // APU registers (write-only), open bus
            return 0x00;
        } else if (k >= 0x4016 and k <= 0x4017) {
            const r = (self.controllers_cache[k & 0x0001] & 0x80) > 0;
            self.controllers_cache[k & 0x0001] <<= 1;
            return if (r) 1 else 0;
        } else if (k >= 0x4018 and k <= 0x5FFF) {
            // Open bus
            return 0;
        } else if (k >= 0x6000 and k <= 0x7FFF) {
            return self.prg_ram[k - 0x6000];
        }

        std.debug.print("Unmapped read bus {x}", .{k});

        unreachable;
    }

    pub fn reset(self: *Bus) void {
        self.cpu.reset();
        self.cycles = 0;
        self.dma_active = false;
        self.dmc_stall = 0;
    }

    pub fn clock(self: *Bus) void {
        self.ppu.clock();

        if (self.cycles % 3 == 0) {
            // Wire interrupt lines BEFORE CPU clock for proper edge detection
            self.cpu.nmi_line = self.ppu.nmi_output;
            self.cpu.irq_line = self.apu.irq_pending;

            if (self.dma_active) {
                self.clock_dma();
            } else if (self.dmc_stall > 0) {
                self.clock_dmc_stall();
            } else {
                self.cpu.clock();
                self.apu.clock();
            }

            // Service DMC read requests
            if (self.apu.dmc_read_pending and self.dmc_stall == 0) {
                self.dmc_stall = 4;
            }
        }

        self.cycles += 1;
    }

    fn clock_dma(self: *Bus) void {
        if (self.dma_dummy) {
            // Alignment cycle — wait for even CPU cycle
            if (self.cycles % 6 == 0) {
                // Odd CPU cycle: extra dummy
            } else {
                self.dma_dummy = false;
            }
        } else {
            const cpu_cycle = self.cycles / 3;
            if (cpu_cycle % 2 == 0) {
                // Read cycle (even)
                self.dma_data = self.read((@as(u16, self.dma_page) << 8) | @as(u16, @truncate(self.dma_byte)));
            } else {
                // Write cycle (odd)
                self.ppu.oam[self.dma_byte] = self.dma_data;
                self.dma_byte += 1;
                if (self.dma_byte == 256) {
                    self.dma_active = false;
                }
            }
        }
        // APU still clocks during DMA
        self.apu.clock();
    }

    fn clock_dmc_stall(self: *Bus) void {
        self.dmc_stall -= 1;
        if (self.dmc_stall == 0) {
            // Perform the DMC read on the last stall cycle
            self.apu.dmc_read_pending = false;
            const data = self.read(self.apu.dmc_read_addr);
            self.apu.dmc.fill_sample_buffer(data);
        }
        // APU still clocks during DMC stall
        self.apu.clock();
    }
};
