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

    pub fn init() Bus {
        return Bus{ .rom = null, .ram = undefined, .prg_ram = std.mem.zeroes([8192]u8), .cpu = CPU.init(), .ppu = PPU.init(), .apu = APU.init(), .cycles = 0, .controllers = undefined, .controllers_cache = undefined };
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
            // OAM DMA - copy 256 bytes from CPU memory to OAM
            const base: u16 = @as(u16, v) << 8;
            for (0..256) |i| {
                self.ppu.oam[i] = self.read(base + @as(u16, @intCast(i)));
            }
            // DMA takes 513/514 CPU cycles, but we'll ignore timing for now
        } else if (k == 0x4015) {
            self.apu.cpu_write(k, v);
        } else if (k == 0x4016) {
            self.controllers_cache[0] = self.controllers[0];
        } else if (k == 0x4017) {
            // $4017 write goes to APU frame counter
            self.apu.cpu_write(k, v);
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
        } else if (k >= 0x6000 and k <= 0x7FFF) {
            return self.prg_ram[k - 0x6000];
        }

        std.debug.print("Unmapped read bus {x}", .{k});

        unreachable;
    }

    pub fn reset(self: *Bus) void {
        self.cpu.reset();
        self.cycles = 0;
    }

    pub fn clock(self: *Bus) void {
        self.ppu.clock();

        if (self.cycles % 3 == 0) {
            self.cpu.clock();
            self.apu.clock();

            // Service DMC read requests
            if (self.apu.dmc_read_pending) {
                self.apu.dmc_read_pending = false;
                const data = self.read(self.apu.dmc_read_addr);
                self.apu.dmc.fill_sample_buffer(data);
            }
        }

        if (self.ppu.nmi) {
            self.ppu.nmi = false;
            self.cpu.nmi();
        }

        if (self.apu.irq_pending) {
            self.cpu.irq();
        }

        self.cycles += 1;
    }
};
