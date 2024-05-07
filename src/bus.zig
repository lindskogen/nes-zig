const std = @import("std");
const Rom = @import("rom.zig").Rom;
const CPU = @import("cpu.zig").CPU;
const PPU = @import("ppu.zig").PPU;

pub const Bus = struct {
    cpu: CPU,

    rom: ?*const Rom,

    /// CPU ram
    ram: [2048]u8,
    ppu: PPU,

    cycles: u32,

    controllers: [2]u8,
    controllers_cache: [2]u8,

    pub fn init() Bus {
        return Bus{ .rom = null, .ram = undefined, .cpu = CPU.init(), .ppu = PPU.init(), .cycles = 0, .controllers = undefined, .controllers_cache = undefined };
    }

    pub fn load_rom(self: *Bus, rom: *const Rom) void {
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
        } else if (k >= 0x4000 and k <= 0x4015) {
            // TODO: APU
        } else if (k >= 0x4016 and k <= 0x4017) {
            self.controllers_cache[k & 0x0001] = self.controllers[k & 0x0001];
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
        } else if (k >= 0x4000 and k <= 0x4015) {
            // APU
            return 0x00;
        } else if (k >= 0x4016 and k <= 0x4017) {
            const r = (self.controllers_cache[k & 0x0001] & 0x80) > 0;
            self.controllers_cache[k & 0x0001] <<= 1;
            return if (r) 1 else 0;
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
        }

        self.cycles += 1;
    }
};
