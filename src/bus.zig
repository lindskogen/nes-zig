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


  pub fn init() Bus {
    return Bus {
      .rom = null,
      .ram = undefined,
      .cpu = CPU.init(),
      .ppu = PPU.init(),
      .cycles = 0
    };
  }

  pub fn load_rom(self: *Bus, rom: *const Rom) void {
    self.rom = rom;
    self.ppu.load_rom(rom);
  }

  pub fn write(self: *Bus, k: u16, v: u8) void {
    if (self.rom.?.write_prg(k, v)) {

      // ROM handled write
      return;

    } else if (k >= 0x0000 and k <= 0x1fff) {
      self.ram[k & 0x07ff] = v;
      return;
    } else if (k >= 0x2000 and k <= 0x3fff) {
      self.ppu.cpu_write(k & 0x0007, v);
      return;
    }

    std.log.debug("Unmapped write bus {x}", .{ k });
    unreachable;
  }

  pub fn read(self: *Bus, k: u16) u8 {
    if (self.rom.?.read_prg(k)) |res| {

      return res;

    } else if (k >= 0x0000 and k <= 0x1fff) {
      return self.ram[k & 0x07ff];
    } else if (k >= 0x2000 and k <= 0x3fff) {
      return self.ppu.cpu_read(k & 0x0007);
    }

    std.log.debug("Unmapped read bus {x}", .{ k });
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
