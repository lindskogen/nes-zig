const std = @import("std");
const Rom = @import("rom.zig").Rom;
const PPU = @import("ppu.zig").PPU;

pub const Mem = struct {
  rom: *const Rom,
  internal: [0x10000]u8,
  ppu: PPU,

  pub fn init(rom: *const Rom) Mem {
    return Mem {
      .rom = rom,
      .internal = undefined,
      .ppu = PPU.init(rom)
    };
  }

  pub fn write(self: *Mem, k: u16, v: u8) void {
    switch (k) {
      0x2000 => {
        self.ppu.ctrl = @bitCast(v);
      },
      0x2001 => {
        self.ppu.mask = v;
      },
      0x2005 => {
        if (self.ppu.w == .msb) {
          self.ppu.scroll = (@as(u16, v) << 8) | @as(u8, @truncate(self.ppu.scroll));
          self.ppu.w = .lsb;
          } else {
          self.ppu.scroll = @as(u16, v) | @as(u8, @truncate(self.ppu.scroll >> 8));
          self.ppu.w = .msb;
        }
      },
      0x2006 => {
        if (self.ppu.w == .msb) {
          self.ppu.addr = (@as(u16, v) << 8) | @as(u8, @truncate(self.ppu.addr));
          self.ppu.w = .lsb;
        } else {
          self.ppu.addr = @as(u16, v) | @as(u8, @truncate(self.ppu.addr >> 8));
          self.ppu.w = .msb;
        }
      },
      0x2007 => self.ppu.write_vram(v),
      0x2004 => {
        std.debug.print("DMA {x} {x}\n", .{ k, v });
        unreachable;
      },
      0x4014 => {
        std.debug.print("DMA {x} {x}\n", .{ k, v });
        unreachable;
      },
      else => {
        self.internal[k] = v;
      }
    }
  }

  pub fn read(self: *Mem, k: u16) u8 {
    return switch (k) {
      0x2000 => unreachable,
      0x2001 => unreachable,
      0x2003 => unreachable,
      0x2005 => unreachable,
      0x2006 => unreachable,
      0x2007 => self.ppu.read_vram(),
      0x8000...0xffff => self.rom.read_prg(k - 0x8000),
      else => self.internal[k],
    };
  }
};
