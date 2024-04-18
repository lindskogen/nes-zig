const std = @import("std");
const Rom = @import("rom.zig").Rom;


const PPUCtrl = packed struct(u8) {
  /// Base nametable address
  /// 0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00
  base_nametable_addr: u2,
  /// VRAM address increment per CPU read/write of PPUDATA
  /// 0: add 1, going across; 1: add 32, going down
  vram_address_increment: u1,
  /// Sprite pattern table address for 8x8 sprites
  /// 0: $0000; 1: $1000; ignored in 8x16 mode
  sprite_pattern_table_addr: u1,
  /// Background pattern table address
  /// 0: $0000; 1: $1000
  background_pattern_table_address: u1,
  /// Sprite size
  /// 0: 8x8 pixels; 1: 8x16 pixels
  sprite_size: u1,
  /// PPU master/slave select
  /// 0: read backdrop from EXT pins; 1: output color on EXT pins
  ppu_master_slave_select: u1,
  /// Generate an NMI at the start of the vertical blanking interval
  nmi_enabled: bool,

  inline fn get_vram_increment(self: PPUCtrl) u16 {
    if (self.vram_address_increment == 1) {
      return 32;
    } else {
      return 1;
    }
  }
};

const WriteLatch = enum { msb, lsb };

pub const PPU = struct {
  internal: [0x4000]u8,
  ctrl: PPUCtrl,
  mask: u8, // TODO map this
  rom: *const Rom,

  addr: u16,
  scroll: u16,

  w: WriteLatch,

  pub fn read(self: *PPU, addr: u16) u8 {
    var a = addr % 0x4000;
    if (a > 0x3f20) {
      a -= 0x1f;
    } else if (a < 0x3f00 and a >= 0x3000) {
      a -= 0x3000;
    }

    return switch (a) {
      0x0000...0x1fff => self.rom.read_chr(a),
      else => self.internal[addr]
    };
  }

  pub fn write_to_buffer(self: *PPU, buffer: []u32, comptime width: usize, comptime height: usize) void  {
    for (0..30) |y| {
      for (0..32) |x| {
        const v: u16 = @intCast(x);

        // Fetch a nametable entry from $2000-$2FFF.
        const tile_address: u16 = @intCast(0x2000 | (v & 0x0FFF));
        const tile = self.read(tile_address);
        _ = tile;


        // Fetch the corresponding attribute table entry from $23C0-$2FFF and
        const attribute_address: u16 = 0x23C0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07);
        const attribute = self.read(attribute_address);
        _ = attribute;
        // TODO: increment the current VRAM address within the same row.

        // Fetch the low-order byte of an 8x1 pixel sliver of pattern table from $0000-$0FF7 or $1000-$1FF7.
        const bg_table_offset = @as(u16, @intCast(self.ctrl.background_pattern_table_address)) * 0x1000;
        const lsb_sliver_addr = bg_table_offset + 0x00;
        const lsb = self.read(lsb_sliver_addr);
        _ = lsb;

        // Fetch the high-order byte of this sliver from an address 8 bytes higher.
        const msb_sliver_addr = bg_table_offset + 8;
        const msb = self.read(msb_sliver_addr);
        _ = msb;


        // Turn the attribute data and the pattern table data into palette indices, and combine them with data from sprite data using priority.

        _ = buffer[y + x + width + height];
      }
      // TODO: It also does a fetch of a 34th (nametable, attribute, pattern) tuple that is never used, but some mappers rely on this fetch for timing purposes.
    }

  }
};

pub const Mem = struct {
  rom: *const Rom,
  internal: [0x10000]u8,
  ppu: PPU,

  pub fn init(rom: *const Rom) Mem {
    return Mem {
      .rom = rom,
      .internal = undefined,
      .ppu = PPU { .rom = rom, .internal = undefined, .ctrl = @bitCast(@as(u8, 0)), .w = .msb, .addr = 0, .scroll = 0, .mask = 0 }
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
      0x2007 => {
        self.ppu.internal[self.ppu.addr] = v;
        self.ppu.addr += self.ppu.ctrl.get_vram_increment();
      },
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
      0x2007 => {
        const r = self.ppu.read(self.ppu.addr);
        self.ppu.addr += self.ppu.ctrl.get_vram_increment();
        return r;
      },
      0x8000...0xffff => self.rom.read_prg(k - 0x8000),
      else => self.internal[k],
    };
  }
};
