const std = @import("std");

pub const MAX_SIZE = 512_000;

const Flags6 = packed struct(u8) {
  vertically_mirrored: bool = false,
  battery_backed_prg_ram: bool = false,
  has_trainer: bool = false,
  alternative_nametable_layout: bool = false,
  lower_nybble_of_mapper_num: u4 = 0
};

const Flags7 = packed struct(u8) {
  _unmapped: u4 = 0,
  upper_nybble_of_mapper_num: u4 = 0
};

pub const ParseError = error { invalidHeader };

const Header = struct {
  /// Constant $4E $45 $53 $1A (ASCII "NES" followed by MS-DOS end-of-file)
  nes: [4]u8 = undefined,
  /// Size of PRG ROM in 16 KB units
  prg_rom_size: u8 = 0,
  /// Size of CHR ROM in 8 KB units (value 0 means the board uses CHR RAM)
  chr_rom_size: u8 = 0,
  /// 6	Flags 6 – Mapper, mirroring, battery, trainer
  flags6: Flags6 = Flags6 {},
  /// 7	Flags 7 – Mapper, VS/Playchoice, NES 2.0
  flags7: Flags7 = Flags7 {},
  /// 8	Flags 8 – PRG-RAM size (rarely used extension)
  flags8: u8 = 0,
  /// 9	Flags 9 – TV system (rarely used extension)
  flags9: u8 = 0,
  /// 10	Flags 10 – TV system, PRG-RAM presence (unofficial, rarely used extension)
  flags10: u8 = 0,
  /// 11-15	Unused padding (should be filled with zero, but some rippers put their name across bytes 7-15)
  _padding: [5]u8 = undefined,

  fn parse(slice: *[16]u8) !Header {

    const checksum = slice[0..4];
    if (!std.mem.eql(u8, checksum, "NES\x1a")) {
      return ParseError.invalidHeader;
    }

    return Header {
      .nes = checksum.*,
      .prg_rom_size = slice[4],
      .chr_rom_size = slice[5],
      .flags6 = @bitCast(slice[6]),
      .flags7 = @bitCast(slice[7]),
      .flags8 = slice[8],
      .flags9 = slice[9],
      .flags10 = slice[10],
      ._padding = slice[11..16].*
      };
    }
};

pub const Rom = struct {
  header: Header,
  mapper: u8,
  buffer: []u8,
  unchecked: bool,

  prg_slice: []u8,
  chr_slice: []u8,

  pub fn load_unchecked(rom_buffer: []u8) !Rom {
    return Rom {
      .buffer = rom_buffer[0..],
      .header = Header {},
      .unchecked = true,
      .prg_slice = rom_buffer[0..],
      .chr_slice = &[_]u8{}
    };
  }

  pub fn load(rom_buffer: []u8) !Rom {
    const header = try Header.parse(rom_buffer[0..16]);
    const start_offset_prg: usize = if (header.flags6.has_trainer) (16 + 512) else 16;
    const prg_rom_len = @as(usize, header.prg_rom_size) * 16_384;
    const chr_rom_start = start_offset_prg + prg_rom_len;
    const chr_rom_len = @as(usize, header.chr_rom_size) * 8192;

    const mapper: u8 = (@as(u8, @intCast(header.flags7.upper_nybble_of_mapper_num)) << 4) | @as(u8, @intCast(header.flags6.lower_nybble_of_mapper_num));

    if (mapper != 0) {
      std.debug.print("Unimplemented mapper: {}\n", .{ mapper });
      unreachable;
    }

    return Rom {
      .buffer = rom_buffer,
      .header = header,
      .mapper = mapper,
      .unchecked = false,
      .prg_slice = rom_buffer[start_offset_prg..(start_offset_prg + prg_rom_len)],
      .chr_slice = rom_buffer[chr_rom_start..(chr_rom_start + chr_rom_len)],
    };
  }

  pub fn read_prg(self: *const Rom, k: u16) ?u8 {
    if (self.unchecked) {
      return self.prg_slice[k];
    }

    if (k >= 0x8000 and k <= 0xffff) {
      const mask: u16 = if (self.header.prg_rom_size > 1) 0x7fff else 0x3fff;
      const addr: u16 = k & mask;
      return self.prg_slice[addr];
    }

    return null;
  }

  pub fn write_prg(self: *const Rom, k: u16, v: u8) bool {
    if (self.unchecked) {
      self.prg_slice[k] = v;
      return true;
    }
    if (k >= 0x8000 and k <= 0xffff) {
      const mask: u16 = if (self.header.prg_rom_size > 1) 0x7fff else 0x3fff;
      const addr: u16 = k & mask;
      self.prg_slice[addr] = v;
      return true;
    }
    return false;
  }

  pub fn read_chr(self: *const Rom, k: u16) ?u8 {
    if (k >= 0x0000 and k <= 0x1FFF) {
      return self.chr_slice[k];
    }
    return null;
  }

  pub fn write_chr(self: *const Rom, k: u16, v: u8) bool {
    if (k >= 0x0000 and k <= 0x1FFF) {
      if (self.header.chr_rom_size == 0) {
        self.chr_slice[k] = v;
        return true;
      }
    }
    return false;
  }
};
