const std = @import("std");
const Mem = @import("mem.zig").Mem;

pub const MAX_SIZE = 512_000;

pub fn load_into_memory(mem: *Mem, rom_buffer: []u8) void {
  const header = try Header.parse(rom_buffer[0..16]);
  std.debug.print("Loaded: {}\n", .{ header });

  const start_offset: usize = if (header.flags6.has_trainer) (16 + 512) else 16;
  const prg_rom_len = @as(usize, header.prg_rom_size) * 16_384;

  std.mem.copyForwards(u8, mem[0x8000..0xffff], rom_buffer[start_offset..(start_offset + prg_rom_len - 1)]);
}


const Flags6 = packed struct(u8) {
  nametable_arrangement: u1 = 0,
  battery_backed_prg_ram: bool = false,
  has_trainer: bool = false,
  _padding: u5 = 0 // TODO: Map rest of Flags6
};

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
  flags7: u8 = 0,
  /// 8	Flags 8 – PRG-RAM size (rarely used extension)
  flags8: u8 = 0,
  /// 9	Flags 9 – TV system (rarely used extension)
  flags9: u8 = 0,
  /// 10	Flags 10 – TV system, PRG-RAM presence (unofficial, rarely used extension)
  flags10: u8 = 0,
  /// 11-15	Unused padding (should be filled with zero, but some rippers put their name across bytes 7-15)
  _padding: [5]u8 = undefined,


  fn parse(slice: *[16]u8) !Header {
    return Header {
      .nes = slice[0..4].*,
      .prg_rom_size = slice[4],
      .chr_rom_size = slice[5],
      .flags6 = @bitCast(slice[6]),
      .flags7 = slice[7],
      .flags8 = slice[8],
      .flags9 = slice[9],
      .flags10 = slice[10],
      ._padding = slice[11..16].*
      };
    }
};
