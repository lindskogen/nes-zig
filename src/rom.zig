const std = @import("std");

pub const MAX_SIZE = 512_000;

const Flags6 = packed struct(u8) { vertically_mirrored: bool = false, battery_backed_prg_ram: bool = false, has_trainer: bool = false, alternative_nametable_layout: bool = false, lower_nybble_of_mapper_num: u4 = 0 };

const Flags7 = packed struct(u8) { _unmapped: u4 = 0, upper_nybble_of_mapper_num: u4 = 0 };

pub const MirrorMode = enum { one_screen_lower, one_screen_upper, vertical, horizontal };

pub const ParseError = error{invalidHeader};

const Header = struct {
    /// Constant $4E $45 $53 $1A (ASCII "NES" followed by MS-DOS end-of-file)
    nes: [4]u8 = undefined,
    /// Size of PRG ROM in 16 KB units
    prg_rom_size: u8 = 0,
    /// Size of CHR ROM in 8 KB units (value 0 means the board uses CHR RAM)
    chr_rom_size: u8 = 0,
    /// 6 Flags 6 – Mapper, mirroring, battery, trainer
    flags6: Flags6 = Flags6{},
    /// 7 Flags 7 – Mapper, VS/Playchoice, NES 2.0
    flags7: Flags7 = Flags7{},
    /// 8 Flags 8 – PRG-RAM size (rarely used extension)
    flags8: u8 = 0,
    /// 9 Flags 9 – TV system (rarely used extension)
    flags9: u8 = 0,
    /// 10 Flags 10 – TV system, PRG-RAM presence (unofficial, rarely used extension)
    flags10: u8 = 0,
    /// 11-15 Unused padding (should be filled with zero, but some rippers put their name across bytes 7-15)
    _padding: [5]u8 = undefined,

    fn parse(slice: *[16]u8) !Header {
        const checksum = slice[0..4];
        if (!std.mem.eql(u8, checksum, "NES\x1a")) {
            return ParseError.invalidHeader;
        }

        return Header{ .nes = checksum.*, .prg_rom_size = slice[4], .chr_rom_size = slice[5], .flags6 = @bitCast(slice[6]), .flags7 = @bitCast(slice[7]), .flags8 = slice[8], .flags9 = slice[9], .flags10 = slice[10], ._padding = slice[11..16].* };
    }
};

const Mmc1State = struct {
    shift_register: u5 = 0b10000,
    write_count: u3 = 0,
    control: u5 = 0x0C, // Initial: PRG mode 3, CHR mode 0
    chr_bank_0: u5 = 0,
    chr_bank_1: u5 = 0,
    prg_bank: u5 = 0,
};

const MapperState = union(enum) { nrom: void, mmc1: Mmc1State };

pub const Rom = struct {
    header: Header,
    mapper_state: MapperState,
    buffer: []u8,
    unchecked: bool,

    prg_slice: []u8,
    chr_slice: []u8,
    chr_ram: [8192]u8,

    pub fn load_unchecked(rom_buffer: []u8) !Rom {
        return Rom{
            .buffer = rom_buffer[0..],
            .mapper_state = .{ .nrom = {} },
            .header = Header{},
            .unchecked = true,
            .prg_slice = rom_buffer[0..],
            .chr_slice = &[_]u8{},
            .chr_ram = std.mem.zeroes([8192]u8),
        };
    }

    pub fn load(rom_buffer: []u8) !Rom {
        const header = try Header.parse(rom_buffer[0..16]);
        const start_offset_prg: usize = if (header.flags6.has_trainer) (16 + 512) else 16;
        const prg_rom_len = @as(usize, header.prg_rom_size) * 16_384;
        const chr_rom_start = start_offset_prg + prg_rom_len;
        const chr_rom_len = @as(usize, header.chr_rom_size) * 8192;

        const mapper: u8 = (@as(u8, @intCast(header.flags7.upper_nybble_of_mapper_num)) << 4) | @as(u8, @intCast(header.flags6.lower_nybble_of_mapper_num));

        const mapper_state: MapperState = switch (mapper) {
            0 => .{ .nrom = {} },
            1 => .{ .mmc1 = Mmc1State{} },
            else => {
                std.debug.print("Unimplemented mapper: {}\n", .{mapper});
                unreachable;
            },
        };

        return Rom{
            .buffer = rom_buffer[16..],
            .header = header,
            .mapper_state = mapper_state,
            .unchecked = false,
            .prg_slice = rom_buffer[start_offset_prg..(start_offset_prg + prg_rom_len)],
            .chr_slice = rom_buffer[chr_rom_start..(chr_rom_start + chr_rom_len)],
            .chr_ram = std.mem.zeroes([8192]u8),
        };
    }

    pub fn get_mirror_mode(self: *Rom) MirrorMode {
        switch (self.mapper_state) {
            .nrom => return if (self.header.flags6.vertically_mirrored) .vertical else .horizontal,
            .mmc1 => |mmc1| {
                return switch (@as(u2, @truncate(mmc1.control))) {
                    0 => .one_screen_lower,
                    1 => .one_screen_upper,
                    2 => .vertical,
                    3 => .horizontal,
                };
            },
        }
    }

    pub fn read_prg(self: *Rom, k: u16) ?u8 {
        if (self.unchecked) {
            return self.prg_slice[k];
        }

        if (k < 0x8000) return null;

        switch (self.mapper_state) {
            .nrom => {
                const mask: u16 = if (self.header.prg_rom_size > 1) 0x7fff else 0x3fff;
                return self.prg_slice[k & mask];
            },
            .mmc1 => |mmc1| {
                const prg_mode = @as(u2, @truncate(mmc1.control >> 2));
                const bank: u8 = @as(u8, mmc1.prg_bank) & 0x0F;
                const num_banks = self.header.prg_rom_size;
                const offset: usize = switch (prg_mode) {
                    // Mode 0,1: 32KB switching
                    0, 1 => blk: {
                        const bank32 = @as(u8, mmc1.prg_bank) & 0x0E;
                        break :blk @as(usize, bank32 % num_banks) * 16384 + (k - 0x8000);
                    },
                    // Mode 2: fix first bank at $8000, switch $C000
                    2 => blk: {
                        if (k < 0xC000) {
                            break :blk @as(usize, k - 0x8000);
                        } else {
                            break :blk @as(usize, bank % num_banks) * 16384 + (k - 0xC000);
                        }
                    },
                    // Mode 3: switch $8000, fix last bank at $C000
                    3 => blk: {
                        if (k < 0xC000) {
                            break :blk @as(usize, bank % num_banks) * 16384 + (k - 0x8000);
                        } else {
                            break :blk @as(usize, num_banks - 1) * 16384 + (k - 0xC000);
                        }
                    },
                };

                if (offset < self.prg_slice.len) {
                    return self.prg_slice[offset];
                }
                return 0;
            },
        }
    }

    pub fn write_prg(self: *Rom, k: u16, v: u8) bool {
        if (self.unchecked) {
            self.prg_slice[k] = v;
            return true;
        }

        if (k < 0x8000) return false;

        switch (self.mapper_state) {
            .nrom => {
                // NROM ignores writes to ROM space
                return true;
            },
            .mmc1 => |*mmc1| {
                // Bit 7 set: reset shift register
                if (v & 0x80 != 0) {
                    mmc1.shift_register = 0b10000;
                    mmc1.write_count = 0;
                    mmc1.control |= 0x0C; // Set PRG mode to 3
                    return true;
                }

                // Shift bit 0 into shift register
                mmc1.shift_register = (@as(u5, @truncate(v)) & 1) << 4 | (mmc1.shift_register >> 1);
                mmc1.write_count += 1;

                // On 5th write, dispatch to target register
                if (mmc1.write_count == 5) {
                    const reg = (k >> 13) & 0x03;
                    switch (reg) {
                        0 => mmc1.control = mmc1.shift_register,
                        1 => mmc1.chr_bank_0 = mmc1.shift_register,
                        2 => mmc1.chr_bank_1 = mmc1.shift_register,
                        3 => mmc1.prg_bank = mmc1.shift_register,
                        else => unreachable,
                    }
                    mmc1.shift_register = 0b10000;
                    mmc1.write_count = 0;
                }
                return true;
            },
        }
    }

    pub fn read_chr(self: *Rom, k: u16) ?u8 {
        if (k > 0x1FFF) return null;

        switch (self.mapper_state) {
            .nrom => {
                if (self.chr_slice.len > 0) {
                    return self.chr_slice[k];
                }
                return self.chr_ram[k];
            },
            .mmc1 => |mmc1| {
                if (self.header.chr_rom_size == 0) {
                    // CHR RAM
                    return self.chr_ram[k];
                }

                const chr_mode = (mmc1.control >> 4) & 1;
                const offset: usize = if (chr_mode == 0) blk: {
                    // 8KB mode: bank = chr_bank_0 with bit 0 cleared
                    const bank = @as(usize, mmc1.chr_bank_0 & 0x1E);
                    break :blk bank * 4096 + k;
                } else blk: {
                    // 4KB mode
                    if (k < 0x1000) {
                        break :blk @as(usize, mmc1.chr_bank_0) * 4096 + k;
                    } else {
                        break :blk @as(usize, mmc1.chr_bank_1) * 4096 + (k - 0x1000);
                    }
                };

                if (offset < self.chr_slice.len) {
                    return self.chr_slice[offset];
                }
                return 0;
            },
        }
    }

    pub fn write_chr(self: *Rom, k: u16, v: u8) bool {
        if (k > 0x1FFF) return false;

        switch (self.mapper_state) {
            .nrom => {
                if (self.header.chr_rom_size == 0) {
                    self.chr_ram[k] = v;
                    return true;
                }
                return false;
            },
            .mmc1 => {
                if (self.header.chr_rom_size == 0) {
                    self.chr_ram[k] = v;
                    return true;
                }
                return false;
            },
        }
    }
};
