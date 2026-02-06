const std = @import("std");
const Rom = @import("rom.zig").Rom;

const PPUStatus = packed struct(u8) {
    /// Returns stale PPU bus contents.
    _open_bus: u5,
    /// Sprite overflow.
    sprite_overflow: bool,
    /// Sprite 0 Hit.
    sprite_0_hit: bool,
    /// Vertical blank has started
    vertical_blank: bool,
};

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

const PPUMask = packed struct(u8) {
    grayscale: bool,
    show_background_left: bool,
    show_sprites_left: bool,
    show_background: bool,
    show_sprites: bool,
    emphasize_red: bool,
    emphasize_green: bool,
    emphasize_blue: bool,
};

const WriteLatch = enum { msb, lsb };

pub const PPU = struct {
    ctrl: PPUCtrl,
    mask: PPUMask,
    status: PPUStatus,
    rom: ?*const Rom,

    addr: u16,
    scroll_x: u8,
    scroll_y: u8,

    data_buffer: u8,

    nameTable: [2][1024]u8,
    patternTable: [2][4096]u8,
    paletteTable: [32]u8,

    scanline: i16,
    cycle: i16,

    w: WriteLatch,

    nmi: bool,

    /// OAM (Object Attribute Memory) - 64 sprites, 4 bytes each
    oam: [256]u8,
    /// OAM address register
    oam_addr: u8,

    /// Frame buffer for rendered output (256x240 pixels)
    frame_buffer: [256 * 240]u32,
    /// Set to true when a frame has finished rendering
    frame_complete: bool,

    // Debug counters
    debug_nt_writes: u32,
    debug_ppudata_writes: u32,
    debug_min_ppudata_addr: u16,
    debug_max_ppudata_addr: u16,

    pub fn init() PPU {
        return PPU{
            .rom = null,
            .ctrl = @bitCast(@as(u8, 0)),
            .w = .msb,
            .addr = 0,
            .scroll_x = 0,
            .scroll_y = 0,
            .mask = @bitCast(@as(u8, 0)),
            .status = @bitCast(@as(u8, 0)),
            .nmi = false,
            .nameTable = std.mem.zeroes([2][1024]u8),
            .patternTable = std.mem.zeroes([2][4096]u8),
            .paletteTable = std.mem.zeroes([32]u8),
            .scanline = 0,
            .cycle = 0,
            .data_buffer = 0,
            .oam = undefined,
            .oam_addr = 0,
            .frame_buffer = undefined,
            .frame_complete = false,
            .debug_nt_writes = 0,
            .debug_ppudata_writes = 0,
            .debug_min_ppudata_addr = 0xFFFF,
            .debug_max_ppudata_addr = 0,
        };
    }

    pub fn load_rom(self: *PPU, rom: *const Rom) void {
        self.rom = rom;
    }

    pub fn clock(self: *PPU) void {
        // Pre-render scanline
        if (self.scanline == -1 and self.cycle == 1) {
            self.status.vertical_blank = false;
            self.status.sprite_0_hit = false;
            self.status.sprite_overflow = false;
        }

        // Visible scanlines (0-239)
        if (self.scanline >= 0 and self.scanline < 240) {
            // Visible cycles (1-256)
            if (self.cycle >= 1 and self.cycle <= 256) {
                self.render_pixel();
            }
        }

        // Start of vblank
        if (self.scanline == 241 and self.cycle == 1) {
            self.status.vertical_blank = true;
            self.frame_complete = true;
            // Debug: count non-zero tiles in both nametables
            var nz0: u16 = 0;
            var nz1: u16 = 0;
            for (0..960) |i| {
                if (self.ppu_read(0x2000 + @as(u16, @intCast(i))) != 0) nz0 += 1;
                if (self.ppu_read(0x2400 + @as(u16, @intCast(i))) != 0) nz1 += 1;
            }
            // Also check raw nameTable arrays
            var raw0: u16 = 0;
            var raw1: u16 = 0;
            for (0..960) |i| {
                if (self.nameTable[0][i] != 0) raw0 += 1;
                if (self.nameTable[1][i] != 0) raw1 += 1;
            }
            // Simplified debug - removed for now
            if (self.ctrl.nmi_enabled) {
                self.nmi = true;
            }
        }

        self.cycle += 1;

        if (self.cycle >= 341) {
            self.cycle = 0;
            self.scanline += 1;
            if (self.scanline >= 261) {
                self.scanline = -1;
                self.frame_complete = false;
            }
        }
    }

    fn render_pixel(self: *PPU) void {
        const x: u16 = @intCast(self.cycle - 1);
        const y: u16 = @intCast(self.scanline);

        var bg_pixel: u8 = 0;
        var bg_palette: u8 = 0;

        if (self.mask.show_background) {
            // Calculate tile coordinates in nametable
            const tile_x = x / 8;
            const tile_y = y / 8;

            // Fine x and y within the tile
            const fine_x: u3 = @truncate(x);
            const fine_y: u3 = @truncate(y);

            // Get nametable address based on base_nametable_addr
            const nametable_base: u16 = 0x2000 + @as(u16, self.ctrl.base_nametable_addr) * 0x400;

            // Get tile index from nametable
            const tile_addr = nametable_base + tile_y * 32 + tile_x;
            const tile_id = self.ppu_read(tile_addr);

            // Get pattern table address
            const pattern_base: u16 = @as(u16, self.ctrl.background_pattern_table_address) * 0x1000;
            const pattern_addr = pattern_base + @as(u16, tile_id) * 16 + fine_y;

            // Read the two bit planes for this row
            const tile_lsb = self.ppu_read(pattern_addr);
            const tile_msb = self.ppu_read(pattern_addr + 8);

            // Extract the pixel (bit 7 is leftmost pixel)
            const bit_shift: u3 = 7 - fine_x;
            const pixel_lsb: u8 = (tile_lsb >> bit_shift) & 1;
            const pixel_msb: u8 = (tile_msb >> bit_shift) & 1;
            bg_pixel = (pixel_msb << 1) | pixel_lsb;

            // Get attribute byte for palette selection
            // Attribute table is at nametable_base + 0x3C0
            // Each byte covers 4x4 tiles (32x32 pixels)
            const attr_x = tile_x / 4;
            const attr_y = tile_y / 4;
            const attr_addr = nametable_base + 0x3C0 + attr_y * 8 + attr_x;
            const attr_byte = self.ppu_read(attr_addr);

            // Each attribute byte has 4 palettes for 4 2x2 tile groups
            // Bits 0-1: top-left, 2-3: top-right, 4-5: bottom-left, 6-7: bottom-right
            const attr_shift: u3 = @truncate(((tile_y & 2) << 1) | (tile_x & 2));
            bg_palette = (attr_byte >> attr_shift) & 0x03;
        }

        // Get the final color
        var color: u32 = undefined;
        if (bg_pixel == 0) {
            // Transparent - use background color
            color = self.get_color_from_palette_ram(0, 0);
        } else {
            color = self.get_color_from_palette_ram(bg_palette, bg_pixel);
        }

        // Write to frame buffer
        const idx = y * 256 + x;
        self.frame_buffer[idx] = color;
    }

    pub fn cpu_read(self: *PPU, k: u16) u8 {
        return switch (k) {
            0x0000 => @bitCast(self.ctrl),
            0x0001 => @as(u8, @bitCast(self.mask)),
            0x0002 => a: {
                const s: u8 = @bitCast(self.status);
                self.status.vertical_blank = false;
                self.w = .msb;
                break :a s;
            },
            // OAMADDR is write-only
            0x0003 => 0,
            // OAM Data
            0x0004 => self.oam[self.oam_addr],
            // PPUSCROLL is write-only
            0x0005 => 0,
            // PPUADDR is write-only
            0x0006 => 0,
            0x0007 => a: {
                var d = self.data_buffer;

                self.data_buffer = self.ppu_read(self.addr);
                if (self.addr >= 0x3f00) {
                    d = self.data_buffer;
                }

                self.addr += self.ctrl.get_vram_increment();
                break :a d;
            },
            else => {
                std.debug.print("unmapped read {x}\n", .{k});
                unreachable;
            },
        };
    }

    pub fn cpu_write(self: *PPU, k: u16, v: u8) void {
        switch (k) {
            // Control
            0x0000 => {
                self.ctrl = @bitCast(v);
            },
            // Mask
            0x0001 => {
                self.mask = @bitCast(v);
            },
            // OAM Address
            0x0003 => {
                self.oam_addr = v;
            },
            // OAM Data
            0x0004 => {
                self.oam[self.oam_addr] = v;
                self.oam_addr +%= 1;
            },
            // Scroll
            0x0005 => {
                if (self.w == .msb) {
                    self.scroll_x = v;
                    self.w = .lsb;
                } else {
                    self.scroll_y = v;
                    self.w = .msb;
                }
            },
            // PPU Address
            0x0006 => {
                if (self.w == .msb) {
                    self.addr = (@as(u16, v) << 8) | (self.addr & 0x00FF);
                    self.w = .lsb;
                } else {
                    self.addr = (self.addr & 0xFF00) | @as(u16, v);
                    self.w = .msb;
                }
            },
            // PPU Data
            0x0007 => {
                self.debug_ppudata_writes += 1;
                if (self.addr < self.debug_min_ppudata_addr) self.debug_min_ppudata_addr = self.addr;
                if (self.addr > self.debug_max_ppudata_addr) self.debug_max_ppudata_addr = self.addr;
                self.ppu_write(self.addr, v);
                self.addr += self.ctrl.get_vram_increment();
            },
            else => {
                unreachable;
            },
        }
    }

    pub fn ppu_read(self: *PPU, addr: u16) u8 {
        var k = addr & 0x3fff;

        if (self.rom.?.read_chr(k)) |v| {
            return v;
        } else if (k >= 0x0000 and k <= 0x1fff) {
            return self.patternTable[(k & 0x1000) >> 12][addr & 0x0fff];
        } else if (k >= 0x2000 and k <= 0x3eff) {
            k &= 0x0fff;

            if (self.rom.?.header.flags6.vertically_mirrored) {
                return switch (k) {
                    0x0000...0x03ff => self.nameTable[0][k & 0x03ff],
                    0x0400...0x07ff => self.nameTable[1][k & 0x03ff],
                    0x0800...0x0bff => self.nameTable[0][k & 0x03ff],
                    0x0C00...0x0fff => self.nameTable[1][k & 0x03ff],
                    else => unreachable,
                };
            } else {
                return switch (k) {
                    0x0000...0x03ff => self.nameTable[0][k & 0x03ff],
                    0x0400...0x07ff => self.nameTable[0][k & 0x03ff],
                    0x0800...0x0bff => self.nameTable[1][k & 0x03ff],
                    0x0C00...0x0fff => self.nameTable[1][k & 0x03ff],
                    else => unreachable,
                };
            }
        } else if (k >= 0x3f00 and k <= 0x3fff) {
            k &= 0x001f;
            k = switch (k) {
                0x0010 => 0x0000,
                0x0014 => 0x0004,
                0x0018 => 0x0008,
                0x001c => 0x000c,
                else => k,
            };
            return self.paletteTable[k]; // TODO mask with grayscale
        } else {
            unreachable;
        }
    }

    pub fn ppu_write(self: *PPU, addr: u16, v: u8) void {
        var k = addr & 0x3fff;

        if (self.rom.?.write_chr(k, v)) {} else if (k >= 0x0000 and k <= 0x1fff) {
            self.patternTable[(k & 0x1000) >> 12][k & 0x0fff] = v;
        } else if (k >= 0x2000 and k <= 0x3eff) {
            self.debug_nt_writes += 1;
            k &= 0x0fff;

            if (self.rom.?.header.flags6.vertically_mirrored) {
                switch (k) {
                    0x0000...0x03ff => self.nameTable[0][k & 0x03ff] = v,
                    0x0400...0x07ff => self.nameTable[1][k & 0x03ff] = v,
                    0x0800...0x0bff => self.nameTable[0][k & 0x03ff] = v,
                    0x0C00...0x0fff => self.nameTable[1][k & 0x03ff] = v,
                    else => unreachable,
                }
            } else {
                switch (k) {
                    0x0000...0x03ff => self.nameTable[0][k & 0x03ff] = v,
                    0x0400...0x07ff => self.nameTable[0][k & 0x03ff] = v,
                    0x0800...0x0bff => self.nameTable[1][k & 0x03ff] = v,
                    0x0C00...0x0fff => self.nameTable[1][k & 0x03ff] = v,
                    else => unreachable,
                }
            }
        } else if (k >= 0x3f00 and k <= 0x3fff) {
            k &= 0x001f;
            k = switch (k) {
                0x0010 => 0x0000,
                0x0014 => 0x0004,
                0x0018 => 0x0008,
                0x001c => 0x000c,
                else => k,
            };

            self.paletteTable[k] = v;
        }
    }

    fn get_color_from_palette_ram(self: *PPU, palette: u8, pixel: u8) u32 {
        const idx = self.ppu_read(0x3f00 + (@as(u16, @intCast(palette)) << 2) + @as(u16, @intCast(pixel))) & 0x3f;

        return nesPalette[idx];
    }

    pub fn get_pattern_table(self: *PPU, i: u8, palette: u8, buf: []u32, sc_offset: usize) void {
        const ii = @as(u16, @intCast(i));
        for (0..16) |ty| {
            for (0..16) |tx| {
                const offset: u16 = @as(u16, @intCast(ty)) * 256 + @as(u16, @intCast(tx)) * 16;

                for (0..8) |row| {
                    var tile_lsb: u8 = self.ppu_read(ii * 0x1000 + offset + @as(u16, @intCast(row)) + 0);
                    var tile_msb: u8 = self.ppu_read(ii * 0x1000 + offset + @as(u16, @intCast(row)) + 8);

                    for (0..8) |col| {
                        const pixel: u8 = @truncate((tile_lsb & 0x01) + (tile_msb & 0x01));
                        tile_lsb >>= 1;
                        tile_msb >>= 1;

                        const x = tx * 8 + (7 - col);
                        const y = ty * 8 + row;
                        _ = self.get_color_from_palette_ram(palette, pixel);

                        buf[y * 256 + x + sc_offset] = nesPalette[pixel];
                    }
                }
            }
        }
    }
};

const nesPalette = [_]u32{ 0x757575, 0x271B8F, 0x0000AB, 0x47009F, 0x8F0077, 0xAB0013, 0xA70000, 0x7F0B00, 0x432F00, 0x004700, 0x005100, 0x003F17, 0x1B3F5F, 0x000000, 0x000000, 0x000000, 0xBCBCBC, 0x0073EF, 0x233BEF, 0x8300F3, 0xBF00BF, 0xE7005B, 0xDB2B00, 0xCB4F0F, 0x8B7300, 0x009700, 0x00AB00, 0x00933B, 0x00838B, 0x000000, 0x000000, 0x000000, 0xFFFFFF, 0x3FBFFF, 0x5F97FF, 0xA78BFD, 0xF77BFF, 0xFF77B7, 0xFF7763, 0xFF9B3B, 0xF3BF3F, 0x83D313, 0x4FDF4B, 0x58F898, 0x00EBDB, 0x000000, 0x000000, 0x000000, 0xFFFFFF, 0xABE7FF, 0xC7D7FF, 0xD7CBFF, 0xFFC7FF, 0xFFC7DB, 0xFFBFB3, 0xFFDBAB, 0xFFE7A3, 0xE3FFA3, 0xABF3BF, 0xB3FFCF, 0x9FFFF3, 0x000000, 0x000000, 0x000000 };
