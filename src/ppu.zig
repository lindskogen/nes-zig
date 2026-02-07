const std = @import("std");
const rom_mod = @import("rom.zig");
const Rom = rom_mod.Rom;
const MirrorMode = rom_mod.MirrorMode;

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
    rom: ?*Rom,

    /// Loopy v register - current VRAM address / scroll position (15 bits used)
    /// Layout: 0yyy NNYY YYYX XXXX
    ///   X: coarse X scroll (5 bits)
    ///   Y: coarse Y scroll (5 bits)
    ///   N: nametable select (2 bits)
    ///   y: fine Y scroll (3 bits)
    v: u16,
    /// Loopy t register - temporary VRAM address (15 bits used)
    t: u16,
    /// Fine X scroll (3 bits)
    fine_x: u3,

    data_buffer: u8,

    nameTable: [2][1024]u8,
    patternTable: [2][4096]u8,
    paletteTable: [32]u8,

    scanline: i16,
    cycle: i16,

    w: WriteLatch,

    nmi_output: bool,

    // DEBUG
    trace_writes: bool = false,

    /// OAM (Object Attribute Memory) - 64 sprites, 4 bytes each
    oam: [256]u8,
    /// OAM address register
    oam_addr: u8,

    /// Frame buffer for rendered output (256x240 pixels)
    frame_buffer: [256 * 240]u32,
    /// Set to true when a frame has finished rendering
    frame_complete: bool,

    pub fn init() PPU {
        return PPU{
            .rom = null,
            .ctrl = @bitCast(@as(u8, 0)),
            .w = .msb,
            .v = 0,
            .t = 0,
            .fine_x = 0,
            .mask = @bitCast(@as(u8, 0)),
            .status = @bitCast(@as(u8, 0)),
            .nmi_output = false,
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
        };
    }

    pub fn load_rom(self: *PPU, rom: *Rom) void {
        self.rom = rom;
    }

    pub fn clock(self: *PPU) void {
        const rendering_enabled = self.mask.show_background or self.mask.show_sprites;
        const visible_line = self.scanline >= 0 and self.scanline < 240;
        const pre_render_line = self.scanline == -1;

        // Pre-render scanline: clear flags
        if (pre_render_line and self.cycle == 1) {
            self.status.vertical_blank = false;
            self.status.sprite_0_hit = false;
            self.status.sprite_overflow = false;
            self.nmi_output = false;
        }

        // Visible scanlines: render pixels
        if (visible_line and self.cycle >= 1 and self.cycle <= 256) {
            self.render_pixel();
        }

        // V register updates during rendering
        if (rendering_enabled and (visible_line or pre_render_line)) {
            // Increment coarse X every 8 cycles during visible pixel rendering
            if (self.cycle >= 8 and self.cycle <= 256 and self.cycle & 7 == 0) {
                self.increment_coarse_x();
            }

            // Increment Y at end of visible pixels
            if (self.cycle == 256) {
                self.increment_y();
            }

            // Copy horizontal bits from t to v
            if (self.cycle == 257) {
                self.v = (self.v & ~@as(u16, 0x041F)) | (self.t & 0x041F);
            }

            // Copy vertical bits from t to v during pre-render
            if (pre_render_line and self.cycle >= 280 and self.cycle <= 304) {
                self.v = (self.v & ~@as(u16, 0x7BE0)) | (self.t & 0x7BE0);
            }
        }

        // Start of vblank
        if (self.scanline == 241 and self.cycle == 1) {
            self.status.vertical_blank = true;
            self.frame_complete = true;
            if (self.ctrl.nmi_enabled) {
                self.nmi_output = true;
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

    fn increment_coarse_x(self: *PPU) void {
        if (self.v & 0x001F == 31) {
            self.v &= ~@as(u16, 0x001F);
            self.v ^= 0x0400;
        } else {
            self.v += 1;
        }
    }

    fn increment_y(self: *PPU) void {
        if (self.v & 0x7000 != 0x7000) {
            self.v += 0x1000;
        } else {
            self.v &= ~@as(u16, 0x7000);
            var coarse_y: u16 = (self.v & 0x03E0) >> 5;
            if (coarse_y == 29) {
                coarse_y = 0;
                self.v ^= 0x0800;
            } else if (coarse_y == 31) {
                coarse_y = 0;
            } else {
                coarse_y += 1;
            }
            self.v = (self.v & ~@as(u16, 0x03E0)) | (coarse_y << 5);
        }
    }

    fn render_pixel(self: *PPU) void {
        const x: u16 = @intCast(self.cycle - 1);
        const y: u16 = @intCast(self.scanline);

        var bg_pixel: u8 = 0;
        var bg_palette: u8 = 0;

        if (self.mask.show_background) {
            const fine_y: u3 = @truncate(self.v >> 12);
            const pixel_col: u3 = @truncate(x + @as(u16, self.fine_x));

            // Determine if we need to look at the next tile due to fine_x offset.
            // v's coarse_x tracks the current tile fetch position (incremented every 8 cycles).
            // When (x % 8) + fine_x >= 8, the pixel falls in the next tile.
            var tile_v = self.v;
            if ((x & 7) + @as(u16, self.fine_x) >= 8) {
                if (tile_v & 0x1F == 31) {
                    tile_v = (tile_v & ~@as(u16, 0x1F)) ^ 0x0400;
                } else {
                    tile_v += 1;
                }
            }

            const coarse_x: u16 = tile_v & 0x1F;
            const coarse_y: u16 = (tile_v >> 5) & 0x1F;
            const nt: u16 = (tile_v >> 10) & 0x03;

            // Get tile index from nametable
            const tile_addr: u16 = 0x2000 | (nt << 10) | (coarse_y << 5) | coarse_x;
            const tile_id: u16 = self.ppu_read(tile_addr);

            // Get pattern table address
            const pattern_base: u16 = @as(u16, self.ctrl.background_pattern_table_address) * 0x1000;
            const pattern_addr = pattern_base + tile_id * 16 + @as(u16, fine_y);

            // Read the two bit planes for this row
            const tile_lsb = self.ppu_read(pattern_addr);
            const tile_msb = self.ppu_read(pattern_addr + 8);

            // Extract the pixel (bit 7 is leftmost pixel)
            const bit_shift: u3 = 7 - pixel_col;
            const pixel_lsb: u8 = (tile_lsb >> bit_shift) & 1;
            const pixel_msb: u8 = (tile_msb >> bit_shift) & 1;
            bg_pixel = (pixel_msb << 1) | pixel_lsb;

            // Get attribute byte for palette selection
            const attr_addr: u16 = 0x2000 | (nt << 10) | 0x3C0 | ((coarse_y >> 2) << 3) | (coarse_x >> 2);
            const attr_byte = self.ppu_read(attr_addr);

            const attr_shift: u3 = @truncate(((coarse_y & 2) << 1) | (coarse_x & 2));
            bg_palette = (attr_byte >> attr_shift) & 0x03;
        }

        // Sprite evaluation
        var sprite_pixel: u8 = 0;
        var sprite_palette: u8 = 0;
        var sprite_priority: bool = false; // false = in front of BG
        var sprite_zero_hit: bool = false;

        if (self.mask.show_sprites and !(x < 8 and !self.mask.show_sprites_left)) {
            const sprite_height: u16 = if (self.ctrl.sprite_size == 1) 16 else 8;

            for (0..64) |i| {
                const sprite_y: u16 = @as(u16, self.oam[i * 4 + 0]) + 1;
                const sprite_x: u16 = self.oam[i * 4 + 3];

                // Check if this sprite is on the current pixel
                if (x < sprite_x or x >= sprite_x + 8) continue;
                if (y < sprite_y or y >= sprite_y + sprite_height) continue;

                const tile_id: u16 = self.oam[i * 4 + 1];
                const attr = self.oam[i * 4 + 2];
                const flip_h = attr & 0x40 != 0;
                const flip_v = attr & 0x80 != 0;

                // Row within the sprite
                var row: u16 = y - sprite_y;
                if (flip_v) row = sprite_height - 1 - row;

                // Pattern table address
                var pattern_addr: u16 = undefined;
                if (self.ctrl.sprite_size == 1) {
                    // 8x16 mode: bit 0 of tile_id selects pattern table
                    const table: u16 = (tile_id & 1) * 0x1000;
                    const base_tile = tile_id & 0xFE;
                    if (row < 8) {
                        pattern_addr = table + base_tile * 16 + row;
                    } else {
                        pattern_addr = table + (base_tile + 1) * 16 + (row - 8);
                    }
                } else {
                    // 8x8 mode
                    const table: u16 = @as(u16, self.ctrl.sprite_pattern_table_addr) * 0x1000;
                    pattern_addr = table + tile_id * 16 + row;
                }

                const tile_lsb = self.ppu_read(pattern_addr);
                const tile_msb = self.ppu_read(pattern_addr + 8);

                // Column within the sprite
                var col: u3 = @truncate(x - sprite_x);
                if (!flip_h) col = 7 - col;

                const pixel_lsb: u8 = (tile_lsb >> col) & 1;
                const pixel_msb: u8 = (tile_msb >> col) & 1;
                const pixel = (pixel_msb << 1) | pixel_lsb;

                if (pixel == 0) continue; // Transparent

                // First non-transparent sprite wins
                sprite_pixel = pixel;
                sprite_palette = attr & 0x03;
                sprite_priority = attr & 0x20 != 0;

                // Sprite 0 hit detection
                if (i == 0 and bg_pixel != 0 and x != 255) {
                    sprite_zero_hit = true;
                }
                break;
            }
        }

        if (sprite_zero_hit) {
            self.status.sprite_0_hit = true;
        }

        // Combine background and sprite
        var color: u32 = undefined;
        if (bg_pixel == 0 and sprite_pixel == 0) {
            color = self.get_color_from_palette_ram(0, 0);
        } else if (bg_pixel == 0) {
            color = self.get_color_from_palette_ram(sprite_palette + 4, sprite_pixel);
        } else if (sprite_pixel == 0 or sprite_priority) {
            color = self.get_color_from_palette_ram(bg_palette, bg_pixel);
        } else {
            color = self.get_color_from_palette_ram(sprite_palette + 4, sprite_pixel);
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
                self.nmi_output = false;
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
                const addr = self.v & 0x3FFF;

                self.data_buffer = self.ppu_read(addr);
                if (addr >= 0x3f00) {
                    d = self.data_buffer;
                }

                self.v +%= self.ctrl.get_vram_increment();
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
                const old_nmi_enabled = self.ctrl.nmi_enabled;
                self.ctrl = @bitCast(v);
                // Update nametable select bits in t
                self.t = (self.t & ~@as(u16, 0x0C00)) | (@as(u16, v & 0x03) << 10);
                // NMI edge case: if NMI just enabled while vblank is set, raise nmi_output
                if (!old_nmi_enabled and self.ctrl.nmi_enabled and self.status.vertical_blank) {
                    self.nmi_output = true;
                }
                // If NMI just disabled, lower nmi_output
                if (old_nmi_enabled and !self.ctrl.nmi_enabled) {
                    self.nmi_output = false;
                }
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
                    // First write: set coarse X and fine X
                    self.t = (self.t & ~@as(u16, 0x001F)) | (@as(u16, v) >> 3);
                    self.fine_x = @truncate(v);
                    self.w = .lsb;
                } else {
                    // Second write: set coarse Y and fine Y
                    self.t = (self.t & ~@as(u16, 0x73E0)) | (@as(u16, v & 0x07) << 12) | (@as(u16, v >> 3) << 5);
                    self.w = .msb;
                }
            },
            // PPU Address
            0x0006 => {
                if (self.w == .msb) {
                    // First write: set high byte of t, clear bit 14
                    self.t = (@as(u16, v & 0x3F) << 8) | (self.t & 0x00FF);
                    self.w = .lsb;
                    if (self.trace_writes) {
                        std.debug.print("$2006.hi=${x:0>2} t=${x:0>4}\n", .{ v, self.t });
                    }
                } else {
                    // Second write: set low byte of t, copy t to v
                    self.t = (self.t & 0xFF00) | @as(u16, v);
                    self.v = self.t;
                    self.w = .msb;
                    if (self.trace_writes) {
                        std.debug.print("$2006.lo=${x:0>2} v=${x:0>4}\n", .{ v, self.v });
                    }
                }
            },
            // PPU Data
            0x0007 => {
                if (self.trace_writes) {
                    std.debug.print("$2007 v=${x:0>4} val=${x:0>2}\n", .{ self.v & 0x3FFF, v });
                }
                self.ppu_write(self.v & 0x3FFF, v);
                self.v +%= self.ctrl.get_vram_increment();
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
            const table, const offset = self.mirror_nametable_addr(k);
            return self.nameTable[table][offset];
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
            k &= 0x0fff;
            const table, const offset = self.mirror_nametable_addr(k);
            self.nameTable[table][offset] = v;
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

    fn mirror_nametable_addr(self: *PPU, k: u16) struct { usize, u16 } {
        const table_index: u2 = @truncate(k >> 10);
        const offset: u16 = k & 0x03FF;
        const physical_table: usize = switch (self.rom.?.get_mirror_mode()) {
            .one_screen_lower => 0,
            .one_screen_upper => 1,
            .vertical => @as(usize, table_index & 1),
            .horizontal => @as(usize, table_index >> 1),
        };
        return .{ physical_table, offset };
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

                        const x_pos = tx * 8 + (7 - col);
                        const y_pos = ty * 8 + row;
                        _ = self.get_color_from_palette_ram(palette, pixel);

                        buf[y_pos * 256 + x_pos + sc_offset] = nesPalette[pixel];
                    }
                }
            }
        }
    }
};

const nesPalette = [_]u32{ 0x757575, 0x271B8F, 0x0000AB, 0x47009F, 0x8F0077, 0xAB0013, 0xA70000, 0x7F0B00, 0x432F00, 0x004700, 0x005100, 0x003F17, 0x1B3F5F, 0x000000, 0x000000, 0x000000, 0xBCBCBC, 0x0073EF, 0x233BEF, 0x8300F3, 0xBF00BF, 0xE7005B, 0xDB2B00, 0xCB4F0F, 0x8B7300, 0x009700, 0x00AB00, 0x00933B, 0x00838B, 0x000000, 0x000000, 0x000000, 0xFFFFFF, 0x3FBFFF, 0x5F97FF, 0xA78BFD, 0xF77BFF, 0xFF77B7, 0xFF7763, 0xFF9B3B, 0xF3BF3F, 0x83D313, 0x4FDF4B, 0x58F898, 0x00EBDB, 0x000000, 0x000000, 0x000000, 0xFFFFFF, 0xABE7FF, 0xC7D7FF, 0xD7CBFF, 0xFFC7FF, 0xFFC7DB, 0xFFBFB3, 0xFFDBAB, 0xFFE7A3, 0xE3FFA3, 0xABF3BF, 0xB3FFCF, 0x9FFFF3, 0x000000, 0x000000, 0x000000 };
