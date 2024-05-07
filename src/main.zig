const std = @import("std");
const c = @cImport({
    @cDefine("FENSTER_HEADER", {});
    @cInclude("fenster.h");
});
const Bus = @import("bus.zig").Bus;
const rom = @import("rom.zig");

const SCALE: comptime_int = 2;
const WIDTH: comptime_int = 256;
const HEIGHT: comptime_int = 240;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var max_rom_buffer: [rom.MAX_SIZE]u8 = undefined;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const romName: []const u8 = if (args.len > 1) args[1] else "src/roms/nestest.nes";

    const rom_buffer = try std.fs.cwd().readFile(romName, &max_rom_buffer);

    const loaded_rom = try rom.Rom.load(rom_buffer);

    var nes: Bus = Bus.init();
    nes.cpu.bus = &nes;

    nes.load_rom(&loaded_rom);

    nes.reset();

    nes.cpu.debug = std.io.getStdOut().writer();

    var game_buffer: [WIDTH * HEIGHT]u32 = undefined;
    var screen_buffer: [WIDTH * SCALE * HEIGHT * SCALE]u32 = undefined;

    var f = std.mem.zeroInit(c.fenster, .{
        .width = WIDTH * SCALE,
        .height = HEIGHT * SCALE,
        .title = "zig-nes",
        .buf = &screen_buffer[0],
    });

    _ = c.fenster_open(&f);
    defer c.fenster_close(&f);

    var palette: u8 = 0x00;

    var t: u32 = 0;
    var now: i64 = c.fenster_time();
    while (c.fenster_loop(&f) == 0) {
        // Exit when Escape is pressed
        if (f.keys[27] != 0) {
            break;
        }

        if (f.keys[80] != 0) {
            palette += 1;
            palette &= 0x07;
        }

        nes.clock();
        while (!nes.cpu.complete()) {
            nes.clock();
        }

        nes.clock();
        while (nes.cpu.complete()) {
            nes.clock();
        }

        nes.ppu.get_pattern_table(0, palette, &game_buffer, 0);
        nes.ppu.get_pattern_table(1, palette, &game_buffer, 128);

        for (0..HEIGHT) |y| {
            for (0..WIDTH) |x| {
                inline for (0..2) |dx| {
                    inline for (0..2) |dy| {
                        screen_buffer[((y * SCALE) + dy) * WIDTH * SCALE + (x * SCALE) + dx] = game_buffer[y * WIDTH + x];
                    }
                }
            }
        }

        t +%= 1;
        // Keep ~60 FPS
        const diff: i64 = 1000 / 60 - (c.fenster_time() - now);
        if (diff > 0) {
            c.fenster_sleep(diff);
        }
        now = c.fenster_time();
    }
}
