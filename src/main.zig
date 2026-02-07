const std = @import("std");
const c = @cImport({
    @cDefine("FENSTER_HEADER", {});
    @cInclude("fenster.h");
});
const Bus = @import("bus.zig").Bus;
const rom = @import("rom.zig");
const cpu_debug = @import("debug.zig");

const SCALE: comptime_int = 2;
const WIDTH: comptime_int = 256;
const HEIGHT: comptime_int = 240;

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var max_rom_buffer: [rom.MAX_SIZE]u8 = undefined;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len > 2 and std.mem.indexOf(u8, args[1], "disasm") != null) {
        const romName: []const u8 = args[2];
        const rom_buffer = try std.fs.cwd().readFile(romName, &max_rom_buffer);
        var loaded_rom = try rom.Rom.load(rom_buffer);
        try cpu_debug.disassemble(&loaded_rom, std.io.getStdOut().writer());
        return;
    }

    // Headless screenshot mode: run N frames and dump framebuffer.ppm
    if (args.len > 2 and std.mem.eql(u8, args[1], "screenshot")) {
        const romName: []const u8 = args[2];
        const num_frames: u32 = if (args.len > 3) std.fmt.parseInt(u32, args[3], 10) catch 120 else 120;
        const rom_buffer2 = try std.fs.cwd().readFile(romName, &max_rom_buffer);
        var loaded_rom2 = try rom.Rom.load(rom_buffer2);
        var nes2: Bus = Bus.init();
        nes2.cpu.bus = &nes2;
        nes2.load_rom(&loaded_rom2);
        nes2.reset();

        var frames: u32 = 0;
        while (frames < num_frames) {
            nes2.clock();
            if (nes2.ppu.frame_complete) {
                frames += 1;
                while (nes2.ppu.frame_complete) {
                    nes2.clock();
                }
            }
        }

        // Write PPM file
        const file = try std.fs.cwd().createFile("framebuffer.ppm", .{});
        defer file.close();
        var writer = file.writer();
        try writer.print("P3\n256 240\n255\n", .{});
        for (0..240) |y| {
            for (0..256) |x| {
                const color = nes2.ppu.frame_buffer[y * 256 + x];
                const r = (color >> 16) & 0xFF;
                const g = (color >> 8) & 0xFF;
                const b = color & 0xFF;
                try writer.print("{d} {d} {d}\n", .{ r, g, b });
            }
        }
        std.debug.print("Screenshot saved to framebuffer.ppm ({d} frames)\n", .{frames});
        return;
    }

    const romName: []const u8 = if (args.len > 1) args[1] else "roms/nestest.nes";

    std.debug.print("Loaded {s}\n", .{romName});

    const rom_buffer = try std.fs.cwd().readFile(romName, &max_rom_buffer);

    const is_functional_test_rom = std.mem.indexOf(u8, romName, "6502_functional_test") != null;

    var loaded_rom = if (is_functional_test_rom)
        try rom.Rom.load_unchecked(rom_buffer)
    else
        try rom.Rom.load(rom_buffer);

    var nes: Bus = Bus.init();
    nes.cpu.bus = &nes;

    nes.load_rom(&loaded_rom);

    nes.reset();

    if (is_functional_test_rom) {
        nes.cpu.pc = 0x400;
    }

    // nes.cpu.debug = std.io.getStdOut().writer();

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

    var t: u32 = 0;
    var now: i64 = c.fenster_time();
    while (c.fenster_loop(&f) == 0) {
        // Exit when Escape is pressed
        if (f.keys[27] != 0) {
            break;
        }

        // Update controller input
        // NES controller bits: A B Select Start Up Down Left Right
        var ctrl: u8 = 0;
        if (f.keys['Z'] != 0 or f.keys['z'] != 0) ctrl |= 0x80; // A
        if (f.keys['X'] != 0 or f.keys['x'] != 0) ctrl |= 0x40; // B
        if (f.keys[16] != 0) ctrl |= 0x20; // Select = Right Shift (fenster key 16)
        if (f.keys[10] != 0) ctrl |= 0x10; // Start = Enter (fenster key 10)
        if (f.keys[17] != 0) ctrl |= 0x08; // Up arrow
        if (f.keys[18] != 0) ctrl |= 0x04; // Down arrow
        if (f.keys[20] != 0) ctrl |= 0x02; // Left arrow
        if (f.keys[19] != 0) ctrl |= 0x01; // Right arrow
        nes.controllers[0] = ctrl;

        // Run until frame is complete
        while (!nes.ppu.frame_complete) {
            nes.clock();
        }

        // Copy frame buffer to game buffer
        @memcpy(&game_buffer, &nes.ppu.frame_buffer);

        // Continue running until next frame starts
        while (nes.ppu.frame_complete) {
            nes.clock();
        }

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
