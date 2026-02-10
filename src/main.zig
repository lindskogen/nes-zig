const std = @import("std");
const c = @cImport({
    @cDefine("FENSTER_HEADER", {});
    @cInclude("fenster.h");
});
// miniaudio uses self-referential struct types that Zig's cImport can't translate,
// so we use an opaque wrapper compiled from C instead.
const ma = struct {
    const Device = opaque {};
    const DeviceConfig = extern struct { data: [512]u8 }; // opaque blob, configured from C
    const AudioCallback = *const fn (?*anyopaque, ?*anyopaque, ?*const anyopaque, u32) callconv(.c) void;

    extern "c" fn zig_ma_device_config_playback(sample_rate: u32, callback: AudioCallback, user_data: ?*anyopaque) DeviceConfig;
    extern "c" fn zig_ma_device_init(config: *const DeviceConfig) ?*Device;
    extern "c" fn zig_ma_device_start(device: *Device) void;
    extern "c" fn zig_ma_device_uninit(device: *Device) void;
    extern "c" fn zig_ma_device_get_sample_rate(device: *Device) u32;
};
const Bus = @import("bus.zig").Bus;
const APU = @import("apu.zig").APU;
const rom = @import("rom.zig");
const cpu_debug = @import("debug.zig");

const SCALE: comptime_int = 2;
const WIDTH: comptime_int = 256;
const HEIGHT: comptime_int = 240;
const SAMPLE_RATE: u32 = 44100;

fn audioCallback(_: ?*anyopaque, output: ?*anyopaque, _: ?*const anyopaque, frame_count: u32) callconv(.c) void {
    const apu: *APU = @ptrCast(@alignCast(audio_apu_ptr));
    const out: [*]f32 = @ptrCast(@alignCast(output.?));
    for (0..frame_count) |i| {
        out[i] = apu.ring_buffer.pop() orelse 0.0;
    }
}

var audio_apu_ptr: ?*anyopaque = null;

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
                nes2.ppu.trace_writes = false;
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

        // Dump blargg test result from PRG RAM ($6000+)
        const status = nes2.prg_ram[0]; // $6000
        std.debug.print("Test status: 0x{x:0>2}\n", .{status});
        // Print text output at $6004+
        var i: usize = 4;
        while (i < 2048 and nes2.prg_ram[i] != 0) : (i += 1) {
            std.debug.print("{c}", .{nes2.prg_ram[i]});
        }
        std.debug.print("\n", .{});

        return;
    }

    // Headless WAV capture mode: run N frames and dump audio to output.wav
    if (args.len > 2 and std.mem.eql(u8, args[1], "wav")) {
        const romName2: []const u8 = args[2];
        const num_frames: u32 = if (args.len > 3) std.fmt.parseInt(u32, args[3], 10) catch 300 else 300;
        const rom_buffer3 = try std.fs.cwd().readFile(romName2, &max_rom_buffer);
        var loaded_rom3 = try rom.Rom.load(rom_buffer3);
        var nes3: Bus = Bus.init();
        nes3.cpu.bus = &nes3;
        nes3.load_rom(&loaded_rom3);
        nes3.reset();

        // Pre-allocate sample buffer: ~44100 samples/sec * num_frames/60 sec
        const estimated_samples = @as(usize, SAMPLE_RATE) * num_frames / 60 + 44100;
        var samples = try std.ArrayList(f32).initCapacity(allocator, estimated_samples);
        defer samples.deinit();

        var frames: u32 = 0;
        while (frames < num_frames) {
            nes3.clock();
            // Drain ring buffer each cycle
            while (nes3.apu.ring_buffer.pop()) |s| {
                samples.append(s) catch break;
            }
            if (nes3.ppu.frame_complete) {
                frames += 1;
                while (nes3.ppu.frame_complete) {
                    nes3.clock();
                    while (nes3.apu.ring_buffer.pop()) |s| {
                        samples.append(s) catch break;
                    }
                }
            }
        }

        // Write WAV file
        const wav_file = try std.fs.cwd().createFile("output.wav", .{});
        defer wav_file.close();
        var w = wav_file.writer();

        const num_samples: u32 = @intCast(samples.items.len);
        const data_size: u32 = num_samples * 2; // 16-bit samples
        const file_size: u32 = 36 + data_size;

        // RIFF header
        try w.writeAll("RIFF");
        try w.writeInt(u32, file_size, .little);
        try w.writeAll("WAVE");

        // fmt chunk
        try w.writeAll("fmt ");
        try w.writeInt(u32, 16, .little); // chunk size
        try w.writeInt(u16, 1, .little); // PCM format
        try w.writeInt(u16, 1, .little); // mono
        try w.writeInt(u32, SAMPLE_RATE, .little);
        try w.writeInt(u32, SAMPLE_RATE * 2, .little); // byte rate (sampleRate * channels * bitsPerSample/8)
        try w.writeInt(u16, 2, .little); // block align
        try w.writeInt(u16, 16, .little); // bits per sample

        // data chunk
        try w.writeAll("data");
        try w.writeInt(u32, data_size, .little);

        for (samples.items) |sample| {
            const clamped = std.math.clamp(sample, -1.0, 1.0);
            const int_sample: i16 = @intFromFloat(clamped * 32767.0);
            try w.writeInt(i16, int_sample, .little);
        }

        std.debug.print("WAV saved to output.wav ({d} frames, {d} samples, {d:.1}s)\n", .{
            frames,
            num_samples,
            @as(f64, @floatFromInt(num_samples)) / @as(f64, SAMPLE_RATE),
        });
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
    } else if (std.mem.indexOf(u8, romName, "nestest.nes") != null) {
        nes.cpu.pc = 0xc000;
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

    // Init audio — request 44100Hz but use whatever the device actually gives us
    audio_apu_ptr = @ptrCast(&nes.apu);
    const audio_config = ma.zig_ma_device_config_playback(SAMPLE_RATE, audioCallback, null);
    const audio_device = ma.zig_ma_device_init(&audio_config);
    if (audio_device) |dev| {
        const actual_rate = ma.zig_ma_device_get_sample_rate(dev);
        if (actual_rate != SAMPLE_RATE) {
            std.debug.print("Audio: requested {d}Hz, device using {d}Hz\n", .{ SAMPLE_RATE, actual_rate });
        }
        nes.apu.set_sample_rate(actual_rate);
        ma.zig_ma_device_start(dev);
    }
    defer {
        if (audio_device) |dev| ma.zig_ma_device_uninit(dev);
    }

    var t: u32 = 0;
    var fps_frame_count: u32 = 0;
    var fps_timer: i64 = c.fenster_time();
    var title_buf: [64]u8 = undefined;
    // NTSC NES: 60.0988 fps → 16.639 ms per frame
    var next_frame: f64 = @floatFromInt(c.fenster_time());
    const frame_duration: f64 = 1000.0 / 60.0988;
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
        fps_frame_count += 1;
        if (fps_frame_count >= 60) {
            const elapsed = c.fenster_time() - fps_timer;
            if (elapsed > 0) {
                // NES NTSC is 60.0988 fps; 60 frames should take ~998.4ms
                const speed: u32 = @intFromFloat(998.4 / @as(f64, @floatFromInt(elapsed)) * 100.0);
                const title_slice = std.fmt.bufPrint(&title_buf, "zig-nes ({d}%)\x00", .{speed}) catch "zig-nes\x00";
                c.fenster_retitle(&f, title_slice.ptr);
            }
            fps_frame_count = 0;
            fps_timer = c.fenster_time();
        }
        // Sleep to match NTSC frame rate (60.0988 fps)
        next_frame += frame_duration;
        const now: i64 = c.fenster_time();
        const sleep_ms: i64 = @as(i64, @intFromFloat(next_frame)) - now;
        if (sleep_ms > 0) {
            c.fenster_sleep(sleep_ms);
        } else if (sleep_ms < -100) {
            // Fallen far behind (e.g. window drag), reset to avoid catch-up burst
            next_frame = @floatFromInt(now);
        }
    }
}
