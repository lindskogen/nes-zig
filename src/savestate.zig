const std = @import("std");
const Bus = @import("bus.zig").Bus;
const Rom = @import("rom.zig").Rom;

const MAGIC = "NSSV".*;
const VERSION: u32 = 1;

pub fn save(bus: *const Bus, rom_ptr: *const Rom, path: []const u8) !void {
    const file = try std.fs.cwd().createFile(path, .{});
    defer file.close();
    const writer = file.writer();

    try writer.writeAll(&MAGIC);
    try writer.writeInt(u32, VERSION, .little);
    try writer.writeAll(std.mem.asBytes(bus));
    try writer.writeAll(std.mem.asBytes(&rom_ptr.mapper_state));
    try writer.writeAll(&rom_ptr.chr_ram);
}

pub fn load(bus: *Bus, rom_ptr: *Rom, path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const reader = file.reader();

    var magic: [4]u8 = undefined;
    const n1 = try reader.readAll(&magic);
    if (n1 != 4 or !std.mem.eql(u8, &magic, &MAGIC)) return error.InvalidSaveState;

    const version = try reader.readInt(u32, .little);
    if (version != VERSION) return error.IncompatibleVersion;

    // Ensure pointers are valid even if load fails partway through
    defer {
        bus.cpu.bus = bus;
        bus.cpu.debug = null;
        bus.rom = rom_ptr;
        bus.ppu.rom = rom_ptr;
    }

    const n2 = try reader.readAll(std.mem.asBytes(bus));
    if (n2 != @sizeOf(Bus)) return error.InvalidSaveState;

    // Reset audio output state (ring buffer, mixer accumulators)
    bus.apu.ring_buffer = .{};
    bus.apu.pulse_acc = 0;
    bus.apu.tnd_acc = 0;
    bus.apu.sample_count = 0;
    bus.apu.sample_counter = 0;
    bus.apu.samples_produced = 0;

    const mapper_bytes = std.mem.asBytes(&rom_ptr.mapper_state);
    const n3 = try reader.readAll(mapper_bytes);
    if (n3 != mapper_bytes.len) return error.InvalidSaveState;

    const n4 = try reader.readAll(&rom_ptr.chr_ram);
    if (n4 != rom_ptr.chr_ram.len) return error.InvalidSaveState;
}
