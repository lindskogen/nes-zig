const std = @import("std");
const c = @cImport({
  @cDefine("FENSTER_HEADER", {});
  @cInclude("fenster.h");
  });
const CPU = @import("cpu.zig").CPU;
const Mem = @import("mem.zig").Mem;
const rom = @import("rom.zig");

const SCALE: comptime_int = 2;
const WIDTH: comptime_int = 320;
const HEIGHT: comptime_int = 240;

pub fn main() !void {
  var cpu = CPU.init();
  var max_rom_buffer: [rom.MAX_SIZE]u8 = undefined;

  const rom_buffer = try std.fs.cwd().readFile("src/roms/sample1.nes", &max_rom_buffer);

  const loaded_rom = try rom.Rom.load(rom_buffer);

  var mem: Mem = Mem.init(&loaded_rom);

  const reset_vector: u16 = (@as(u16, mem.read(0xfffd)) << 8) | @as(u16, mem.read(0xfffc));
  cpu.pc = reset_vector;

  std.debug.assert(reset_vector == 0x8000);

  cpu.run(&mem);

  var buf: [WIDTH * HEIGHT]u32 = undefined;

  var f = std.mem.zeroInit(c.fenster, .{
    .width = WIDTH,
    .height = HEIGHT,
    .title = "hello",
    .buf = &buf[0],
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

    mem.ppu.write_to_buffer(&buf, WIDTH, HEIGHT);

    // Render x^y^t pattern
    for (buf, 0..) |_, i| {
      buf[i] = @as(u32, @intCast(i % WIDTH)) ^ @as(u32, @intCast(i / HEIGHT)) ^ t;
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
