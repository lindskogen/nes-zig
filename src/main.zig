const std = @import("std");
const c = @cImport({
  @cDefine("FENSTER_HEADER", {});
  @cInclude("fenster.h");
  });
const CPU = @import("cpu.zig").CPU;
const Mem = @import("mem.zig").Mem;


const SCALE: comptime_int = 2;
const WIDTH: comptime_int = 320;
const HEIGHT: comptime_int = 240;



pub fn main() !void {
  var cpu = CPU.init();
  var mem: Mem = undefined;

  const text = @embedFile("6502_functional_test.bin");

  std.mem.copyForwards(u8, &mem, text);

  cpu.jmp(.{ .absolute = 0x0400 });

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
