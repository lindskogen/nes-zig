const std = @import("std");
const c = @cImport({
    @cDefine("FENSTER_HEADER", {});
    @cInclude("fenster.h");
});


const SCALE: comptime_int = 2;
const WIDTH: comptime_int = 320;
const HEIGHT: comptime_int = 240;

const Mem = [65_536]u8;

const CPU = struct {
    pc: u16,

    /// Offset from $0100
    sp: u8,
    a: u8,
    x: u8,
    y: u8,

    /// N, V, _, B, D, I, Z, C
    p: u8,


    fn jmp(self: *CPU, dst: u16) void {
        self.pc = dst;
    }

    fn run(self: *CPU, mem: *Mem) void {
        switch (mem[self.pc]) {
            // 0x29 => {
            //     self.a = self.a & mem[self.pc + 1];
            //     self.pc += 1;
            // },
            0xd8 => {

            },
            else => {
                std.debug.print("Unreachable: {x}\n", .{ mem[self.pc] });
                unreachable;
            }
        }
        self.pc += 1;
    }
};

pub fn main() !void {
    var cpu = CPU { .p = 0, .a = 0, .x = 0, .y = 0, .sp = 0xff, .pc = 0 };
    var mem: Mem = undefined;

    const text = @embedFile("6502_functional_test.bin");

    std.mem.copyForwards(u8, &mem, text);

    cpu.jmp(0x0400);
    var pc: u16 = 0;
    while (cpu.pc != pc) {
        pc = cpu.pc;
        cpu.run(&mem);
        std.debug.print("loop {x}\n", .{ pc });
    }

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
