const AddrMode = @import("addr.zig").AddrMode;
const Mem = @import("mem.zig").Mem;
const std = @import("std");


pub const CPU = struct {
  pc: u16,

  /// Offset from $0100
  sp: u8,
  a: u8,
  x: u8,
  y: u8,

  /// N, V, _, B, D, I, Z, C
  p: u8,

  pub fn init() CPU {
    return .{ .p = 0, .a = 0, .x = 0, .y = 0, .sp = 0xff, .pc = 0 };
  }


  pub fn jmp(self: *CPU, dst: AddrMode) void {
    self.pc = switch (dst) {
      .absolute => |v| v,
      else => unreachable,
    };
  }

  pub fn run(self: *CPU, mem: *Mem) void {
    var pc: u16 = 0;
    while (self.pc != pc) {
      pc = self.pc;
      self.step(mem);
      std.debug.print("loop {x}\n", .{ pc });
    }
  }

  fn step(self: *CPU, mem: *Mem) void {
    switch (mem[self.pc]) {
      // 0x29 => {
      //     self.a = self.a & mem[self.pc + 1];
      //     self.pc += 1;
      // },
      0xa2 => {

      },
      0xd8 => {
        // CLD - Clear Decimal Mode

      },
      else => {
        std.debug.print("Unreachable: {x}\n", .{ mem[self.pc] });
        unreachable;
      }
    }
    self.pc += 1;
  }
};
