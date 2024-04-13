const AddrMode = @import("addr.zig").AddrMode;
const Mem = @import("mem.zig").Mem;
const std = @import("std");

const Flags = packed struct(u8) {
  negative: bool = false,
  overflow: bool = false,
  _padding: u1 = 0,
  break_command: bool = false,
  decimal_mode: bool = false,
  interrupt_disable: bool = false,
  zero: bool = false,
  carry: bool = false,
};

pub const CPU = struct {
  pc: u16,

  /// Offset from $0100
  sp: u8,
  a: u8,
  x: u8,
  y: u8,

  /// N, V, _, B, D, I, Z, C
  p: Flags,

  cycles: u32,

  pub fn init() CPU {
    return .{ .p = Flags {}, .a = 0, .x = 0, .y = 0, .sp = 0xff, .pc = 0, .cycles = 0 };
  }


  pub fn jmp(self: *CPU, dst: AddrMode) void {
    self.pc = switch (dst) {
      .absolute => |v| v,
      else => unreachable,
    };
  }

  fn ldx(self: *CPU, dst: AddrMode) void {
    self.x = switch (dst) {
      .immediate => |v| v,
      else => unreachable,
    };

    self.cycles += switch (dst) {
      .immediate => 2,
      else => unreachable,
    };

    self.p.zero = self.x == 0;
    self.p.negative = @clz(self.x) == 0;
  }

  fn lda(self: *CPU, dst: AddrMode) void {
    self.a = switch (dst) {
      .immediate => |v| v,
      else => unreachable,
    };

    self.cycles += switch (dst) {
      .immediate => 2,
      else => unreachable,
    };

    self.p.zero = self.a == 0;
    self.p.negative = @clz(self.a) == 0;
  }

  fn sta(self: *CPU, mem: *Mem, dst: AddrMode) void {
    const addr = switch (dst) {
      .absolute => |v| v,
      else => unreachable,
    };

    mem[addr] = self.a;

    self.cycles += switch (dst) {
      .absolute => 4,
      else => unreachable,
    };

    // No flags affected
  }

  fn txs(self: *CPU) void {
    self.sp = self.x;
    self.cycles += 2;
    // No flags affected
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
      0x9a => {
        // TXS - Transfer X to Stack Pointer
        self.txs();
      },
      0x8d => {
        // STA - Store Accumulator
        self.sta(mem, .{ .absolute = mem[self.pc + 1]});
        self.pc += 1;
      },
      0xa9 => {
        // LDA - Load Accumulator
        self.lda(.{ .immediate = mem[self.pc + 1] });
        self.pc += 1;
      },
      0xa2 => {
        // LDX - Load X Register
        self.ldx(.{ .immediate = mem[self.pc + 1] });
        self.pc += 1;
      },
      0xd8 => {
        // CLD - Clear Decimal Mode
        self.p.decimal_mode = false;
        self.cycles += 2;
      },
      else => {
        std.debug.print("Unreachable: {x}\n", .{ mem[self.pc] });
        unreachable;
      }
    }
    self.pc += 1;
  }
};



