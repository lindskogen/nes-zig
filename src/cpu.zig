const AddrMode = @import("addr.zig").AddrMode;
const Mem = @import("mem.zig").Mem;
const std = @import("std");

inline fn is_negative(v: u8) bool {
  return @clz(v) == 0;
}

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

  fn set_nz_flags(self: *CPU, v: u8) void {
    self.p.zero = v == 0;
    self.p.negative = is_negative(v);
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

    self.set_nz_flags(self.x);
  }

  fn ldy(self: *CPU, dst: AddrMode) void {
    self.y = switch (dst) {
      .immediate => |v| v,
      else => unreachable,
    };

    self.cycles += switch (dst) {
      .immediate => 2,
      else => unreachable,
    };

    self.set_nz_flags(self.y);
  }

  fn cmp(self: *CPU, mem: *Mem, dst: AddrMode) void {
    const m = switch (dst) {
      .immediate => |v| v,
      .absolute => |v| mem[v],
      else => unreachable,
    };

    self.cycles += switch (dst) {
      .immediate => 2,
      .absolute => 4,
      else => unreachable,
    };

    self.p.zero = self.a == m;
    self.p.carry = self.a >= m;
    self.p.negative = is_negative(self.a - m);
  }

  fn lda(self: *CPU, mem: *Mem, dst: AddrMode) void {
    self.a = switch (dst) {
      .immediate => |v| v,
      .absolute => |v| mem[v],
      else => unreachable,
    };

    self.cycles += switch (dst) {
      .immediate => 2,
      .absolute => 4,
      else => unreachable,
    };

    self.set_nz_flags(self.a);
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

  fn bne(self: *CPU, dst: AddrMode) void {
    const addr = switch (dst) {
      .relative => |v| v,
      else => unreachable,
    };

    if (!self.p.zero) {
      if (addr >= 0) {
        self.pc += @abs(addr);
      } else {
        self.pc -= @abs(addr);
      }
      self.cycles += 1; // TODO: +2 if to a new page
    }

    self.cycles += switch (dst) {
      .relative => 2,
      else => unreachable,
    };

    // No flags affected
  }

  fn bpl(self: *CPU, dst: AddrMode) void {
    const addr = switch (dst) {
      .relative => |v| v,
      else => unreachable,
    };

    if (!self.p.negative) {
      if (addr >= 0) {
        self.pc += @abs(addr);
      } else {
        self.pc -= @abs(addr);
      }
      self.cycles += 1; // TODO: +2 if to a new page
    }

    self.cycles += switch (dst) {
      .relative => 2,
      else => unreachable,
    };

    // No flags affected
  }

  fn txs(self: *CPU) void {
    self.sp = self.x;
    self.cycles += 2;
    // No flags affected
  }

  fn dey(self: *CPU) void {
    self.y -%= 1;
    self.cycles += 2;

    self.set_nz_flags(self.y);
  }

  fn tya(self: *CPU) void {
    self.a = self.y;
    self.cycles += 2;

    self.set_nz_flags(self.a);
  }

  fn tax(self: *CPU) void {
    self.x = self.a;
    self.cycles += 2;

    self.set_nz_flags(self.x);
  }

  pub fn run(self: *CPU, mem: *Mem) void {
    var pc: u16 = 0;
    while (self.pc != pc) {
      pc = self.pc;
      self.step(mem);
      std.debug.print("loop {x}\n", .{ pc });
    }
  }

  inline fn operand(self: *CPU, mem: *Mem, comptime addrType: anytype) AddrMode {
    switch (addrType) {
      .absolute => {
        const lsb: u16 = mem[self.pc];
        const msb: u16 = mem[self.pc + 1];
        self.pc += 2;

        return .{ .absolute = (msb << 8) & lsb };
      },
      .immediate => {
        const b: u8 = mem[self.pc];
        self.pc += 1;
        return .{ .immediate = b };
      },
      .relative => {
        const b: i8 = @as(i8, @bitCast(mem[self.pc]));
        self.pc += 1;
        return .{ .relative = b };
      },
      else => {
        @compileLog("No implementation for {s}", .{addrType});
        unreachable;
      }
    }
  }

  fn step(self: *CPU, mem: *Mem) void {
    const instr = mem[self.pc];
    self.pc += 1;
    switch (instr) {
      // TYA - Transfer Y to Accumulator
      0x98 => self.tya(),
      // TXS - Transfer X to Stack Pointer
      0x9a => self.txs(),
      // TAX - Transfer Accumulator to X
      0xaa => self.tax(),
      // BPL - Branch if Positive
      0x10 => self.bpl(self.operand(mem, .relative)),
      // BNE - Branch if Not Equal
      0xd0 => self.bne(self.operand(mem, .relative)),
      // STA - Store Accumulator
      0x8d => self.sta(mem, self.operand(mem, .absolute)),
      // CMP - Compare
      0xc9 => self.cmp(mem, self.operand(mem, .immediate)),
      // LDA - Load Accumulator
      0xa9 => self.lda(mem, self.operand(mem, .immediate)),
      0xad => self.lda(mem, self.operand(mem, .absolute)),
      // LDX - Load X Register
      0xa2 => self.ldx(self.operand(mem, .immediate)),
      // LDY - Load Y Register
      0xa0 => self.ldy(self.operand(mem, .immediate)),
      // DEY - Decrement Y Register
      0x88 => self.dey(),
      0x18 => {
        // CLC - Clear Carry Flag
        self.p.carry = false;
        self.cycles += 2;
      },
      0xd8 => {
        // CLD - Clear Decimal Mode
        self.p.decimal_mode = false;
        self.cycles += 2;
      },
      else => {
        std.debug.print("Op code not implemented: 0x{x:0>2}\n", .{ instr });
        unreachable;
      }
    }
  }
};



