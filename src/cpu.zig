const debug_op_code = @import("debug.zig").debug_op_code;
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

  pub fn run(self: *CPU, mem: *Mem) void {
    var pc: u16 = 0;
    while (self.pc != pc) {
      pc = self.pc;
      self.step(mem);
    }
  }

  fn set_nz_flags(self: *CPU, v: u8) void {
    self.p.zero = v == 0;
    self.p.negative = is_negative(v);
  }

  fn set_cnz_flags(self: *CPU, v: u8, carry: bool) void {
    self.set_nz_flags(v);
    self.p.carry = carry;
  }

  fn jmp(self: *CPU, dst: AddrMode) void {
    self.pc = switch (dst) {
      .absolute => |v| v,
      else => unreachable,
    };

    self.cycles += switch (dst) {
      .absolute => 3,
      else => unreachable,
    };
  }

  fn jsr(self: *CPU, mem: *Mem, dst: AddrMode) void {
    self.push16(mem, self.pc - 1);

    self.pc = switch (dst) {
      .absolute => |v| v,
      else => unreachable,
    };

    self.cycles += 6;
    // No flags affected
  }

  fn rts(self: *CPU, mem: *Mem) void {
    self.pc = self.pop16(mem);

    self.cycles += 6;
    // No flags affected
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

  fn adc(self: *CPU, mem: *Mem, dst: AddrMode) void {
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

    const r = @addWithOverflow(self.a, m);
    self.a = r[0];
    self.set_cnz_flags(self.a, r[1] == 1);
  }

  fn cmp(self: *CPU, mem: *Mem, dst: AddrMode) void {
    const m = switch (dst) {
      .immediate => |v| v,
      .absolute => |v| mem[v],
      else => unreachable,
    };

    // std.debug.print("{b:0>8} {b:0>8}\n", .{self.a, m});
    // std.debug.print("{x} {x}\n", .{self.a, m});

    self.cycles += switch (dst) {
      .immediate => 2,
      .absolute => 4,
      else => unreachable,
    };

    self.set_cnz_flags(self.a -% m, self.a >= m);
  }

  fn cpx(self: *CPU, mem: *Mem, dst: AddrMode) void {
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

    self.set_cnz_flags(self.x -% m, self.x >= m);
  }

  fn cpy(self: *CPU, mem: *Mem, dst: AddrMode) void {
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

    self.set_cnz_flags(self.y -% m, self.y >= m);
  }

  fn xor(self: *CPU, mem: *Mem, dst: AddrMode) void {
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

    self.a ^= m;

    self.set_nz_flags(self.a);
  }

  fn lda(self: *CPU, mem: *Mem, dst: AddrMode) void {
    self.a = switch (dst) {
      .immediate => |v| v,
      .absolute,
      .indexedAbsoluteX,
      .indexedAbsoluteY => |v| mem[v],
      .zeroPage => |v| mem[v],
      else => unreachable,
    };

    self.cycles += switch (dst) {
      .immediate => 2,
      .absolute,
      .indexedAbsoluteX,
      .indexedAbsoluteY => 4,
      .zeroPage => 3,
      else => unreachable,
    };

    self.set_nz_flags(self.a);
  }

  fn sta(self: *CPU, mem: *Mem, dst: AddrMode) void {
    const addr = switch (dst) {
      .absolute => |v| v,
      .zeroPage => |v| v,
      else => unreachable,
    };

    mem[addr] = self.a;

    self.cycles += switch (dst) {
      .absolute => 4,
      .zeroPage => 3,
      else => unreachable,
    };

    // No flags affected
  }

  inline fn branch(self: *CPU, dst: anytype, cond: bool) void {
    const addr = switch (dst) {
      .relative => |v| v,
      else => unreachable,
    };

    if (cond) {
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

  fn bne(self: *CPU, dst: AddrMode) void {
    self.branch(dst, !self.p.zero);
  }

  fn beq(self: *CPU, dst: AddrMode) void {
    self.branch(dst, self.p.zero);
  }

  fn bpl(self: *CPU, dst: AddrMode) void {
    self.branch(dst, !self.p.negative);
  }

  fn bmi(self: *CPU, dst: AddrMode) void {
    self.branch(dst, self.p.negative);
  }

  fn bcc(self: *CPU, dst: AddrMode) void {
    self.branch(dst, !self.p.carry);
  }

  fn bcs(self: *CPU, dst: AddrMode) void {
    self.branch(dst, self.p.carry);
  }

  fn bvc(self: *CPU, dst: AddrMode) void {
    self.branch(dst, !self.p.overflow);
  }

  fn bvs(self: *CPU, dst: AddrMode) void {
    self.branch(dst, self.p.overflow);
  }

  fn iny(self: *CPU) void {
    self.y +%= 1;
    self.cycles += 2;

    self.set_nz_flags(self.y);
  }

  fn inx(self: *CPU) void {
    self.x +%= 1;
    self.cycles += 2;

    self.set_nz_flags(self.x);
  }

  fn dey(self: *CPU) void {
    self.y -%= 1;
    self.cycles += 2;

    self.set_nz_flags(self.y);
  }

  fn dex(self: *CPU) void {
    self.x -%= 1;
    self.cycles += 2;

    self.set_nz_flags(self.x);
  }

  fn lsr(self: *CPU, mem: *Mem, dst: AddrMode) void {
    const m = switch (dst) {
      .accumulator => self.a,
      .absolute => |v| mem[v],
      else => unreachable,
    };

    const bit0 = 0b1 & m;
    const res = m / 2;

    switch (dst) {
      .accumulator => {
        self.a = res;
      },
      .absolute => |v| {
        mem[v] = res;
      },
      else => unreachable,
    }

    self.cycles += switch (dst) {
      .accumulator => 2,
      .absolute => 6,
      else => unreachable,
    };

    self.set_cnz_flags(res, bit0 == 1);
  }

  inline fn push(self: *CPU, mem: *Mem, v: u8) void {
    mem[0x0100 | @as(u16, self.sp)] = v;
    self.sp -= 1;
  }

  inline fn push16(self: *CPU, mem: *Mem, v: u16) void {
    const msb = @as(u8, @truncate(v >> 8));
    const lsb = @as(u8, @truncate(v));
    self.push(mem, lsb);
    self.push(mem, msb);
  }

  fn pha(self: *CPU, mem: *Mem) void {
    self.push(mem, self.a);
    self.cycles += 3;
  }

  fn php(self: *CPU, mem: *Mem) void {
    self.push(mem, @bitCast(self.p));
    self.cycles += 3;
  }

  inline fn pop(self: *CPU, mem: *Mem) u8 {
    self.sp += 1;
    const v = mem[0x0100 | @as(u16, self.sp)];
    return v;
  }

  inline fn pop16(self: *CPU, mem: *Mem) u16 {
    const msb = self.pop(mem);
    const lsb = self.pop(mem);
    return @as(u16, msb) << 8 | @as(u16, lsb);
  }

  fn pla(self: *CPU, mem: *Mem) void {
    self.a = self.pop(mem);
    self.cycles += 4;
    self.set_nz_flags(self.a);
  }

  fn plp(self: *CPU, mem: *Mem) void {
    self.p = @bitCast(self.pop(mem));
    self.cycles += 4;
  }


  fn txs(self: *CPU) void {
    self.sp = self.x;
    self.cycles += 2;
    // No flags affected
  }

  fn tsx(self: *CPU) void {
    self.x = self.sp;
    self.cycles += 2;

    self.set_nz_flags(self.x);
  }

  fn tya(self: *CPU) void {
    self.a = self.y;
    self.cycles += 2;

    self.set_nz_flags(self.a);
  }

  fn tay(self: *CPU) void {
    self.y = self.a;
    self.cycles += 2;

    self.set_nz_flags(self.y);
  }

  fn tax(self: *CPU) void {
    self.x = self.a;
    self.cycles += 2;

    self.set_nz_flags(self.x);
  }

  fn txa(self: *CPU) void {
    self.a = self.x;
    self.cycles += 2;

    self.set_nz_flags(self.a);
  }

  fn step(self: *CPU, mem: *Mem) void {
    const instr_pos = self.pc;
    const instr = mem[instr_pos];
    self.pc += 1;

    switch (instr) {
      // JMP - Jump
      0x4c => self.jmp(self.operand(mem, .absolute)),
      // 0x6c => self.jmp(self.operand(mem, .indirect)),
      // JSR - Jump to Subroutine
      0x20 => self.jsr(mem, self.operand(mem, .absolute)),
      // RTS - Return from Subroutine
      0x60 => self.rts(mem),
      // EOR - Exclusive OR
      0x49 => self.xor(mem, self.operand(mem, .immediate)),
      0x4d => self.xor(mem, self.operand(mem, .absolute)),
      // ADC - Add with Carry
      0x69 => self.adc(mem, self.operand(mem, .immediate)),
      0x6d => self.adc(mem, self.operand(mem, .absolute)),
      // TAY - Transfer Accumulator to Y
      0xa8 => self.tay(),
      // TYA - Transfer Y to Accumulator
      0x98 => self.tya(),
      // TXS - Transfer X to Stack Pointer
      0x9a => self.txs(),
      // TSX - Transfer Stack Pointer to X
      0xba => self.tsx(),
      // TAX - Transfer Accumulator to X
      0xaa => self.tax(),
      // TXA - Transfer X to Accumulator
      0x8a => self.txa(),
      // BPL - Branch if Positive
      0x10 => self.bpl(self.operand(mem, .relative)),
      // BNE - Branch if Not Equal
      0xd0 => self.bne(self.operand(mem, .relative)),
      // BEQ - Branch if Equal
      0xf0 => self.beq(self.operand(mem, .relative)),
      // BCC - Branch if Carry Clear
      0x90 => self.bcc(self.operand(mem, .relative)),
      // BCS - Branch if Carry Set
      0xb0 => self.bcs(self.operand(mem, .relative)),
      // BVC - Branch if Overflow Clear
      0x50 => self.bvc(self.operand(mem, .relative)),
      // BVS - Branch if Overflow Set
      0x70 => self.bvs(self.operand(mem, .relative)),
      // BMI - Branch if Minus
      0x30 => self.bmi(self.operand(mem, .relative)),
      // STA - Store Accumulator
      0x8d => self.sta(mem, self.operand(mem, .absolute)),
      0x85 => self.sta(mem, self.operand(mem, .zeroPage)),
      // CMP - Compare
      0xc9 => self.cmp(mem, self.operand(mem, .immediate)),
      0xcd => self.cmp(mem, self.operand(mem, .absolute)),
      // CPX - Compare X Register
      0xe0 => self.cpx(mem, self.operand(mem, .immediate)),
      0xec => self.cpx(mem, self.operand(mem, .absolute)),
      // CPY - Compare Y Register
      0xc0 => self.cpy(mem, self.operand(mem, .immediate)),
      0xcc => self.cpy(mem, self.operand(mem, .absolute)),
      // LDA - Load Accumulator
      0xa9 => self.lda(mem, self.operand(mem, .immediate)),
      0xad => self.lda(mem, self.operand(mem, .absolute)),
      0xbd => self.lda(mem, self.operand(mem, .indexedAbsoluteX)),
      0xb9 => self.lda(mem, self.operand(mem, .indexedAbsoluteY)),
      0xa5 => self.lda(mem, self.operand(mem, .zeroPage)),
      // LDX - Load X Register
      0xa2 => self.ldx(self.operand(mem, .immediate)),
      // LDY - Load Y Register
      0xa0 => self.ldy(self.operand(mem, .immediate)),
      // INX - Increment X Register
      0xe8 => self.inx(),
      // INY - Increment Y Register
      0xc8 => self.iny(),
      // DEY - Decrement Y Register
      0x88 => self.dey(),
      // DEX - Decrement X Register
      0xca => self.dex(),
      // PHA - Push Accumulator
      0x48 => self.pha(mem),
      // PHP - Push Processor Status
      0x08 => self.php(mem),
      // PLA - Pull Accumulator
      0x68 => self.pla(mem),
      // PLP - Pull Processor Status
      0x28 => self.plp(mem),
      // LSR - Logical Shift Right
      0x4a => self.lsr(mem, self.operand(mem, .accumulator)),
      0x4e => self.lsr(mem, self.operand(mem, .absolute)),
      0x1a,
      0x3a,
      0x5a,
      0x7a,
      0xea => {
        // NOP - No Operation
        self.cycles += 2;
      },
      0x18 => {
        // CLC - Clear Carry Flag
        self.p.carry = false;
        self.cycles += 2;
      },
      0x78 => {
        // SEI - Set Interrupt Disable
        self.p.interrupt_disable = true;
        self.cycles += 2;
      },
      0xd8 => {
        // CLD - Clear Decimal Mode
        self.p.decimal_mode = false;
        self.cycles += 2;
      },
      else => {
        const name = debug_op_code(instr);
        std.debug.print("Op code not implemented: {s} 0x{x:0>2} at: {x}\n", .{ name, instr, instr_pos });
        unreachable;
      }
    }
  }

  inline fn read_u16(self: *CPU, mem: *Mem) u16 {
    const lsb: u16 = mem[self.pc];
    const msb: u16 = mem[self.pc + 1];
    self.pc += 2;

    return (msb << 8) | lsb;
  }

  inline fn operand(self: *CPU, mem: *Mem, comptime addrType: anytype) AddrMode {
    switch (addrType) {
      .accumulator => {
        return .{ .accumulator = {} };
      },
      .zeroPage => {
        const b: u8 = mem[self.pc];
        self.pc += 1;
        return .{ .zeroPage = b };
      },
      .absolute => {
        const v = self.read_u16(mem);
        return .{ .absolute = v };
      },
      .indexedAbsoluteX => {
        const v = self.read_u16(mem);
        return .{ .indexedAbsoluteX = self.x + v };
      },
      .indexedAbsoluteY => {
        const v = self.read_u16(mem);
        return .{ .indexedAbsoluteY = self.y + v };
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
};



test "6502_functional_test" {
  var cpu = CPU.init();
  var mem: Mem = undefined;

  const text = @embedFile("6502_functional_test.bin");

  std.mem.copyForwards(u8, &mem, text);

  cpu.pc = 0x0400;

  cpu.run(&mem);
  std.debug.print("\npc = {x}\n", .{ cpu.pc });

  try std.testing.expectEqual(cpu.pc, 0x3399);
}
