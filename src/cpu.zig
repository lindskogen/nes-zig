const debug_op_code = @import("debug.zig").debug_op_code;
const AddrMode = @import("addr.zig").AddrMode;
const Bus = @import("bus.zig").Bus;
const std = @import("std");

inline fn is_negative(v: u8) bool {
  return @clz(v) == 0;
}

const Flags = packed struct(u8) {
  negative: bool = false,
  overflow: bool = false,
  _padding: u1 = 1,
  break_command: bool = false,
  decimal_mode: bool = false,
  interrupt_disable: bool = false,
  zero: bool = false,
  carry: bool = false,
};

pub const CPU = struct {
  bus: ?*Bus,
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
    return .{ .bus = null, .p = Flags {}, .a = 0, .x = 0, .y = 0, .sp = 0xfd, .pc = 0, .cycles = 0 };
  }

  inline fn read_u16(self: *CPU, addr: u16) u16 {
    const lsb = @as(u16, self.bus.?.read(addr + 0));
    const msb = @as(u16, self.bus.?.read(addr + 1));
    return (msb << 8) | lsb;
  }

  pub fn reset(self: *CPU) void {
    self.a = 0;
    self.x = 0;
    self.y = 0;
    self.sp = 0xfd;
    self.p =  Flags {};

    self.pc = self.read_u16(0xfffc);

    self.cycles = 8;
  }

  pub fn irq(self: *CPU) void {
    if (!self.p.interrupt_disable) {
      self.push16(self.pc);
      self.p.break_command = false;
      self.p._padding = 1;
      self.p.interrupt_disable = true;

      self.push(self.p);
      self.pc = self.read_u16(0xfffe);
      self.cycles = 7;
    }
  }

  pub fn nmi(self: *CPU) void {
    self.push16(self.pc);
    self.p.break_command = false;
    self.p._padding = 1;
    self.p.interrupt_disable = true;

    self.push(self.p);
    self.pc = self.read_u16(0xfffa);
    self.cycles = 8;
  }

  pub fn rti(self: *CPU) void {
    self.p = self.pop();
    // TODO: restore flags B & U?
    self.pc = self.pop16();
  }

  pub fn clock(self: *CPU) void {
    if (self.cycles == 0) {
      self.step();
    }
    self.cycles -= 1;
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

  fn jsr(self: *CPU, dst: AddrMode) void {
    self.push16(self.pc - 1);

    self.pc = switch (dst) {
      .absolute => |v| v,
      else => unreachable,
    };

    self.cycles += 6;
    // No flags affected
  }

  fn rts(self: *CPU) void {
    self.pc = self.pop16();

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

  fn adc(self: *CPU, dst: AddrMode) void {
     const m = switch (dst) {
      .immediate => |v| v,
      .absolute => |v| self.bus.?.read(v),
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

  fn cmp(self: *CPU, dst: AddrMode) void {
    const m = switch (dst) {
      .immediate => |v| v,
      .absolute => |v| self.bus.?.read(v),
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

  fn cpx(self: *CPU, dst: AddrMode) void {
    const m = switch (dst) {
      .immediate => |v| v,
      .absolute => |v| self.bus.?.read(v),
      else => unreachable,
    };

    self.cycles += switch (dst) {
      .immediate => 2,
      .absolute => 4,
      else => unreachable,
    };

    self.set_cnz_flags(self.x -% m, self.x >= m);
  }

  fn cpy(self: *CPU, dst: AddrMode) void {
    const m = switch (dst) {
      .immediate => |v| v,
      .absolute => |v| self.bus.?.read(v),
      else => unreachable,
    };

    self.cycles += switch (dst) {
      .immediate => 2,
      .absolute => 4,
      else => unreachable,
    };

    self.set_cnz_flags(self.y -% m, self.y >= m);
  }

  fn xor(self: *CPU, dst: AddrMode) void {
    const m = switch (dst) {
      .immediate => |v| v,
      .absolute => |v| self.bus.?.read(v),
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

  fn lda(self: *CPU, dst: AddrMode) void {
    self.a = switch (dst) {
      .immediate => |v| v,
      .absolute,
      .indexedAbsoluteX,
      .indexedAbsoluteY => |v| self.bus.?.read(v),
      .zeroPage => |v| self.bus.?.read(v),
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

  fn sta(self: *CPU, dst: AddrMode) void {
    const addr = switch (dst) {
      .absolute => |v| v,
      .zeroPage => |v| v,
      else => unreachable,
    };

    self.bus.?.write(addr, self.a);

    self.cycles += switch (dst) {
      .absolute => 4,
      .zeroPage => 3,
      else => unreachable,
    };

    // No flags affected
  }

  fn stx(self: *CPU, dst: AddrMode) void {
    const addr = switch (dst) {
      .absolute => |v| v,
      .zeroPage => |v| v,
      .indexedZeroPageY => |v| v,
      else => unreachable,
    };

    self.bus.?.write(addr, self.x);

    self.cycles += switch (dst) {
      .absolute => 4,
      .indexedZeroPageY => 4,
      .zeroPage => 3,
      else => unreachable,
    };

    // No flags affected
  }

  fn sty(self: *CPU, dst: AddrMode) void {
    const addr = switch (dst) {
      .absolute => |v| v,
      .zeroPage => |v| v,
      .indexedZeroPageX => |v| v,
      else => unreachable,
    };

    self.bus.?.write(addr, self.y);

    self.cycles += switch (dst) {
      .absolute => 4,
      .indexedZeroPageX => 4,
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

  fn lsr(self: *CPU, dst: AddrMode) void {
    const m = switch (dst) {
      .accumulator => self.a,
      .absolute => |v| self.bus.?.read(v),
      else => unreachable,
    };

    const bit0 = 0b1 & m;
    const res = m / 2;

    switch (dst) {
      .accumulator => {
        self.a = res;
      },
      .absolute => |v| {
        self.bus.?.write(v, res);
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

  inline fn push(self: *CPU, v: u8) void {
    self.bus.?.write(0x0100 | @as(u16, self.sp), v);
    self.sp -= 1;
  }

  inline fn push16(self: *CPU, v: u16) void {
    const msb = @as(u8, @truncate(v >> 8));
    const lsb = @as(u8, @truncate(v));
    self.push(lsb);
    self.push(msb);
  }

  fn pha(self: *CPU) void {
    self.push(self.a);
    self.cycles += 3;
  }

  fn php(self: *CPU) void {
    self.push(@bitCast(self.p));
    self.cycles += 3;
  }

  inline fn pop(self: *CPU) u8 {
    self.sp += 1;
    const v = self.bus.?.read(0x0100 | @as(u16, self.sp));
    return v;
  }

  inline fn pop16(self: *CPU) u16 {
    const msb = self.pop();
    const lsb = self.pop();
    return @as(u16, msb) << 8 | @as(u16, lsb);
  }

  fn pla(self: *CPU) void {
    self.a = self.pop();
    self.cycles += 4;
    self.set_nz_flags(self.a);
  }

  fn plp(self: *CPU) void {
    self.p = @bitCast(self.pop());
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

  fn step(self: *CPU) void {
    const instr_pos = self.pc;
    const instr = self.bus.?.read(instr_pos);
    self.pc += 1;

    switch (instr) {
      // JMP - Jump
      0x4c => self.jmp(self.operand(.absolute)),
      // 0x6c => self.jmp(self.operand(bus, .indirect)),
      // JSR - Jump to Subroutine
      0x20 => self.jsr(self.operand(.absolute)),
      // RTS - Return from Subroutine
      0x60 => self.rts(),
      // EOR - Exclusive OR
      0x49 => self.xor(self.operand(.immediate)),
      0x4d => self.xor(self.operand(.absolute)),
      // ADC - Add with Carry
      0x69 => self.adc(self.operand(.immediate)),
      0x6d => self.adc(self.operand(.absolute)),
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
      0x10 => self.bpl(self.operand(.relative)),
      // BNE - Branch if Not Equal
      0xd0 => self.bne(self.operand(.relative)),
      // BEQ - Branch if Equal
      0xf0 => self.beq(self.operand(.relative)),
      // BCC - Branch if Carry Clear
      0x90 => self.bcc(self.operand(.relative)),
      // BCS - Branch if Carry Set
      0xb0 => self.bcs(self.operand(.relative)),
      // BVC - Branch if Overflow Clear
      0x50 => self.bvc(self.operand(.relative)),
      // BVS - Branch if Overflow Set
      0x70 => self.bvs(self.operand(.relative)),
      // BMI - Branch if Minus
      0x30 => self.bmi(self.operand(.relative)),
      // STA - Store Accumulator
      0x8d => self.sta(self.operand(.absolute)),
      0x85 => self.sta(self.operand(.zeroPage)),
      // STX - Store X Register
      0x86 => self.stx(self.operand(.zeroPage)),
      0x96 => self.stx(self.operand(.indexedZeroPageY)),
      0x8e => self.stx(self.operand(.absolute)),
      // STY - Store Y Register
      0x84 => self.sty(self.operand(.zeroPage)),
      0x94 => self.sty(self.operand(.indexedZeroPageX)),
      0x8c => self.sty(self.operand(.absolute)),
      // CMP - Compare
      0xc9 => self.cmp(self.operand(.immediate)),
      0xcd => self.cmp(self.operand(.absolute)),
      // CPX - Compare X Register
      0xe0 => self.cpx(self.operand(.immediate)),
      0xec => self.cpx(self.operand(.absolute)),
      // CPY - Compare Y Register
      0xc0 => self.cpy(self.operand(.immediate)),
      0xcc => self.cpy(self.operand(.absolute)),
      // LDA - Load Accumulator
      0xa9 => self.lda(self.operand(.immediate)),
      0xad => self.lda(self.operand(.absolute)),
      0xbd => self.lda(self.operand(.indexedAbsoluteX)),
      0xb9 => self.lda(self.operand(.indexedAbsoluteY)),
      0xa5 => self.lda(self.operand(.zeroPage)),
      // LDX - Load X Register
      0xa2 => self.ldx(self.operand(.immediate)),
      // LDY - Load Y Register
      0xa0 => self.ldy(self.operand(.immediate)),
      // INX - Increment X Register
      0xe8 => self.inx(),
      // INY - Increment Y Register
      0xc8 => self.iny(),
      // DEY - Decrement Y Register
      0x88 => self.dey(),
      // DEX - Decrement X Register
      0xca => self.dex(),
      // PHA - Push Accumulator
      0x48 => self.pha(),
      // PHP - Push Processor Status
      0x08 => self.php(),
      // PLA - Pull Accumulator
      0x68 => self.pla(),
      // PLP - Pull Processor Status
      0x28 => self.plp(),
      // LSR - Logical Shift Right
      0x4a => self.lsr(self.operand(.accumulator)),
      0x4e => self.lsr(self.operand(.absolute)),
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

  inline fn read_u16_operand(self: *CPU) u16 {
    const lsb: u16 = self.bus.?.read(self.pc);
    const msb: u16 = self.bus.?.read(self.pc + 1);
    self.pc += 2;

    return (msb << 8) | lsb;
  }

  inline fn operand(self: *CPU, comptime addrType: anytype) AddrMode {
    switch (addrType) {
      .accumulator => {
        return .{ .accumulator = {} };
      },
      .zeroPage => {
        const b: u8 = self.bus.?.read(self.pc);
        self.pc += 1;
        return .{ .zeroPage = b };
      },
      .indexedZeroPageX => {
        const v: u8 = self.bus.?.read(self.pc);
        self.pc += 1;
        return .{ .indexedZeroPageX = self.x + v };
      },
      .indexedZeroPageY => {
        const v: u8 = self.bus.?.read(self.pc);
        self.pc += 1;
        return .{ .indexedZeroPageY = self.y + v };
      },
      .absolute => {
        const v = self.read_u16_operand();
        return .{ .absolute = v };
      },
      .indexedAbsoluteX => {
        const v = self.read_u16_operand();
        return .{ .indexedAbsoluteX = self.x + v };
      },
      .indexedAbsoluteY => {
        const v = self.read_u16_operand();
        return .{ .indexedAbsoluteY = self.y + v };
      },
      .immediate => {
        const b: u8 = self.bus.?.read(self.pc);
        self.pc += 1;
        return .{ .immediate = b };
      },
      .relative => {
        const b: i8 = @as(i8, @bitCast(self.bus.?.read(self.pc)));
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
  var bus: Bus = undefined;

  const text = @embedFile("6502_functional_test.bin");

  std.bus.copyForwards(u8, &bus, text);

  cpu.pc = 0x0400;

  cpu.run(&bus);
  std.debug.print("\npc = {x}\n", .{ cpu.pc });

  try std.testing.expectEqual(cpu.pc, 0x3399);
}
