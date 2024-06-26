const cpu_debug = @import("debug.zig");
const AddrMode = @import("addr.zig").AddrMode;
const Bus = @import("bus.zig").Bus;
const rom = @import("rom.zig");
const std = @import("std");

inline fn is_negative(v: u8) bool {
    return @clz(v) == 0;
}

pub const Op = union(std.meta.FieldEnum(AddrMode)) {
    zeroPage: *const (fn (self: *CPU, dst: AddrMode) void),
    indexedZeroPageX: *const (fn (self: *CPU, dst: AddrMode) void),
    indexedZeroPageY: *const (fn (self: *CPU, dst: AddrMode) void),
    absolute: *const (fn (self: *CPU, dst: AddrMode) void),
    indexedAbsoluteX: *const (fn (self: *CPU, dst: AddrMode) void),
    indexedAbsoluteY: *const (fn (self: *CPU, dst: AddrMode) void),
    indirect: *const (fn (self: *CPU, dst: AddrMode) void),
    implied: *const (fn (self: *CPU) void),
    accumulator: *const (fn (self: *CPU, dst: AddrMode) void),
    immediate: *const (fn (self: *CPU, dst: AddrMode) void),
    relative: *const (fn (self: *CPU, dst: AddrMode) void),
    indexedIndirect: *const (fn (self: *CPU, dst: AddrMode) void),
    indirectIndexed: *const (fn (self: *CPU, dst: AddrMode) void),
};

const Flags = packed struct(u8) {
    carry: bool = false,
    zero: bool = false,
    interrupt_disable: bool = true,
    decimal_mode: bool = false,
    break_command: bool = false,
    _padding: u1 = 1,
    overflow: bool = false,
    negative: bool = false,
};

const CPUError = error{unmappedCode};

pub const CPU = struct {
    bus: ?*Bus,
    pc: u16,

    /// Offset from $0100
    sp: u8,
    a: u8,
    x: u8,
    y: u8,

    debug: ?std.fs.File.Writer,

    /// N, V, _, B, D, I, Z, C
    p: Flags,

    cycles: u32,

    pub fn init() CPU {
        return .{ .bus = null, .p = Flags{}, .debug = null, .a = 0, .x = 0, .y = 0, .sp = 0xfd, .pc = 0, .cycles = 0 };
    }

    pub fn complete(self: *CPU) bool {
        return self.cycles == 0;
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
        self.p = Flags{};

        self.pc = self.read_u16(0xfffc);

        self.cycles = 8;
    }

    pub fn irq(self: *CPU) void {
        if (!self.p.interrupt_disable) {
            self.push16(self.pc);
            self.p.break_command = false;
            self.p._padding = 1;
            self.p.interrupt_disable = true;

            self.push(@bitCast(self.p));
            self.pc = self.read_u16(0xfffe);
            self.cycles = 7;
        }
    }

    pub fn nmi(self: *CPU) void {
        self.push16(self.pc);
        self.p.break_command = false;
        self.p._padding = 1;
        self.p.interrupt_disable = true;

        self.push(@bitCast(self.p));
        self.pc = self.read_u16(0xfffa);
        self.cycles = 8;
    }

    pub fn rti(self: *CPU) void {
        self.p = @bitCast(self.pop());

        self.p.break_command = false;
        self.p._padding = 1;

        self.pc = self.pop16();
        self.cycles += 6;
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
            .indirect => |v| v,
            else => unreachable,
        };

        self.cycles += switch (dst) {
            .indirect => 3,
            else => unreachable,
        };
    }

    fn brk(self: *CPU) void {
        self.p.interrupt_disable = true;
        self.push16(self.pc);

        self.p.break_command = true;
        self.push(@bitCast(self.p));
        self.p.break_command = false;

        self.pc = self.read_u16(0xfffe);
        self.cycles += 7;
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
        // JSR puts (pc-1) on the stack, we need to move ahead one step
        self.pc += 1;

        self.cycles += 6;
        // No flags affected
    }

    fn ldx(self: *CPU, dst: AddrMode) void {
        self.x = self.eval_operand_value(dst);

        self.cycles += switch (dst) {
            .immediate => 2,
            .zeroPage => 3,
            .indexedZeroPageY => 4,
            .absolute => 4,
            .indexedAbsoluteX => 4, // TODO: +1 if page crossed
            else => unreachable,
        };

        self.set_nz_flags(self.x);
    }

    fn ldy(self: *CPU, dst: AddrMode) void {
        self.y = self.eval_operand_value(dst);

        self.cycles += switch (dst) {
            .immediate => 2,
            .zeroPage => 3,
            .indexedZeroPageX => 4,
            .absolute => 4,
            .indexedAbsoluteX => 4, // TODO: +1 if page crossed
            else => unreachable,
        };

        self.set_nz_flags(self.y);
    }

    inline fn add_with_carry(self: *CPU, v: u16) void {
        const carry_num: u16 = if (self.p.carry) 1 else 0;

        const r: u16 = @as(u16, @intCast(self.a)) + v + carry_num;

        self.p.carry = r > 255;
        self.p.zero = (r & 0xff) == 0;

        self.p.overflow = ((~(self.a ^ v) & (self.a ^ r)) & 0x80) > 0;
        self.p.negative = (r & 0x80) > 0;

        self.a = @truncate(r);
    }

    fn adc(self: *CPU, dst: AddrMode) void {
        const m = self.eval_operand_value(dst);

        self.cycles += switch (dst) {
            .immediate => 2,
            .zeroPage => 3,
            .indexedZeroPageX => 4,
            .absolute => 4,
            .indexedAbsoluteX => 4, // TODO (+1 if page crossed)
            .indexedAbsoluteY => 4, // TODO (+1 if page crossed)
            .indexedIndirect => 6,
            .indirectIndexed => 5, // TODO (+1 if page crossed)
            else => unreachable,
        };

        self.add_with_carry(m);
    }

    fn sbc(self: *CPU, dst: AddrMode) void {
        const m = self.eval_operand_value(dst);

        self.cycles += switch (dst) {
            .immediate => 2,
            .zeroPage => 3,
            .indexedZeroPageX => 4,
            .absolute => 4,
            .indexedAbsoluteX => 4, // TODO (+1 if page crossed)
            .indexedAbsoluteY => 4, // TODO (+1 if page crossed)
            .indexedIndirect => 6,
            .indirectIndexed => 5, // TODO (+1 if page crossed)
            else => unreachable,
        };

        self.add_with_carry(m ^ 0xff);
    }

    fn cmp(self: *CPU, dst: AddrMode) void {
        const m = self.eval_operand_value(dst);

        // std.debug.print("{b:0>8} {b:0>8}\n", .{self.a, m});
        // std.debug.print("{x} {x}\n", .{self.a, m});

        self.cycles += switch (dst) {
            .immediate => 2,
            .zeroPage => 3,
            .indexedZeroPageX => 4,
            .absolute => 4,
            .indexedAbsoluteX => 4, // TODO: +1 if page crossed
            .indexedAbsoluteY => 4, // TODO: +1 if page crossed
            .indexedIndirect => 6,
            .indirectIndexed => 5, // TODO: +1 if page crossed
            else => unreachable,
        };

        self.set_cnz_flags(self.a -% m, self.a >= m);
    }

    fn cpx(self: *CPU, dst: AddrMode) void {
        const m = self.eval_operand_value(dst);

        self.cycles += switch (dst) {
            .immediate => 2,
            .indexedZeroPageX => 3,
            .absolute => 4,
            else => unreachable,
        };

        self.set_cnz_flags(self.x -% m, self.x >= m);
    }

    fn cpy(self: *CPU, dst: AddrMode) void {
        const m = self.eval_operand_value(dst);

        self.cycles += switch (dst) {
            .immediate => 2,
            .indexedZeroPageX => 3,
            .absolute => 4,
            else => unreachable,
        };

        self.set_cnz_flags(self.y -% m, self.y >= m);
    }

    fn cpu_and(self: *CPU, dst: AddrMode) void {
        const b = self.eval_operand_value(dst);

        self.a &= b;

        self.cycles += switch (dst) {
            .immediate => 2,
            .zeroPage => 3,
            .indexedZeroPageX => 3,
            .absolute => 4,
            .indexedAbsoluteX => 4, // TODO: +1 if page crossed
            .indexedAbsoluteY => 4, // TODO: +1 if page crossed
            .indexedIndirect => 6,
            else => unreachable,
        };

        self.set_nz_flags(self.a);
    }

    fn cpu_or(self: *CPU, dst: AddrMode) void {
        const b = self.eval_operand_value(dst);

        self.a |= b;

        self.cycles += switch (dst) {
            .immediate => 2,
            .zeroPage => 3,
            .indexedZeroPageX => 3,
            .absolute => 4,
            .indexedAbsoluteX => 4, // TODO: +1 if page crossed
            .indexedAbsoluteY => 4, // TODO: +1 if page crossed
            .indexedIndirect => 6,
            else => unreachable,
        };

        self.set_nz_flags(self.a);
    }

    fn xor(self: *CPU, dst: AddrMode) void {
        const m = self.eval_operand_value(dst);

        self.cycles += switch (dst) {
            .immediate => 2,
            .zeroPage => 3,
            .indexedZeroPageX => 4,
            .absolute => 4,
            .indexedAbsoluteX => 4, // TODO: +1 if page crossed
            .indexedAbsoluteY => 4, // TODO: +1 if page crossed
            .indexedIndirect => 6,
            else => unreachable,
        };

        self.a ^= m;

        self.set_nz_flags(self.a);
    }

    inline fn eval_operand_value(self: *CPU, op: AddrMode) u8 {
        return switch (op) {
            .accumulator => self.a,
            .absolute, .zeroPage => |k| self.read_bus(k),
            .immediate => |k| k,
            .indirect => |k| a: {
                if (k & 0xff00 == 0xff) { // Simulate page boundary hardware bug
                    const lsb: u16 = @intCast(self.read_bus(k + 0));
                    const msb: u16 = @intCast(self.read_bus(k & 0xff00));
                    const vv = ((msb << 8) | lsb);
                    break :a self.read_bus(vv);
                } else { // Behave normally
                    const lsb: u16 = @intCast(self.read_bus(k + 0));
                    const msb: u16 = @intCast(self.read_bus(k + 1));
                    const vv = ((msb << 8) | lsb);
                    break :a self.read_bus(vv);
                }
            },
            .indirectIndexed => |k| a: {
                const lsb: u16 = @intCast(self.read_bus(k + 0));
                const msb: u16 = @intCast(self.read_bus(k + 1));
                const vv = ((msb << 8) | lsb);
                break :a self.read_bus(vv + self.y);
            },
            .indexedIndirect => |k| a: {
                const kk = k + self.x;
                const lsb: u16 = @intCast(self.read_bus(kk + 0));
                const msb: u16 = @intCast(self.read_bus(kk + 1));
                const vv = ((msb << 8) | lsb);
                break :a self.read_bus(vv);
            },
            .indexedZeroPageX => |k| self.read_bus(self.x + k),
            .indexedZeroPageY => |k| self.read_bus(self.y + k),
            .indexedAbsoluteX => |k| self.read_bus(self.x + k),
            .indexedAbsoluteY => |k| self.read_bus(self.y + k),
            else => {
                std.debug.print("{}", .{op});
                unreachable;
            },
        };
    }

    fn lda(self: *CPU, dst: AddrMode) void {
        self.a = self.eval_operand_value(dst);

        self.cycles += switch (dst) {
            .immediate => 2,
            .absolute, .indexedAbsoluteX, .indexedAbsoluteY => 4,
            .zeroPage => 3,
            .indirectIndexed => 5,
            .indexedIndirect => 6,
            else => unreachable,
        };

        self.set_nz_flags(self.a);
    }

    fn sta(self: *CPU, dst: AddrMode) void {
        self.write_operand_value(dst, self.a);

        self.cycles += switch (dst) {
            .indexedAbsoluteX, .indexedAbsoluteY, .absolute => 3,
            .indexedIndirect, .indirectIndexed, .zeroPage, .indexedZeroPageX => 2,
            else => unreachable,
        };

        // No flags affected
    }

    fn stx(self: *CPU, dst: AddrMode) void {
        self.write_operand_value(dst, self.x);

        self.cycles += switch (dst) {
            .absolute => 4,
            .indexedZeroPageY => 4,
            .zeroPage => 3,
            else => unreachable,
        };

        // No flags affected
    }

    fn sty(self: *CPU, dst: AddrMode) void {
        self.write_operand_value(dst, self.y);

        self.cycles += switch (dst) {
            .absolute => 4,
            .indexedZeroPageX => 4,
            .zeroPage => 3,
            else => unreachable,
        };

        // No flags affected
    }

    inline fn branch(self: *CPU, dst: anytype, cond: bool) void {
        const addr: i8 = switch (dst) {
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

    fn inc(self: *CPU, dst: AddrMode) void {
        var m = self.eval_operand_value(dst);

        m +%= 1;

        self.write_operand_value(dst, m);

        self.cycles += switch (dst) {
            .zeroPage => 5,
            .indexedZeroPageX, .absolute => 6,
            .indexedAbsoluteX => 7,
            else => unreachable,
        };

        self.set_nz_flags(m);
    }

    fn dec(self: *CPU, dst: AddrMode) void {
        var m = self.eval_operand_value(dst);

        m -%= 1;

        self.write_operand_value(dst, m);

        self.cycles += switch (dst) {
            .zeroPage => 5,
            .indexedZeroPageX, .absolute => 6,
            .indexedAbsoluteX => 7,
            else => unreachable,
        };

        self.set_nz_flags(m);
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

    inline fn read_bus(self: *CPU, k: u16) u8 {
        return self.bus.?.read(k);
    }

    inline fn write_bus(self: *CPU, k: u16, v: u8) void {
        self.bus.?.write(k, v);
    }

    inline fn write_operand_value(self: *CPU, dst: AddrMode, val: u8) void {
        switch (dst) {
            .accumulator => self.a = val,
            .absolute, .zeroPage => |k| self.write_bus(k, val),
            .indirectIndexed => |k| a: {
                const lsb: u16 = @intCast(self.read_bus(k + 0));
                const msb: u16 = @intCast(self.read_bus(k + 1));
                const vv = ((msb << 8) | lsb);
                break :a self.write_bus(vv + self.y, val);
            },
            .indexedIndirect => |k| a: {
                const kk = k + self.x;
                const lsb: u16 = @intCast(self.read_bus(kk + 0));
                const msb: u16 = @intCast(self.read_bus(kk + 1));
                const vv = ((msb << 8) | lsb);
                break :a self.write_bus(vv, val);
            },
            .indexedZeroPageX => |k| self.write_bus(self.x + k, val),
            .indexedZeroPageY => |k| self.write_bus(self.y + k, val),
            .indexedAbsoluteX => |k| self.write_bus(self.x + k, val),
            .indexedAbsoluteY => |k| self.write_bus(self.y + k, val),
            else => unreachable,
        }
    }

    fn lsr(self: *CPU, dst: AddrMode) void {
        const m = self.eval_operand_value(dst);

        const bit0 = 0b1 & m;
        const res = m / 2;

        self.write_operand_value(dst, res);

        self.cycles += switch (dst) {
            .accumulator => 2,
            .absolute => 6,
            else => unreachable,
        };

        self.set_cnz_flags(res, bit0 == 1);
    }

    fn asl(self: *CPU, dst: AddrMode) void {
        const m: u16 = @intCast(self.eval_operand_value(dst));

        const res = m << 1;

        self.p.carry = (res & 0xff00) > 0;
        self.p.zero = (res & 0xff) == 0;
        self.p.negative = (res & 0x80) != 0;

        self.write_operand_value(dst, @truncate(res));

        self.cycles += switch (dst) {
            .accumulator => 2,
            .zeroPage => 5,
            .indexedZeroPageX => 6,
            .absolute => 6,
            .indexedAbsoluteX => 7,
            else => unreachable,
        };
    }

    fn ror(self: *CPU, dst: AddrMode) void {
        const m = self.eval_operand_value(dst);

        const bit0 = m & 0b1;

        const carry_num: u8 = if (self.p.carry) 0x80 else 0;

        const res = (m >> 1) | carry_num;

        self.write_operand_value(dst, res);

        self.cycles += switch (dst) {
            .accumulator => 2,
            .zeroPage => 5,
            .indexedZeroPageX => 6,
            .absolute => 6,
            .indexedAbsoluteX => 7,
            else => unreachable,
        };

        self.set_cnz_flags(res, bit0 != 0);
    }

    fn rol(self: *CPU, dst: AddrMode) void {
        const m = self.eval_operand_value(dst);

        const bit7 = m & 0x80;

        const carry_num: u8 = if (self.p.carry) 1 else 0;

        const res = (m << 1) | carry_num;

        self.write_operand_value(dst, res);

        self.cycles += switch (dst) {
            .accumulator => 2,
            .zeroPage => 5,
            .indexedZeroPageX => 6,
            .absolute => 6,
            .indexedAbsoluteX => 7,
            else => unreachable,
        };

        self.set_cnz_flags(res, bit7 != 0);
    }

    fn bit(self: *CPU, dst: AddrMode) void {
        const m = self.eval_operand_value(dst);

        const res = self.a & m;

        self.cycles += switch (dst) {
            .zeroPage => 2,
            .absolute => 3,
            else => unreachable,
        };

        self.p.zero = res == 0;
        self.p.overflow = (m & (1 << 6)) != 0;
        self.p.negative = (m & (1 << 7)) != 0;
    }

    inline fn push(self: *CPU, v: u8) void {
        self.bus.?.write(0x0100 | @as(u16, self.sp), v);
        self.sp -= 1;
    }

    inline fn push16(self: *CPU, v: u16) void {
        const msb = @as(u8, @truncate(v >> 8));
        const lsb = @as(u8, @truncate(v));
        self.push(msb);
        self.push(lsb);
    }

    fn pha(self: *CPU) void {
        self.push(self.a);
        self.cycles += 3;
    }

    fn php(self: *CPU) void {
        var a = self.p;
        a.break_command = true;
        a._padding = 1;
        self.push(@as(u8, @bitCast(a)));
        self.cycles += 3;
    }

    inline fn pop(self: *CPU) u8 {
        self.sp += 1;
        const v = self.bus.?.read(0x0100 | @as(u16, self.sp));

        return v;
    }

    inline fn pop16(self: *CPU) u16 {
        const lsb = self.pop();
        const msb = self.pop();
        return @as(u16, msb) << 8 | @as(u16, lsb);
    }

    fn pla(self: *CPU) void {
        self.a = self.pop();
        self.cycles += 4;
        self.set_nz_flags(self.a);
    }

    fn plp(self: *CPU) void {
        self.p = @bitCast(self.pop());
        self.p.break_command = false;
        self.p._padding = 1;
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

    fn nop(self: *CPU) void {
        self.cycles += 2;
    }

    fn clc(self: *CPU) void {
        self.p.carry = false;
        self.cycles += 2;
    }

    fn clv(self: *CPU) void {
        self.p.overflow = false;
        self.cycles += 2;
    }

    fn sec(self: *CPU) void {
        self.p.carry = true;
        self.cycles += 2;
    }

    fn sei(self: *CPU) void {
        self.p.interrupt_disable = true;
        self.cycles += 2;
    }

    fn cld(self: *CPU) void {
        self.p.decimal_mode = false;
        self.cycles += 2;
    }

    fn sed(self: *CPU) void {
        self.p.decimal_mode = true;
        self.cycles += 2;
    }

    fn mapOp(code: u8) !Op {
        return switch (code) {
            // BRK - Force Interrupt
            // 0x00 => self.brk(),
            // RTI - Return from Interrupt
            0x40 => .{ .implied = &rti },
            // JMP - Jump
            0x4c => .{ .indirect = &jmp },
            // 0x6c => self.jmp(self.operand(bus, .indirect)),
            // JSR - Jump to Subroutine
            0x20 => .{ .absolute = &jsr }, // },
            // RTS - Return from Subroutine
            0x60 => .{ .implied = &rts },
            // EOR - Exclusive OR
            0x49 => .{ .immediate = &xor },
            0x45 => .{ .zeroPage = &xor },
            0x55 => .{ .indexedZeroPageX = &xor },
            0x4d => .{ .absolute = &xor },
            0x5d => .{ .indexedAbsoluteX = &xor },
            0x59 => .{ .indexedAbsoluteY = &xor },
            0x41 => .{ .indexedIndirect = &xor },
            // AND - Logical AND
            0x29 => .{ .immediate = &cpu_and },
            0x25 => .{ .zeroPage = &cpu_and },
            0x35 => .{ .indexedZeroPageX = &cpu_and },
            0x2d => .{ .absolute = &cpu_and },
            0x3d => .{ .indexedAbsoluteX = &cpu_and },
            0x39 => .{ .indexedAbsoluteY = &cpu_and },
            0x21 => .{ .indexedIndirect = &cpu_and },
            // ORA - Logical Inclusive OR
            0x09 => .{ .immediate = &cpu_or },
            0x05 => .{ .zeroPage = &cpu_or },
            0x15 => .{ .indexedZeroPageX = &cpu_or },
            0x0d => .{ .absolute = &cpu_or },
            0x1d => .{ .indexedAbsoluteX = &cpu_or },
            0x19 => .{ .indexedAbsoluteY = &cpu_or },
            0x01 => .{ .indexedIndirect = &cpu_or },
            // ADC - Add with Carry
            0x69 => .{ .immediate = &adc },
            0x65 => .{ .zeroPage = &adc },
            0x75 => .{ .indexedZeroPageX = &adc },
            0x6d => .{ .absolute = &adc },
            0x7d => .{ .indexedAbsoluteX = &adc },
            0x79 => .{ .indexedAbsoluteY = &adc },
            0x71 => .{ .indirectIndexed = &adc },
            0x61 => .{ .indexedIndirect = &adc },
            // SBC - Add with Carry
            0xe9 => .{ .immediate = &sbc },
            0xe5 => .{ .zeroPage = &sbc },
            0xf5 => .{ .indexedZeroPageX = &sbc },
            0xed => .{ .absolute = &sbc },
            0xfd => .{ .indexedAbsoluteX = &sbc },
            0xf9 => .{ .indexedAbsoluteY = &sbc },
            0xe1 => .{ .indirectIndexed = &sbc },
            0xf1 => .{ .indexedIndirect = &sbc },
            // TAY - Transfer Accumulator to Y
            0xa8 => .{ .implied = &tay },
            // TYA - Transfer Y to Accumulator
            0x98 => .{ .implied = &tya },
            // TXS - Transfer X to Stack Pointer
            0x9a => .{ .implied = &txs },
            // TSX - Transfer Stack Pointer to X
            0xba => .{ .implied = &tsx },
            // TAX - Transfer Accumulator to X
            0xaa => .{ .implied = &tax },
            // TXA - Transfer X to Accumulator
            0x8a => .{ .implied = &txa },
            // BPL - Branch if Positive
            0x10 => .{ .relative = &bpl },
            // BNE - Branch if Not Equal
            0xd0 => .{ .relative = &bne },
            // BEQ - Branch if Equal
            0xf0 => .{ .relative = &beq },
            // BCC - Branch if Carry Clear
            0x90 => .{ .relative = &bcc },
            // BCS - Branch if Carry Set
            0xb0 => .{ .relative = &bcs },
            // BVC - Branch if Overflow Clear
            0x50 => .{ .relative = &bvc },
            // BVS - Branch if Overflow Set
            0x70 => .{ .relative = &bvs },
            // BMI - Branch if Minus
            0x30 => .{ .relative = &bmi },
            // STA - Store Accumulator
            0x85 => .{ .zeroPage = &sta },
            0x95 => .{ .indexedZeroPageX = &sta },
            0x8d => .{ .absolute = &sta },
            0x9d => .{ .indexedAbsoluteX = &sta },
            0x99 => .{ .indexedAbsoluteY = &sta },
            0x81 => .{ .indexedIndirect = &sta },
            0x91 => .{ .indirectIndexed = &sta },
            // STX - Store X Register
            0x86 => .{ .zeroPage = &stx },
            0x96 => .{ .indexedZeroPageY = &stx },
            0x8e => .{ .absolute = &stx },
            // STY - Store Y Register
            0x84 => .{ .zeroPage = &sty },
            0x94 => .{ .indexedZeroPageX = &sty },
            0x8c => .{ .absolute = &sty },
            // CMP - Compare
            0xc9 => .{ .immediate = &cmp },
            0xc5 => .{ .zeroPage = &cmp },
            0xd5 => .{ .indexedZeroPageX = &cmp },
            0xcd => .{ .absolute = &cmp },
            0xdd => .{ .indexedAbsoluteX = &cmp },
            0xd9 => .{ .indexedAbsoluteY = &cmp },
            0xc1 => .{ .indexedIndirect = &cmp },
            0xd1 => .{ .indirectIndexed = &cmp },
            // CPX - Compare X Register
            0xe0 => .{ .immediate = &cpx },
            0xe4 => .{ .indexedZeroPageX = &cpx },
            0xec => .{ .absolute = &cpx },
            // CPY - Compare Y Register
            0xc0 => .{ .immediate = &cpy },
            0xc4 => .{ .indexedZeroPageX = &cpy },
            0xcc => .{ .absolute = &cpy },
            // LDA - Load Accumulator
            0xa9 => .{ .immediate = &lda },
            0xad => .{ .absolute = &lda },
            0xbd => .{ .indexedAbsoluteX = &lda },
            0xb9 => .{ .indexedAbsoluteY = &lda },
            0xa5 => .{ .zeroPage = &lda },
            0xa1 => .{ .indexedIndirect = &lda },
            0xb1 => .{ .indirectIndexed = &lda },
            // LDX - Load X Register
            0xa2 => .{ .immediate = &ldx },
            0xa6 => .{ .zeroPage = &ldx },
            0xb6 => .{ .indexedZeroPageY = &ldx },
            0xae => .{ .absolute = &ldx },
            0xbe => .{ .indexedAbsoluteY = &ldx },
            // LDY - Load Y Register
            0xa0 => .{ .immediate = &ldy },
            0xa4 => .{ .zeroPage = &ldy },
            0xb4 => .{ .indexedZeroPageX = &ldy },
            0xac => .{ .absolute = &ldy },
            0xbc => .{ .indexedAbsoluteX = &ldy },
            // INC - Increment Memory
            0xe6 => .{ .zeroPage = &inc },
            0xf6 => .{ .indexedZeroPageX = &inc },
            0xee => .{ .absolute = &inc },
            0xfe => .{ .indexedAbsoluteX = &inc },
            // INX - Increment X Register
            0xe8 => .{ .implied = &inx },
            // INY - Increment Y Register
            0xc8 => .{ .implied = &iny },
            // DEC - Decrement Memory
            0xc6 => .{ .zeroPage = &dec },
            0xd6 => .{ .indexedZeroPageX = &dec },
            0xce => .{ .absolute = &dec },
            0xde => .{ .indexedAbsoluteX = &dec },
            // DEY - Decrement Y Register
            0x88 => .{ .implied = &dey },
            // DEX - Decrement X Register
            0xca => .{ .implied = &dex },
            // PHA - Push Accumulator
            0x48 => .{ .implied = &pha },
            // PHP - Push Processor Status
            0x08 => .{ .implied = &php },
            // PLA - Pull Accumulator
            0x68 => .{ .implied = &pla },
            // PLP - Pull Processor Status
            0x28 => .{ .implied = &plp },
            // BIT - Bit Test
            0x24 => .{ .zeroPage = &bit },
            0x2c => .{ .absolute = &bit },
            // LSR - Logical Shift Right
            0x4a => .{ .accumulator = &lsr },
            0x4e => .{ .absolute = &lsr },
            // ASL - Arithmetic Shift Left
            0x0a => .{ .accumulator = &asl },
            0x06 => .{ .zeroPage = &asl },
            0x16 => .{ .indexedZeroPageX = &asl },
            0x0e => .{ .absolute = &asl },
            0x1e => .{ .indexedAbsoluteX = &asl },
            // ROR - Rotate Right
            0x6a => .{ .accumulator = &ror },
            0x66 => .{ .zeroPage = &ror },
            0x76 => .{ .indexedZeroPageX = &ror },
            0x6e => .{ .absolute = &ror },
            0x7e => .{ .indexedAbsoluteX = &ror },
            // ROL - Rotate Left
            0x2a => .{ .accumulator = &rol },
            0x26 => .{ .zeroPage = &rol },
            0x36 => .{ .indexedZeroPageX = &rol },
            0x2e => .{ .absolute = &rol },
            0x3e => .{ .indexedAbsoluteX = &rol },
            // NOP - No Operation
            0xc2, 0x1a, 0x3a, 0x5a, 0x7a, 0xea => .{ .implied = &nop },
            // CLC - Clear Carry Flag
            0x18 => .{ .implied = &clc },
            // CLV - Clear Overflow Flag
            0xb8 => .{ .implied = &clv },
            // SEC - Set Carry Flag
            0x38 => .{ .implied = &sec },
            // SEI - Set Interrupt Disable
            0x78 => .{ .implied = &sei },
            // CLD - Clear Decimal Mode
            0xd8 => .{ .implied = &cld },
            // SED - Set Decimal Flag
            0xf8 => .{ .implied = &sed },
            else => CPUError.unmappedCode,
        };
    }

    fn step(self: *CPU) void {
        const instr_pos = self.pc;
        const instr = self.bus.?.read(instr_pos);
        self.pc += 1;

        const op: Op = mapOp(instr) catch {
            const name = cpu_debug.debug_op_code(instr);
            const nameWithDetails = name[1];
            std.debug.print("Op code not implemented: {s} 0x{x:0>2} at: {x}\n", .{ nameWithDetails, instr, instr_pos });
            unreachable;
        };

        const curr_operand: AddrMode = switch (op) {
            .implied => .{ .implied = {} },
            .accumulator => .{ .accumulator = {} },
            .zeroPage => operand(self, .zeroPage),
            .indexedZeroPageX => operand(self, .indexedZeroPageX),
            .indexedZeroPageY => operand(self, .indexedZeroPageY),
            .absolute => operand(self, .absolute),
            .indirect => operand(self, .indirect),
            .indexedAbsoluteX => operand(self, .indexedAbsoluteX),
            .indexedAbsoluteY => operand(self, .indexedAbsoluteY),
            .indirectIndexed => operand(self, .indirectIndexed),
            .indexedIndirect => operand(self, .indexedIndirect),
            .immediate => operand(self, .immediate),
            .relative => operand(self, .relative),
        };

        if (self.debug) |writer| {
            cpu_debug.debug_print(self, writer, curr_operand, instr_pos, instr) catch unreachable;
        }

        switch (op) {
            .implied => |func| func(self),
            .accumulator => |func| func(self, curr_operand),
            .zeroPage => |func| func(self, curr_operand),
            .indexedZeroPageX => |func| func(self, curr_operand),
            .indexedZeroPageY => |func| func(self, curr_operand),
            .absolute => |func| func(self, curr_operand),
            .indirect => |func| func(self, curr_operand),
            .indexedAbsoluteX => |func| func(self, curr_operand),
            .indexedAbsoluteY => |func| func(self, curr_operand),
            .indirectIndexed => |func| func(self, curr_operand),
            .indexedIndirect => |func| func(self, curr_operand),
            .immediate => |func| func(self, curr_operand),
            .relative => |func| func(self, curr_operand),
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
                return .{ .indexedZeroPageX = v };
            },
            .indexedZeroPageY => {
                const v: u8 = self.bus.?.read(self.pc);
                self.pc += 1;
                return .{ .indexedZeroPageY = v };
            },
            .indirect => {
                const v = self.read_u16_operand();
                return .{ .indirect = v };
            },
            .absolute => {
                const v = self.read_u16_operand();
                return .{ .absolute = v };
            },
            .indexedAbsoluteX => {
                const v = self.read_u16_operand();
                return .{ .indexedAbsoluteX = v };
            },
            .indexedAbsoluteY => {
                const v = self.read_u16_operand();
                return .{ .indexedAbsoluteY = v };
            },
            .indirectIndexed => {
                const v = self.bus.?.read(self.pc);
                self.pc += 1;

                return .{ .indirectIndexed = v };
            },
            .indexedIndirect => {
                const v = self.bus.?.read(self.pc);
                self.pc += 1;

                return .{ .indexedIndirect = v };
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
            },
        }
    }
};

test "6502_functional_test" {
    std.debug.print("\n", .{});
    const file = @embedFile("6502_functional_test.bin");
    var buffer: [file.len]u8 = undefined;

    std.mem.copyForwards(u8, &buffer, file);

    const loaded_rom = try rom.Rom.load_unchecked(&buffer);
    var nes: Bus = Bus.init();
    nes.cpu.bus = &nes;
    nes.cpu.debug = std.io.getStdOut().writer();

    nes.load_rom(&loaded_rom);

    nes.reset();

    nes.cpu.pc = 0x0400;

    var pc: u16 = 0;
    while (nes.cpu.pc != pc) {
        pc = nes.cpu.pc;
        nes.clock();
    }

    try std.testing.expectEqual(0x3399, nes.cpu.pc);
}

test "nestest" {
    std.debug.print("\n", .{});
    const file = @embedFile("roms/nestest.nes");
    var buffer: [file.len]u8 = undefined;

    std.mem.copyForwards(u8, &buffer, file);

    const loaded_rom = try rom.Rom.load(&buffer);
    var nes: Bus = Bus.init();
    nes.cpu.debug = std.io.getStdOut().writer();
    nes.cpu.bus = &nes;

    nes.load_rom(&loaded_rom);

    nes.reset();

    nes.cpu.pc = 0xC000;

    while (nes.cpu.pc != 0xC66E) {
        nes.clock();
    }

    try std.testing.expectEqual(0x0000, nes.read(0x0002));
    try std.testing.expectEqual(0x0000, nes.read(0x0003));
}
