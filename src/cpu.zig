const cpu_debug = @import("debug.zig");
const Bus = @import("bus.zig").Bus;
const rom = @import("rom.zig");
const std = @import("std");

inline fn is_negative(v: u8) bool {
    return @clz(v) == 0;
}

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

    // Cycle-stepped state machine fields
    opcode: u8 = 0,
    step: u4 = 0,
    addr: u16 = 0,
    ptr: u8 = 0,
    data: u8 = 0,
    data2: u8 = 0, // secondary latch (page-cross flag in bit 0)

    total_cycles: u64 = 0,

    // Interrupt lines (driven by bus wiring)
    nmi_line: bool = false,
    nmi_prev: bool = false,
    nmi_pending: bool = false,
    irq_line: bool = false,
    interrupt_pending: InterruptType = .none,

    const InterruptType = enum { none, nmi, irq };

    pub fn init() CPU {
        return .{
            .bus = null,
            .p = Flags{},
            .debug = null,
            .a = 0,
            .x = 0,
            .y = 0,
            .sp = 0xfd,
            .pc = 0,
        };
    }

    pub fn complete(self: *CPU) bool {
        return self.step == 0;
    }

    pub fn reset(self: *CPU) void {
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = 0xfd;
        self.p = Flags{};

        // Read reset vector
        const lo: u16 = self.read(0xfffc);
        const hi: u16 = self.read(0xfffd);
        self.pc = (hi << 8) | lo;

        self.step = 0;
        self.opcode = 0;
        self.nmi_line = false;
        self.nmi_prev = false;
        self.nmi_pending = false;
        self.irq_line = false;
        self.interrupt_pending = .none;
    }

    fn set_nz_flags(self: *CPU, v: u8) void {
        self.p.zero = v == 0;
        self.p.negative = is_negative(v);
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

    inline fn read(self: *CPU, k: u16) u8 {
        return self.bus.?.read(k);
    }

    inline fn write(self: *CPU, k: u16, v: u8) void {
        self.bus.?.write(k, v);
    }

    pub fn clock(self: *CPU) void {
        self.total_cycles += 1;
        // NMI edge detection: rising edge of nmi_line latches nmi_pending
        if (self.nmi_line and !self.nmi_prev) {
            self.nmi_pending = true;
        }
        self.nmi_prev = self.nmi_line;

        if (self.step == 0) {
            // Instruction boundary — check for pending interrupts
            if (self.nmi_pending) {
                self.nmi_pending = false;
                self.interrupt_pending = .nmi;
                _ = self.read(self.pc);
                self.opcode = 0;
                self.step = 1;
                return;
            } else if (self.irq_line and !self.p.interrupt_disable) {
                self.interrupt_pending = .irq;
                _ = self.read(self.pc);
                self.opcode = 0;
                self.step = 1;
                return;
            } else {
                // Fetch opcode
                self.opcode = self.read(self.pc);

                if (self.debug) |writer| {
                    cpu_debug.debug_print(self, writer, self.pc, self.opcode) catch unreachable;
                }

                self.pc +%= 1;
                self.step = 1;
                return;
            }
        }

        // Execute one step of the current instruction
        if (self.interrupt_pending != .none) {
            self.exec_interrupt();
        } else {
            const handler = opcode_table[self.opcode];
            handler(self);
        }
    }

    fn exec_interrupt(self: *CPU) void {
        // Interrupt sequence: 7 cycles total
        // step 1: dummy read (already done at step 0)
        // step 2: push PCH
        // step 3: push PCL
        // step 4: push P
        // step 5: read vector low
        // step 6: read vector high → done
        switch (self.step) {
            1 => {
                // Second dummy read
                _ = self.read(self.pc);
                self.step = 2;
            },
            2 => {
                self.write(0x0100 | @as(u16, self.sp), @truncate(self.pc >> 8));
                self.sp -%= 1;
                self.step = 3;
            },
            3 => {
                self.write(0x0100 | @as(u16, self.sp), @truncate(self.pc));
                self.sp -%= 1;
                self.step = 4;
            },
            4 => {
                var flags = self.p;
                flags.break_command = false;
                flags._padding = 1;
                self.write(0x0100 | @as(u16, self.sp), @bitCast(flags));
                self.sp -%= 1;
                self.p.interrupt_disable = true;
                self.step = 5;
            },
            5 => {
                const vector: u16 = if (self.interrupt_pending == .nmi) 0xFFFA else 0xFFFE;
                self.addr = self.read(vector);
                self.step = 6;
            },
            6 => {
                const vector: u16 = if (self.interrupt_pending == .nmi) 0xFFFA else 0xFFFE;
                self.pc = (@as(u16, self.read(vector + 1)) << 8) | (self.addr & 0xFF);
                self.interrupt_pending = .none;
                self.step = 0;
            },
            else => unreachable,
        }
    }

    // ─── Operations ─────────────────────────────────────────────────────────

    fn op_lda(self: *CPU, val: u8) void {
        self.a = val;
        self.set_nz_flags(self.a);
    }
    fn op_ldx(self: *CPU, val: u8) void {
        self.x = val;
        self.set_nz_flags(self.x);
    }
    fn op_ldy(self: *CPU, val: u8) void {
        self.y = val;
        self.set_nz_flags(self.y);
    }
    fn op_adc(self: *CPU, val: u8) void {
        self.add_with_carry(val);
    }
    fn op_sbc(self: *CPU, val: u8) void {
        self.add_with_carry(val ^ 0xff);
    }
    fn op_and(self: *CPU, val: u8) void {
        self.a &= val;
        self.set_nz_flags(self.a);
    }
    fn op_lax(self: *CPU, val: u8) void {
        self.a = val;
        self.x = val;
        self.set_nz_flags(val);
    }
    fn op_anc(self: *CPU, val: u8) void {
        self.a &= val;
        self.set_nz_flags(self.a);
        self.p.carry = self.p.negative;
    }
    fn op_alr(self: *CPU, val: u8) void {
        self.a &= val;
        self.p.carry = self.a & 1 != 0;
        self.a >>= 1;
        self.set_nz_flags(self.a);
    }
    fn op_axs(self: *CPU, val: u8) void {
        const t: u8 = self.a & self.x;
        self.p.carry = t >= val;
        self.x = t -% val;
        self.set_nz_flags(self.x);
    }
    fn op_atx(self: *CPU, val: u8) void {
        self.a = (self.a | 0xff) & val;
        self.x = self.a;
        self.set_nz_flags(self.a);
    }
    fn op_arr(self: *CPU, val: u8) void {
        self.a &= val;
        self.a = (self.a >> 1) | (if (self.p.carry) @as(u8, 0x80) else @as(u8, 0));
        self.set_nz_flags(self.a);
        self.p.carry = self.a & 0x40 != 0;
        self.p.overflow = (self.a & 0x40 != 0) != (self.a & 0x20 != 0);
    }
    fn op_ora(self: *CPU, val: u8) void {
        self.a |= val;
        self.set_nz_flags(self.a);
    }
    fn op_eor(self: *CPU, val: u8) void {
        self.a ^= val;
        self.set_nz_flags(self.a);
    }
    fn op_cmp(self: *CPU, val: u8) void {
        self.p.carry = self.a >= val;
        self.set_nz_flags(self.a -% val);
    }
    fn op_cpx(self: *CPU, val: u8) void {
        self.p.carry = self.x >= val;
        self.set_nz_flags(self.x -% val);
    }
    fn op_cpy(self: *CPU, val: u8) void {
        self.p.carry = self.y >= val;
        self.set_nz_flags(self.y -% val);
    }
    fn op_bit(self: *CPU, val: u8) void {
        self.p.zero = (self.a & val) == 0;
        self.p.overflow = (val & (1 << 6)) != 0;
        self.p.negative = (val & (1 << 7)) != 0;
    }

    // RMW operations: take old value, return new value
    fn op_slo(self: *CPU, val: u8) u8 {
        self.p.carry = (val & 0x80) != 0;
        const res = val << 1;
        self.a |= res;
        self.set_nz_flags(self.a);
        return res;
    }
    fn op_rla(self: *CPU, val: u8) u8 {
        const carry_in: u8 = if (self.p.carry) 1 else 0;
        self.p.carry = (val & 0x80) != 0;
        const res = (val << 1) | carry_in;
        self.a &= res;
        self.set_nz_flags(self.a);
        return res;
    }
    fn op_rra(self: *CPU, val: u8) u8 {
        const carry_in: u8 = if (self.p.carry) 0x80 else 0;
        self.p.carry = (val & 1) != 0;
        const res = (val >> 1) | carry_in;
        self.add_with_carry(res);
        return res;
    }
    fn op_sre(self: *CPU, val: u8) u8 {
        self.p.carry = (val & 1) != 0;
        const res = val >> 1;
        self.a ^= res;
        self.set_nz_flags(self.a);
        return res;
    }
    fn op_asl(self: *CPU, val: u8) u8 {
        self.p.carry = (val & 0x80) != 0;
        const res = val << 1;
        self.set_nz_flags(res);
        return res;
    }
    fn op_lsr(self: *CPU, val: u8) u8 {
        self.p.carry = (val & 0x01) != 0;
        const res = val >> 1;
        self.set_nz_flags(res);
        return res;
    }
    fn op_rol(self: *CPU, val: u8) u8 {
        const carry_in: u8 = if (self.p.carry) 1 else 0;
        self.p.carry = (val & 0x80) != 0;
        const res = (val << 1) | carry_in;
        self.set_nz_flags(res);
        return res;
    }
    fn op_ror(self: *CPU, val: u8) u8 {
        const carry_in: u8 = if (self.p.carry) 0x80 else 0;
        self.p.carry = (val & 0x01) != 0;
        const res = (val >> 1) | carry_in;
        self.set_nz_flags(res);
        return res;
    }
    fn op_isc(self: *CPU, val: u8) u8 {
        const res = val +% 1;
        self.add_with_carry(res ^ 0xff);
        return res;
    }
    fn op_dcp(self: *CPU, val: u8) u8 {
        const res = val -% 1;
        self.p.carry = self.a >= res;
        self.set_nz_flags(self.a -% res);
        return res;
    }
    fn op_inc(self: *CPU, val: u8) u8 {
        const res = val +% 1;
        self.set_nz_flags(res);
        return res;
    }
    fn op_dec(self: *CPU, val: u8) u8 {
        const res = val -% 1;
        self.set_nz_flags(res);
        return res;
    }

    // ─── Addressing mode templates ──────────────────────────────────────────

    // Immediate: 2 cycles
    // step 1: read operand, execute, done
    fn immediate(comptime op: fn (*CPU, u8) void) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        const val = self.read(self.pc);
                        self.pc +%= 1;
                        op(self, val);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Zero Page Read: 3 cycles
    fn zp_read(comptime op: fn (*CPU, u8) void) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.addr = self.read(self.pc);
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        const val = self.read(self.addr & 0xFF);
                        op(self, val);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Zero Page Write: 3 cycles
    fn zp_write(comptime get_val: fn (*CPU) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.addr = self.read(self.pc);
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        self.write(self.addr & 0xFF, get_val(self));
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Zero Page RMW: 5 cycles
    fn zp_rmw(comptime op: fn (*CPU, u8) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.addr = self.read(self.pc);
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        self.data = self.read(self.addr & 0xFF);
                        self.step = 3;
                    },
                    3 => {
                        // Dummy write of old value
                        self.write(self.addr & 0xFF, self.data);
                        self.data = op(self, self.data);
                        self.step = 4;
                    },
                    4 => {
                        self.write(self.addr & 0xFF, self.data);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Zero Page,X Read: 4 cycles
    fn zpx_read(comptime op: fn (*CPU, u8) void) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.ptr = @truncate(self.read(self.pc));
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        // Dummy read at unindexed zp addr
                        _ = self.read(self.ptr);
                        self.addr = self.ptr +% self.x;
                        self.step = 3;
                    },
                    3 => {
                        const val = self.read(self.addr & 0xFF);
                        op(self, val);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Zero Page,Y Read: 4 cycles
    fn zpy_read(comptime op: fn (*CPU, u8) void) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.ptr = @truncate(self.read(self.pc));
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        _ = self.read(self.ptr);
                        self.addr = self.ptr +% self.y;
                        self.step = 3;
                    },
                    3 => {
                        const val = self.read(self.addr & 0xFF);
                        op(self, val);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Zero Page,X Write: 4 cycles
    fn zpx_write(comptime get_val: fn (*CPU) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.ptr = @truncate(self.read(self.pc));
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        _ = self.read(self.ptr);
                        self.addr = self.ptr +% self.x;
                        self.step = 3;
                    },
                    3 => {
                        self.write(self.addr & 0xFF, get_val(self));
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Zero Page,Y Write: 4 cycles
    fn zpy_write(comptime get_val: fn (*CPU) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.ptr = @truncate(self.read(self.pc));
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        _ = self.read(self.ptr);
                        self.addr = self.ptr +% self.y;
                        self.step = 3;
                    },
                    3 => {
                        self.write(self.addr & 0xFF, get_val(self));
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Zero Page,X RMW: 6 cycles
    fn zpx_rmw(comptime op: fn (*CPU, u8) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.ptr = @truncate(self.read(self.pc));
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        _ = self.read(self.ptr);
                        self.addr = self.ptr +% self.x;
                        self.step = 3;
                    },
                    3 => {
                        self.data = self.read(self.addr & 0xFF);
                        self.step = 4;
                    },
                    4 => {
                        self.write(self.addr & 0xFF, self.data);
                        self.data = op(self, self.data);
                        self.step = 5;
                    },
                    5 => {
                        self.write(self.addr & 0xFF, self.data);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Absolute Read: 4 cycles
    fn abs_read(comptime op: fn (*CPU, u8) void) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.addr = self.read(self.pc);
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        self.addr |= @as(u16, self.read(self.pc)) << 8;
                        self.pc +%= 1;
                        self.step = 3;
                    },
                    3 => {
                        const val = self.read(self.addr);
                        op(self, val);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Absolute Write: 4 cycles
    fn abs_write(comptime get_val: fn (*CPU) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.addr = self.read(self.pc);
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        self.addr |= @as(u16, self.read(self.pc)) << 8;
                        self.pc +%= 1;
                        self.step = 3;
                    },
                    3 => {
                        self.write(self.addr, get_val(self));
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Absolute RMW: 6 cycles
    fn abs_rmw(comptime op: fn (*CPU, u8) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.addr = self.read(self.pc);
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        self.addr |= @as(u16, self.read(self.pc)) << 8;
                        self.pc +%= 1;
                        self.step = 3;
                    },
                    3 => {
                        self.data = self.read(self.addr);
                        self.step = 4;
                    },
                    4 => {
                        self.write(self.addr, self.data);
                        self.data = op(self, self.data);
                        self.step = 5;
                    },
                    5 => {
                        self.write(self.addr, self.data);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Absolute,X Read: 4-5 cycles (page cross penalty)
    fn abx_read(comptime op: fn (*CPU, u8) void) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.addr = self.read(self.pc);
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        const hi = self.read(self.pc);
                        self.pc +%= 1;
                        const lo: u8 = @truncate(self.addr);
                        _, const page_cross = @addWithOverflow(lo, self.x);
                        self.addr = (@as(u16, hi) << 8) | @as(u16, lo);
                        self.addr +%= @as(u16, self.x);
                        self.data = page_cross;
                        self.step = 3;
                    },
                    3 => {
                        if (self.data == 0) {
                            const val = self.read(self.addr);
                            op(self, val);
                            self.step = 0;
                        } else {
                            _ = self.read(self.addr -% 0x100);
                            self.step = 4;
                        }
                    },
                    4 => {
                        const val = self.read(self.addr);
                        op(self, val);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Absolute,Y Read: 4-5 cycles (page cross penalty)
    fn aby_read(comptime op: fn (*CPU, u8) void) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.addr = self.read(self.pc);
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        const hi = self.read(self.pc);
                        self.pc +%= 1;
                        const lo: u8 = @truncate(self.addr);
                        _, const page_cross = @addWithOverflow(lo, self.y);
                        self.addr = (@as(u16, hi) << 8) | @as(u16, lo);
                        self.addr +%= @as(u16, self.y);
                        self.data = page_cross;
                        self.step = 3;
                    },
                    3 => {
                        if (self.data == 0) {
                            const val = self.read(self.addr);
                            op(self, val);
                            self.step = 0;
                        } else {
                            _ = self.read(self.addr -% 0x100);
                            self.step = 4;
                        }
                    },
                    4 => {
                        const val = self.read(self.addr);
                        op(self, val);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Absolute,X Write: 5 cycles (always extra cycle)
    fn abx_write(comptime get_val: fn (*CPU) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.addr = self.read(self.pc);
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        const hi = self.read(self.pc);
                        self.pc +%= 1;
                        const lo: u8 = @truncate(self.addr);
                        self.addr = (@as(u16, hi) << 8) | @as(u16, lo);
                        self.addr +%= @as(u16, self.x);
                        self.step = 3;
                    },
                    3 => {
                        // Dummy read (always for writes)
                        _ = self.read(self.addr);
                        self.step = 4;
                    },
                    4 => {
                        self.write(self.addr, get_val(self));
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Absolute,Y Write: 5 cycles (always extra cycle)
    fn aby_write(comptime get_val: fn (*CPU) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.addr = self.read(self.pc);
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        const hi = self.read(self.pc);
                        self.pc +%= 1;
                        const lo: u8 = @truncate(self.addr);
                        self.addr = (@as(u16, hi) << 8) | @as(u16, lo);
                        self.addr +%= @as(u16, self.y);
                        self.step = 3;
                    },
                    3 => {
                        _ = self.read(self.addr);
                        self.step = 4;
                    },
                    4 => {
                        self.write(self.addr, get_val(self));
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Absolute,X RMW: 7 cycles
    fn abx_rmw(comptime op: fn (*CPU, u8) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.addr = self.read(self.pc);
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        const hi = self.read(self.pc);
                        self.pc +%= 1;
                        const lo: u8 = @truncate(self.addr);
                        self.addr = (@as(u16, hi) << 8) | @as(u16, lo);
                        self.addr +%= @as(u16, self.x);
                        self.step = 3;
                    },
                    3 => {
                        // Dummy read at effective address
                        _ = self.read(self.addr);
                        self.step = 4;
                    },
                    4 => {
                        self.data = self.read(self.addr);
                        self.step = 5;
                    },
                    5 => {
                        self.write(self.addr, self.data);
                        self.data = op(self, self.data);
                        self.step = 6;
                    },
                    6 => {
                        self.write(self.addr, self.data);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Absolute,Y RMW: 7 cycles (always extra cycle)
    fn aby_rmw(comptime op: fn (*CPU, u8) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.addr = self.read(self.pc);
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        const hi = self.read(self.pc);
                        self.pc +%= 1;
                        const lo: u8 = @truncate(self.addr);
                        self.addr = (@as(u16, hi) << 8) | @as(u16, lo);
                        self.addr +%= @as(u16, self.y);
                        self.step = 3;
                    },
                    3 => {
                        _ = self.read(self.addr);
                        self.step = 4;
                    },
                    4 => {
                        self.data = self.read(self.addr);
                        self.step = 5;
                    },
                    5 => {
                        self.write(self.addr, self.data);
                        self.data = op(self, self.data);
                        self.step = 6;
                    },
                    6 => {
                        self.write(self.addr, self.data);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // (Indirect,X) RMW: 8 cycles
    fn izx_rmw(comptime op: fn (*CPU, u8) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.ptr = @truncate(self.read(self.pc));
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        _ = self.read(self.ptr);
                        self.ptr +%= self.x;
                        self.step = 3;
                    },
                    3 => {
                        self.addr = self.read(self.ptr);
                        self.step = 4;
                    },
                    4 => {
                        self.addr |= @as(u16, self.read(self.ptr +% 1)) << 8;
                        self.step = 5;
                    },
                    5 => {
                        self.data = self.read(self.addr);
                        self.step = 6;
                    },
                    6 => {
                        self.write(self.addr, self.data);
                        self.data = op(self, self.data);
                        self.step = 7;
                    },
                    7 => {
                        self.write(self.addr, self.data);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // (Indirect),Y RMW: 8 cycles (always extra cycle)
    fn izy_rmw(comptime op: fn (*CPU, u8) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.ptr = @truncate(self.read(self.pc));
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        self.addr = self.read(self.ptr);
                        self.step = 3;
                    },
                    3 => {
                        const hi = self.read(self.ptr +% 1);
                        const lo: u8 = @truncate(self.addr);
                        self.addr = (@as(u16, hi) << 8) | @as(u16, lo);
                        self.addr +%= @as(u16, self.y);
                        self.step = 4;
                    },
                    4 => {
                        _ = self.read(self.addr);
                        self.step = 5;
                    },
                    5 => {
                        self.data = self.read(self.addr);
                        self.step = 6;
                    },
                    6 => {
                        self.write(self.addr, self.data);
                        self.data = op(self, self.data);
                        self.step = 7;
                    },
                    7 => {
                        self.write(self.addr, self.data);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // (Indirect,X) Read: 6 cycles
    fn izx_read(comptime op: fn (*CPU, u8) void) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.ptr = @truncate(self.read(self.pc));
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        // Dummy read, add X
                        _ = self.read(self.ptr);
                        self.ptr +%= self.x;
                        self.step = 3;
                    },
                    3 => {
                        self.addr = self.read(self.ptr);
                        self.step = 4;
                    },
                    4 => {
                        self.addr |= @as(u16, self.read(self.ptr +% 1)) << 8;
                        self.step = 5;
                    },
                    5 => {
                        const val = self.read(self.addr);
                        op(self, val);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // (Indirect,X) Write: 6 cycles
    fn izx_write(comptime get_val: fn (*CPU) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.ptr = @truncate(self.read(self.pc));
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        _ = self.read(self.ptr);
                        self.ptr +%= self.x;
                        self.step = 3;
                    },
                    3 => {
                        self.addr = self.read(self.ptr);
                        self.step = 4;
                    },
                    4 => {
                        self.addr |= @as(u16, self.read(self.ptr +% 1)) << 8;
                        self.step = 5;
                    },
                    5 => {
                        self.write(self.addr, get_val(self));
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // (Indirect),Y Read: 5-6 cycles
    fn izy_read(comptime op: fn (*CPU, u8) void) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.ptr = @truncate(self.read(self.pc));
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        self.addr = self.read(self.ptr);
                        self.step = 3;
                    },
                    3 => {
                        const hi = self.read(self.ptr +% 1);
                        const lo: u8 = @truncate(self.addr);
                        _, const page_cross = @addWithOverflow(lo, self.y);
                        self.addr = (@as(u16, hi) << 8) | @as(u16, lo);
                        self.addr +%= @as(u16, self.y);
                        self.data = page_cross;
                        self.step = 4;
                    },
                    4 => {
                        if (self.data == 0) {
                            const val = self.read(self.addr);
                            op(self, val);
                            self.step = 0;
                        } else {
                            _ = self.read(self.addr -% 0x100);
                            self.step = 5;
                        }
                    },
                    5 => {
                        const val = self.read(self.addr);
                        op(self, val);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // (Indirect),Y Write: 6 cycles (always extra cycle)
    fn izy_write(comptime get_val: fn (*CPU) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.ptr = @truncate(self.read(self.pc));
                        self.pc +%= 1;
                        self.step = 2;
                    },
                    2 => {
                        self.addr = self.read(self.ptr);
                        self.step = 3;
                    },
                    3 => {
                        const hi = self.read(self.ptr +% 1);
                        const lo: u8 = @truncate(self.addr);
                        self.addr = (@as(u16, hi) << 8) | @as(u16, lo);
                        self.addr +%= @as(u16, self.y);
                        self.step = 4;
                    },
                    4 => {
                        // Dummy read (always for writes)
                        _ = self.read(self.addr);
                        self.step = 5;
                    },
                    5 => {
                        self.write(self.addr, get_val(self));
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // ─── Implied / Accumulator operations ───────────────────────────────────

    // Implied: 2 cycles
    fn implied(comptime op: fn (*CPU) void) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        // Dummy read at PC
                        _ = self.read(self.pc);
                        op(self);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // Accumulator RMW: 2 cycles
    fn acc_rmw(comptime op: fn (*CPU, u8) u8) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        _ = self.read(self.pc);
                        self.a = op(self, self.a);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    // ─── Value getters for write operations ─────────────────────────────────
    fn get_ax(self: *CPU) u8 {
        return self.a & self.x;
    }
    fn get_a(self: *CPU) u8 {
        return self.a;
    }
    fn get_x(self: *CPU) u8 {
        return self.x;
    }
    fn get_y(self: *CPU) u8 {
        return self.y;
    }

    // ─── Implied operation bodies ───────────────────────────────────────────
    fn impl_tax(self: *CPU) void {
        self.x = self.a;
        self.set_nz_flags(self.x);
    }
    fn impl_tay(self: *CPU) void {
        self.y = self.a;
        self.set_nz_flags(self.y);
    }
    fn impl_txa(self: *CPU) void {
        self.a = self.x;
        self.set_nz_flags(self.a);
    }
    fn impl_tya(self: *CPU) void {
        self.a = self.y;
        self.set_nz_flags(self.a);
    }
    fn impl_tsx(self: *CPU) void {
        self.x = self.sp;
        self.set_nz_flags(self.x);
    }
    fn impl_txs(self: *CPU) void {
        self.sp = self.x;
    }
    fn impl_inx(self: *CPU) void {
        self.x +%= 1;
        self.set_nz_flags(self.x);
    }
    fn impl_iny(self: *CPU) void {
        self.y +%= 1;
        self.set_nz_flags(self.y);
    }
    fn impl_dex(self: *CPU) void {
        self.x -%= 1;
        self.set_nz_flags(self.x);
    }
    fn impl_dey(self: *CPU) void {
        self.y -%= 1;
        self.set_nz_flags(self.y);
    }
    fn impl_clc(self: *CPU) void {
        self.p.carry = false;
    }
    fn impl_sec(self: *CPU) void {
        self.p.carry = true;
    }
    fn impl_cli(self: *CPU) void {
        self.p.interrupt_disable = false;
    }
    fn impl_sei(self: *CPU) void {
        self.p.interrupt_disable = true;
    }
    fn impl_cld(self: *CPU) void {
        self.p.decimal_mode = false;
    }
    fn impl_sed(self: *CPU) void {
        self.p.decimal_mode = true;
    }
    fn impl_clv(self: *CPU) void {
        self.p.overflow = false;
    }
    fn impl_nop(_: *CPU) void {}

    // ─── Special instructions (manually stepped) ────────────────────────────

    fn exec_brk(self: *CPU) void {
        switch (self.step) {
            1 => {
                // Read and discard next byte, increment PC
                _ = self.read(self.pc);
                self.pc +%= 1;
                self.step = 2;
            },
            2 => {
                self.write(0x0100 | @as(u16, self.sp), @truncate(self.pc >> 8));
                self.sp -%= 1;
                self.step = 3;
            },
            3 => {
                self.write(0x0100 | @as(u16, self.sp), @truncate(self.pc));
                self.sp -%= 1;
                self.step = 4;
            },
            4 => {
                var flags = self.p;
                flags.break_command = true;
                flags._padding = 1;
                self.write(0x0100 | @as(u16, self.sp), @bitCast(flags));
                self.sp -%= 1;
                self.p.interrupt_disable = true;
                self.step = 5;
            },
            5 => {
                self.addr = self.read(0xFFFE);
                self.step = 6;
            },
            6 => {
                self.pc = (@as(u16, self.read(0xFFFF)) << 8) | (self.addr & 0xFF);
                self.step = 0;
            },
            else => unreachable,
        }
    }

    fn exec_rti(self: *CPU) void {
        switch (self.step) {
            1 => {
                // Dummy read
                _ = self.read(self.pc);
                self.step = 2;
            },
            2 => {
                // Dummy read from stack (increment SP)
                self.sp +%= 1;
                _ = self.read(0x0100 | @as(u16, self.sp));
                self.step = 3;
            },
            3 => {
                self.p = @bitCast(self.read(0x0100 | @as(u16, self.sp)));
                self.p.break_command = false;
                self.p._padding = 1;
                self.sp +%= 1;
                self.step = 4;
            },
            4 => {
                self.addr = self.read(0x0100 | @as(u16, self.sp));
                self.sp +%= 1;
                self.step = 5;
            },
            5 => {
                self.pc = (@as(u16, self.read(0x0100 | @as(u16, self.sp))) << 8) | (self.addr & 0xFF);
                self.step = 0;
            },
            else => unreachable,
        }
    }

    fn exec_rts(self: *CPU) void {
        switch (self.step) {
            1 => {
                // Dummy read
                _ = self.read(self.pc);
                self.step = 2;
            },
            2 => {
                // Dummy read from stack
                self.sp +%= 1;
                _ = self.read(0x0100 | @as(u16, self.sp));
                self.step = 3;
            },
            3 => {
                self.addr = self.read(0x0100 | @as(u16, self.sp));
                self.sp +%= 1;
                self.step = 4;
            },
            4 => {
                self.pc = (@as(u16, self.read(0x0100 | @as(u16, self.sp))) << 8) | (self.addr & 0xFF);
                self.step = 5;
            },
            5 => {
                // Increment PC (JSR pushed PC-1)
                _ = self.read(self.pc);
                self.pc +%= 1;
                self.step = 0;
            },
            else => unreachable,
        }
    }

    fn exec_jsr(self: *CPU) void {
        switch (self.step) {
            1 => {
                self.addr = self.read(self.pc);
                self.pc +%= 1;
                self.step = 2;
            },
            2 => {
                // "Internal operation" — dummy read from stack
                _ = self.read(0x0100 | @as(u16, self.sp));
                self.step = 3;
            },
            3 => {
                self.write(0x0100 | @as(u16, self.sp), @truncate(self.pc >> 8));
                self.sp -%= 1;
                self.step = 4;
            },
            4 => {
                self.write(0x0100 | @as(u16, self.sp), @truncate(self.pc));
                self.sp -%= 1;
                self.step = 5;
            },
            5 => {
                self.pc = (@as(u16, self.read(self.pc)) << 8) | (self.addr & 0xFF);
                self.step = 0;
            },
            else => unreachable,
        }
    }

    fn exec_jmp_abs(self: *CPU) void {
        switch (self.step) {
            1 => {
                self.addr = self.read(self.pc);
                self.pc +%= 1;
                self.step = 2;
            },
            2 => {
                self.pc = (@as(u16, self.read(self.pc)) << 8) | (self.addr & 0xFF);
                self.step = 0;
            },
            else => unreachable,
        }
    }

    fn exec_jmp_ind(self: *CPU) void {
        switch (self.step) {
            1 => {
                self.addr = self.read(self.pc);
                self.pc +%= 1;
                self.step = 2;
            },
            2 => {
                self.addr |= @as(u16, self.read(self.pc)) << 8;
                self.pc +%= 1;
                self.step = 3;
            },
            3 => {
                self.data = self.read(self.addr);
                self.step = 4;
            },
            4 => {
                // 6502 page boundary bug
                const hi_addr = if (self.addr & 0x00FF == 0x00FF)
                    self.addr & 0xFF00
                else
                    self.addr + 1;
                self.pc = (@as(u16, self.read(hi_addr)) << 8) | @as(u16, self.data);
                self.step = 0;
            },
            else => unreachable,
        }
    }

    fn exec_pha(self: *CPU) void {
        switch (self.step) {
            1 => {
                // Dummy read
                _ = self.read(self.pc);
                self.step = 2;
            },
            2 => {
                self.write(0x0100 | @as(u16, self.sp), self.a);
                self.sp -%= 1;
                self.step = 0;
            },
            else => unreachable,
        }
    }

    fn exec_php(self: *CPU) void {
        switch (self.step) {
            1 => {
                _ = self.read(self.pc);
                self.step = 2;
            },
            2 => {
                var flags = self.p;
                flags.break_command = true;
                flags._padding = 1;
                self.write(0x0100 | @as(u16, self.sp), @bitCast(flags));
                self.sp -%= 1;
                self.step = 0;
            },
            else => unreachable,
        }
    }

    fn exec_pla(self: *CPU) void {
        switch (self.step) {
            1 => {
                // Dummy read
                _ = self.read(self.pc);
                self.step = 2;
            },
            2 => {
                // Increment SP
                self.sp +%= 1;
                _ = self.read(0x0100 | @as(u16, self.sp));
                self.step = 3;
            },
            3 => {
                self.a = self.read(0x0100 | @as(u16, self.sp));
                self.set_nz_flags(self.a);
                self.step = 0;
            },
            else => unreachable,
        }
    }

    fn exec_plp(self: *CPU) void {
        switch (self.step) {
            1 => {
                _ = self.read(self.pc);
                self.step = 2;
            },
            2 => {
                self.sp +%= 1;
                _ = self.read(0x0100 | @as(u16, self.sp));
                self.step = 3;
            },
            3 => {
                self.p = @bitCast(self.read(0x0100 | @as(u16, self.sp)));
                self.p.break_command = false;
                self.p._padding = 1;
                self.step = 0;
            },
            else => unreachable,
        }
    }

    // Branch: 2-4 cycles
    fn branch(comptime cond: fn (*CPU) bool) fn (*CPU) void {
        return struct {
            fn handler(self: *CPU) void {
                switch (self.step) {
                    1 => {
                        self.data = self.read(self.pc);
                        self.pc +%= 1;
                        if (!cond(self)) {
                            self.step = 0; // Not taken: 2 cycles total
                        } else {
                            self.step = 2;
                        }
                    },
                    2 => {
                        // Dummy read at current PC
                        _ = self.read(self.pc);
                        const offset: i8 = @bitCast(self.data);
                        const old_pc = self.pc;
                        if (offset >= 0) {
                            self.pc +%= @abs(offset);
                        } else {
                            self.pc -%= @abs(offset);
                        }
                        // Check page cross
                        if ((old_pc ^ self.pc) & 0xFF00 != 0) {
                            self.step = 3; // Page crossed: 4 cycles
                        } else {
                            self.step = 0; // No page cross: 3 cycles
                        }
                    },
                    3 => {
                        // Dummy read for page cross fixup
                        _ = self.read(self.pc);
                        self.step = 0;
                    },
                    else => unreachable,
                }
            }
        }.handler;
    }

    fn cond_bcc(self: *CPU) bool {
        return !self.p.carry;
    }
    fn cond_bcs(self: *CPU) bool {
        return self.p.carry;
    }
    fn cond_beq(self: *CPU) bool {
        return self.p.zero;
    }
    fn cond_bne(self: *CPU) bool {
        return !self.p.zero;
    }
    fn cond_bmi(self: *CPU) bool {
        return self.p.negative;
    }
    fn cond_bpl(self: *CPU) bool {
        return !self.p.negative;
    }
    fn cond_bvs(self: *CPU) bool {
        return self.p.overflow;
    }
    fn cond_bvc(self: *CPU) bool {
        return !self.p.overflow;
    }

    // NOP immediate (DOP): 2 cycles - reads and discards a byte
    fn exec_nop_imm(self: *CPU) void {
        switch (self.step) {
            1 => {
                _ = self.read(self.pc);
                self.pc +%= 1;
                self.step = 0;
            },
            else => unreachable,
        }
    }

    // NOP zero page (DOP z): 3 cycles
    fn exec_nop_zp(self: *CPU) void {
        switch (self.step) {
            1 => {
                self.addr = self.read(self.pc);
                self.pc +%= 1;
                self.step = 2;
            },
            2 => {
                _ = self.read(self.addr & 0xFF);
                self.step = 0;
            },
            else => unreachable,
        }
    }

    // NOP zpx (DOP z,X): 4 cycles
    fn exec_nop_zpx(self: *CPU) void {
        switch (self.step) {
            1 => {
                self.ptr = @truncate(self.read(self.pc));
                self.pc +%= 1;
                self.step = 2;
            },
            2 => {
                _ = self.read(self.ptr);
                self.addr = self.ptr +% self.x;
                self.step = 3;
            },
            3 => {
                _ = self.read(self.addr & 0xFF);
                self.step = 0;
            },
            else => unreachable,
        }
    }

    // NOP absolute (TOP abs): 4 cycles
    fn exec_nop_abs(self: *CPU) void {
        switch (self.step) {
            1 => {
                self.addr = self.read(self.pc);
                self.pc +%= 1;
                self.step = 2;
            },
            2 => {
                self.addr |= @as(u16, self.read(self.pc)) << 8;
                self.pc +%= 1;
                self.step = 3;
            },
            3 => {
                _ = self.read(self.addr);
                self.step = 0;
            },
            else => unreachable,
        }
    }

    // NOP abs,X (TOP abs,X): 4-5 cycles (page cross)
    fn exec_nop_abx(self: *CPU) void {
        switch (self.step) {
            1 => {
                self.addr = self.read(self.pc);
                self.pc +%= 1;
                self.step = 2;
            },
            2 => {
                const hi = self.read(self.pc);
                self.pc +%= 1;
                const lo: u8 = @truncate(self.addr);
                _, const page_cross = @addWithOverflow(lo, self.x);
                self.addr = (@as(u16, hi) << 8) | @as(u16, lo);
                self.addr +%= @as(u16, self.x);
                self.data = page_cross;
                self.step = 3;
            },
            3 => {
                if (self.data == 0) {
                    _ = self.read(self.addr);
                    self.step = 0;
                } else {
                    _ = self.read(self.addr -% 0x100);
                    self.step = 4;
                }
            },
            4 => {
                _ = self.read(self.addr);
                self.step = 0;
            },
            else => unreachable,
        }
    }

    // Unofficial opcode: NOP implied but reads next byte (0xC2 etc.)
    // Actually 0xC2 is DOP #n (2-byte NOP)
    fn exec_unofficial_nop(self: *CPU) void {
        switch (self.step) {
            1 => {
                _ = self.read(self.pc);
                self.step = 0;
            },
            else => unreachable,
        }
    }

    // SYA/SHY: abs,X write, val = Y & (high_byte + 1), page-cross glitch
    fn exec_sya(self: *CPU) void {
        switch (self.step) {
            1 => {
                self.addr = self.read(self.pc);
                self.pc +%= 1;
                self.step = 2;
            },
            2 => {
                self.data2 = self.read(self.pc);
                self.pc +%= 1;
                const lo: u8 = @truncate(self.addr);
                _, const page_cross = @addWithOverflow(lo, self.x);
                self.addr = (@as(u16, self.data2) << 8) | @as(u16, lo);
                self.addr +%= @as(u16, self.x);
                self.data = page_cross;
                self.step = 3;
            },
            3 => {
                _ = self.read(self.addr -% if (self.data != 0) @as(u16, 0x100) else @as(u16, 0));
                const val = self.y & (self.data2 +% 1);
                if (self.data != 0) {
                    self.addr = (@as(u16, val) << 8) | (self.addr & 0xFF);
                }
                self.data = val;
                self.step = 4;
            },
            4 => {
                self.write(self.addr, self.data);
                self.step = 0;
            },
            else => unreachable,
        }
    }

    // SXA/SHX: abs,Y write, val = X & (high_byte + 1), page-cross glitch
    fn exec_sxa(self: *CPU) void {
        switch (self.step) {
            1 => {
                self.addr = self.read(self.pc);
                self.pc +%= 1;
                self.step = 2;
            },
            2 => {
                self.data2 = self.read(self.pc);
                self.pc +%= 1;
                const lo: u8 = @truncate(self.addr);
                _, const page_cross = @addWithOverflow(lo, self.y);
                self.addr = (@as(u16, self.data2) << 8) | @as(u16, lo);
                self.addr +%= @as(u16, self.y);
                self.data = page_cross;
                self.step = 3;
            },
            3 => {
                _ = self.read(self.addr -% if (self.data != 0) @as(u16, 0x100) else @as(u16, 0));
                const val = self.x & (self.data2 +% 1);
                if (self.data != 0) {
                    self.addr = (@as(u16, val) << 8) | (self.addr & 0xFF);
                }
                self.data = val;
                self.step = 4;
            },
            4 => {
                self.write(self.addr, self.data);
                self.step = 0;
            },
            else => unreachable,
        }
    }

    // ─── Opcode dispatch table ──────────────────────────────────────────────

    const opcode_table: [256]*const fn (*CPU) void = init_opcode_table();

    fn init_opcode_table() [256]*const fn (*CPU) void {
        var table: [256]*const fn (*CPU) void = undefined;
        // Initialize all to unimplemented
        for (0..256) |i| {
            table[i] = &exec_unimplemented;
        }

        // BRK
        table[0x00] = &exec_brk;

        // ORA
        table[0x09] = &immediate(op_ora);
        table[0x05] = &zp_read(op_ora);
        table[0x15] = &zpx_read(op_ora);
        table[0x0d] = &abs_read(op_ora);
        table[0x1d] = &abx_read(op_ora);
        table[0x19] = &aby_read(op_ora);
        table[0x01] = &izx_read(op_ora);
        table[0x11] = &izy_read(op_ora);

        // AND
        table[0x29] = &immediate(op_and);
        table[0x25] = &zp_read(op_and);
        table[0x35] = &zpx_read(op_and);
        table[0x2d] = &abs_read(op_and);
        table[0x3d] = &abx_read(op_and);
        table[0x39] = &aby_read(op_and);
        table[0x21] = &izx_read(op_and);
        table[0x31] = &izy_read(op_and);

        // EOR
        table[0x49] = &immediate(op_eor);
        table[0x45] = &zp_read(op_eor);
        table[0x55] = &zpx_read(op_eor);
        table[0x4d] = &abs_read(op_eor);
        table[0x5d] = &abx_read(op_eor);
        table[0x59] = &aby_read(op_eor);
        table[0x41] = &izx_read(op_eor);
        table[0x51] = &izy_read(op_eor);

        // ADC
        table[0x69] = &immediate(op_adc);
        table[0x65] = &zp_read(op_adc);
        table[0x75] = &zpx_read(op_adc);
        table[0x6d] = &abs_read(op_adc);
        table[0x7d] = &abx_read(op_adc);
        table[0x79] = &aby_read(op_adc);
        table[0x61] = &izx_read(op_adc);
        table[0x71] = &izy_read(op_adc);

        // SBC
        table[0xe9] = &immediate(op_sbc);
        table[0xe5] = &zp_read(op_sbc);
        table[0xf5] = &zpx_read(op_sbc);
        table[0xed] = &abs_read(op_sbc);
        table[0xfd] = &abx_read(op_sbc);
        table[0xf9] = &aby_read(op_sbc);
        table[0xe1] = &izx_read(op_sbc);
        table[0xf1] = &izy_read(op_sbc);
        table[0xeb] = &immediate(op_sbc); // Unofficial SBC #n

        // ANC/AAC (AND #imm, copy bit 7 to carry)
        table[0x0b] = &immediate(op_anc);
        table[0x2b] = &immediate(op_anc);

        // ALR/ASR (AND #imm, then LSR A)
        table[0x4b] = &immediate(op_alr);

        // RLA (ROL mem, then AND into A)
        table[0x27] = &zp_rmw(op_rla);
        table[0x37] = &zpx_rmw(op_rla);
        table[0x2f] = &abs_rmw(op_rla);
        table[0x3f] = &abx_rmw(op_rla);
        table[0x3b] = &aby_rmw(op_rla);
        table[0x23] = &izx_rmw(op_rla);
        table[0x33] = &izy_rmw(op_rla);

        // RRA (ROR mem, then ADC into A)
        table[0x67] = &zp_rmw(op_rra);
        table[0x77] = &zpx_rmw(op_rra);
        table[0x6f] = &abs_rmw(op_rra);
        table[0x7f] = &abx_rmw(op_rra);
        table[0x7b] = &aby_rmw(op_rra);
        table[0x63] = &izx_rmw(op_rra);
        table[0x73] = &izy_rmw(op_rra);

        // SRE (LSR mem, then EOR into A)
        table[0x47] = &zp_rmw(op_sre);
        table[0x57] = &zpx_rmw(op_sre);
        table[0x4f] = &abs_rmw(op_sre);
        table[0x5f] = &abx_rmw(op_sre);
        table[0x5b] = &aby_rmw(op_sre);
        table[0x43] = &izx_rmw(op_sre);
        table[0x53] = &izy_rmw(op_sre);

        // SLO (ASL mem, then ORA into A)
        table[0x07] = &zp_rmw(op_slo);
        table[0x17] = &zpx_rmw(op_slo);
        table[0x0f] = &abs_rmw(op_slo);
        table[0x1f] = &abx_rmw(op_slo);
        table[0x1b] = &aby_rmw(op_slo);
        table[0x03] = &izx_rmw(op_slo);
        table[0x13] = &izy_rmw(op_slo);

        // AXS/SBX (X = (A & X) - imm)
        table[0xcb] = &immediate(op_axs);

        // ATX/LXA (AND #imm, copy A to X)
        table[0xab] = &immediate(op_atx);

        // ARR (AND #imm, then ROR A, special C/V)
        table[0x6b] = &immediate(op_arr);

        // ISC (INC mem, then SBC from A)
        table[0xe7] = &zp_rmw(op_isc);
        table[0xf7] = &zpx_rmw(op_isc);
        table[0xef] = &abs_rmw(op_isc);
        table[0xff] = &abx_rmw(op_isc);
        table[0xfb] = &aby_rmw(op_isc);
        table[0xe3] = &izx_rmw(op_isc);
        table[0xf3] = &izy_rmw(op_isc);

        // DCP (DEC mem, then CMP with A)
        table[0xc7] = &zp_rmw(op_dcp);
        table[0xd7] = &zpx_rmw(op_dcp);
        table[0xcf] = &abs_rmw(op_dcp);
        table[0xdf] = &abx_rmw(op_dcp);
        table[0xdb] = &aby_rmw(op_dcp);
        table[0xc3] = &izx_rmw(op_dcp);
        table[0xd3] = &izy_rmw(op_dcp);

        // SAX/AAX (A & X -> memory)
        table[0x87] = &zp_write(get_ax);
        table[0x97] = &zpy_write(get_ax);
        table[0x8f] = &abs_write(get_ax);
        table[0x83] = &izx_write(get_ax);

        // SYA/SHY (Y & (H+1) -> abs,X)
        table[0x9c] = &exec_sya;

        // LAX (load A and X from memory)
        table[0xa7] = &zp_read(op_lax);
        table[0xb7] = &zpy_read(op_lax);
        table[0xaf] = &abs_read(op_lax);
        table[0xbf] = &aby_read(op_lax);
        table[0xa3] = &izx_read(op_lax);
        table[0xb3] = &izy_read(op_lax);

        // SXA/SHX (X & (H+1) -> abs,Y)
        table[0x9e] = &exec_sxa;

        // CMP
        table[0xc9] = &immediate(op_cmp);
        table[0xc5] = &zp_read(op_cmp);
        table[0xd5] = &zpx_read(op_cmp);
        table[0xcd] = &abs_read(op_cmp);
        table[0xdd] = &abx_read(op_cmp);
        table[0xd9] = &aby_read(op_cmp);
        table[0xc1] = &izx_read(op_cmp);
        table[0xd1] = &izy_read(op_cmp);

        // CPX
        table[0xe0] = &immediate(op_cpx);
        table[0xe4] = &zp_read(op_cpx);
        table[0xec] = &abs_read(op_cpx);

        // CPY
        table[0xc0] = &immediate(op_cpy);
        table[0xc4] = &zp_read(op_cpy);
        table[0xcc] = &abs_read(op_cpy);

        // BIT
        table[0x24] = &zp_read(op_bit);
        table[0x2c] = &abs_read(op_bit);

        // LDA
        table[0xa9] = &immediate(op_lda);
        table[0xa5] = &zp_read(op_lda);
        table[0xb5] = &zpx_read(op_lda);
        table[0xad] = &abs_read(op_lda);
        table[0xbd] = &abx_read(op_lda);
        table[0xb9] = &aby_read(op_lda);
        table[0xa1] = &izx_read(op_lda);
        table[0xb1] = &izy_read(op_lda);

        // LDX
        table[0xa2] = &immediate(op_ldx);
        table[0xa6] = &zp_read(op_ldx);
        table[0xb6] = &zpy_read(op_ldx);
        table[0xae] = &abs_read(op_ldx);
        table[0xbe] = &aby_read(op_ldx);

        // LDY
        table[0xa0] = &immediate(op_ldy);
        table[0xa4] = &zp_read(op_ldy);
        table[0xb4] = &zpx_read(op_ldy);
        table[0xac] = &abs_read(op_ldy);
        table[0xbc] = &abx_read(op_ldy);

        // STA
        table[0x85] = &zp_write(get_a);
        table[0x95] = &zpx_write(get_a);
        table[0x8d] = &abs_write(get_a);
        table[0x9d] = &abx_write(get_a);
        table[0x99] = &aby_write(get_a);
        table[0x81] = &izx_write(get_a);
        table[0x91] = &izy_write(get_a);

        // STX
        table[0x86] = &zp_write(get_x);
        table[0x96] = &zpy_write(get_x);
        table[0x8e] = &abs_write(get_x);

        // STY
        table[0x84] = &zp_write(get_y);
        table[0x94] = &zpx_write(get_y);
        table[0x8c] = &abs_write(get_y);

        // ASL
        table[0x0a] = &acc_rmw(op_asl);
        table[0x06] = &zp_rmw(op_asl);
        table[0x16] = &zpx_rmw(op_asl);
        table[0x0e] = &abs_rmw(op_asl);
        table[0x1e] = &abx_rmw(op_asl);

        // LSR
        table[0x4a] = &acc_rmw(op_lsr);
        table[0x46] = &zp_rmw(op_lsr);
        table[0x56] = &zpx_rmw(op_lsr);
        table[0x4e] = &abs_rmw(op_lsr);
        table[0x5e] = &abx_rmw(op_lsr);

        // ROL
        table[0x2a] = &acc_rmw(op_rol);
        table[0x26] = &zp_rmw(op_rol);
        table[0x36] = &zpx_rmw(op_rol);
        table[0x2e] = &abs_rmw(op_rol);
        table[0x3e] = &abx_rmw(op_rol);

        // ROR
        table[0x6a] = &acc_rmw(op_ror);
        table[0x66] = &zp_rmw(op_ror);
        table[0x76] = &zpx_rmw(op_ror);
        table[0x6e] = &abs_rmw(op_ror);
        table[0x7e] = &abx_rmw(op_ror);

        // INC
        table[0xe6] = &zp_rmw(op_inc);
        table[0xf6] = &zpx_rmw(op_inc);
        table[0xee] = &abs_rmw(op_inc);
        table[0xfe] = &abx_rmw(op_inc);

        // DEC
        table[0xc6] = &zp_rmw(op_dec);
        table[0xd6] = &zpx_rmw(op_dec);
        table[0xce] = &abs_rmw(op_dec);
        table[0xde] = &abx_rmw(op_dec);

        // Implied register ops
        table[0xaa] = &implied(impl_tax);
        table[0xa8] = &implied(impl_tay);
        table[0x8a] = &implied(impl_txa);
        table[0x98] = &implied(impl_tya);
        table[0xba] = &implied(impl_tsx);
        table[0x9a] = &implied(impl_txs);
        table[0xe8] = &implied(impl_inx);
        table[0xc8] = &implied(impl_iny);
        table[0xca] = &implied(impl_dex);
        table[0x88] = &implied(impl_dey);
        table[0x18] = &implied(impl_clc);
        table[0x38] = &implied(impl_sec);
        table[0x58] = &implied(impl_cli);
        table[0x78] = &implied(impl_sei);
        table[0xd8] = &implied(impl_cld);
        table[0xf8] = &implied(impl_sed);
        table[0xb8] = &implied(impl_clv);
        table[0xea] = &implied(impl_nop);

        // NOP variants (unofficial)
        table[0x1a] = &implied(impl_nop);
        table[0x3a] = &implied(impl_nop);
        table[0x5a] = &implied(impl_nop);
        table[0x7a] = &implied(impl_nop);
        table[0xda] = &implied(impl_nop);
        table[0xfa] = &implied(impl_nop);

        // DOP (double NOP) - immediate
        table[0x80] = &exec_nop_imm;
        table[0x82] = &exec_nop_imm;
        table[0x89] = &exec_nop_imm;
        table[0xc2] = &exec_nop_imm;
        table[0xe2] = &exec_nop_imm;

        // DOP zero page
        table[0x04] = &exec_nop_zp;
        table[0x44] = &exec_nop_zp;
        table[0x64] = &exec_nop_zp;

        // DOP zero page,X
        table[0x14] = &exec_nop_zpx;
        table[0x34] = &exec_nop_zpx;
        table[0x54] = &exec_nop_zpx;
        table[0x74] = &exec_nop_zpx;
        table[0xd4] = &exec_nop_zpx;
        table[0xf4] = &exec_nop_zpx;

        // TOP absolute
        table[0x0c] = &exec_nop_abs;

        // TOP absolute,X
        table[0x1c] = &exec_nop_abx;
        table[0x3c] = &exec_nop_abx;
        table[0x5c] = &exec_nop_abx;
        table[0x7c] = &exec_nop_abx;
        table[0xdc] = &exec_nop_abx;
        table[0xfc] = &exec_nop_abx;

        // Branches
        table[0x10] = &branch(cond_bpl);
        table[0x30] = &branch(cond_bmi);
        table[0x50] = &branch(cond_bvc);
        table[0x70] = &branch(cond_bvs);
        table[0x90] = &branch(cond_bcc);
        table[0xb0] = &branch(cond_bcs);
        table[0xd0] = &branch(cond_bne);
        table[0xf0] = &branch(cond_beq);

        // Stack
        table[0x48] = &exec_pha;
        table[0x08] = &exec_php;
        table[0x68] = &exec_pla;
        table[0x28] = &exec_plp;

        // Jumps & Returns
        table[0x4c] = &exec_jmp_abs;
        table[0x6c] = &exec_jmp_ind;
        table[0x20] = &exec_jsr;
        table[0x60] = &exec_rts;
        table[0x40] = &exec_rti;

        return table;
    }

    fn exec_unimplemented(self: *CPU) void {
        const info = cpu_debug.debug_op_code(self.opcode);
        std.debug.print("Unimplemented opcode: {s} 0x{x:0>2} at PC=0x{x:0>4}\n", .{ info[1], self.opcode, self.pc -% 1 });
        unreachable;
    }
};

test "6502_functional_test" {
    std.debug.print("\n", .{});
    const file = @embedFile("6502_functional_test.bin");
    var buffer: [file.len]u8 = undefined;

    std.mem.copyForwards(u8, &buffer, file);

    var loaded_rom = try rom.Rom.load_unchecked(&buffer);
    var nes: Bus = Bus.init();
    nes.cpu.bus = &nes;

    nes.load_rom(&loaded_rom);

    nes.reset();

    nes.cpu.pc = 0x0400;

    var pc: u16 = 0;
    var stuck_count: u32 = 0;
    while (stuck_count < 100) {
        nes.clock();
        // Only check for stuck at instruction boundaries
        if (nes.cpu.step == 0) {
            if (nes.cpu.pc == pc) {
                stuck_count += 1;
            } else {
                stuck_count = 0;
                pc = nes.cpu.pc;
            }
        }
    }

    try std.testing.expectEqual(0x3399, nes.cpu.pc);
}

test "nestest" {
    std.debug.print("\n", .{});
    const file = @embedFile("roms/nestest.nes");
    var buffer: [file.len]u8 = undefined;

    std.mem.copyForwards(u8, &buffer, file);

    var loaded_rom = try rom.Rom.load(&buffer);
    var nes: Bus = Bus.init();
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
