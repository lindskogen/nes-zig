const std = @import("std");

// Length counter lookup table (32 entries)
const length_table = [32]u8{
    10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14,
    12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30,
};

// Duty cycle sequences for pulse channels
const duty_table = [4][8]u1{
    .{ 0, 0, 0, 0, 0, 0, 0, 1 }, // 12.5%
    .{ 0, 0, 0, 0, 0, 0, 1, 1 }, // 25%
    .{ 0, 0, 0, 0, 1, 1, 1, 1 }, // 50%
    .{ 1, 1, 1, 1, 1, 1, 0, 0 }, // 75% (inverted 25%)
};

// Triangle channel 32-step sequence
const triangle_sequence = [32]u8{
    15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
};

// Noise channel timer period lookup (NTSC)
const noise_period_table = [16]u16{
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
};

// DMC rate table (NTSC) - in CPU cycles
const dmc_rate_table = [16]u16{
    428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54,
};

// ─── Envelope ───────────────────────────────────────────────────────────────

const Envelope = struct {
    start: bool = false,
    loop_flag: bool = false,
    constant_volume: bool = false,
    volume: u4 = 0,
    decay_level: u4 = 0,
    divider: u4 = 0,

    fn clock(self: *Envelope) void {
        if (self.start) {
            self.start = false;
            self.decay_level = 15;
            self.divider = self.volume;
        } else {
            if (self.divider == 0) {
                self.divider = self.volume;
                if (self.decay_level == 0) {
                    if (self.loop_flag) {
                        self.decay_level = 15;
                    }
                } else {
                    self.decay_level -= 1;
                }
            } else {
                self.divider -= 1;
            }
        }
    }

    fn output(self: *const Envelope) u4 {
        if (self.constant_volume) {
            return self.volume;
        } else {
            return self.decay_level;
        }
    }
};

// ─── Sweep ──────────────────────────────────────────────────────────────────

const Sweep = struct {
    enabled: bool = false,
    negate: bool = false,
    reload: bool = false,
    shift: u3 = 0,
    period: u3 = 0,
    divider: u3 = 0,
    /// true for pulse 1 (ones' complement negate), false for pulse 2 (twos' complement)
    ones_complement: bool = false,

    fn clock(self: *Sweep, timer_period: *u11) void {
        const change = @as(u16, timer_period.*) >> self.shift;
        const target: i32 = if (self.negate)
            @as(i32, timer_period.*) - @as(i32, change) - @as(i32, if (self.ones_complement) @as(u1, 1) else 0)
        else
            @as(i32, timer_period.*) + @as(i32, change);

        const muting = timer_period.* < 8 or target > 0x7FF;

        if (self.divider == 0 and self.enabled and !muting and self.shift > 0) {
            timer_period.* = @intCast(@as(u16, @intCast(std.math.clamp(target, 0, 0x7FF))));
        }

        if (self.divider == 0 or self.reload) {
            self.divider = self.period;
            self.reload = false;
        } else {
            self.divider -= 1;
        }
    }

    fn is_muting(self: *const Sweep, timer_period: u11) bool {
        const change = @as(u16, timer_period) >> self.shift;
        const target: i32 = if (self.negate)
            @as(i32, timer_period) - @as(i32, change) - @as(i32, if (self.ones_complement) @as(u1, 1) else 0)
        else
            @as(i32, timer_period) + @as(i32, change);
        return timer_period < 8 or target > 0x7FF;
    }
};

// ─── Length Counter ─────────────────────────────────────────────────────────

const LengthCounter = struct {
    counter: u8 = 0,
    halt: bool = false,
    enabled: bool = false,

    fn clock(self: *LengthCounter) void {
        if (!self.halt and self.counter > 0) {
            self.counter -= 1;
        }
    }

    fn set_enabled(self: *LengthCounter, enabled: bool) void {
        self.enabled = enabled;
        if (!enabled) {
            self.counter = 0;
        }
    }

    fn load(self: *LengthCounter, index: u5) void {
        if (self.enabled) {
            self.counter = length_table[index];
        }
    }
};

// ─── Pulse Channel ──────────────────────────────────────────────────────────

const Pulse = struct {
    duty: u2 = 0,
    duty_pos: u3 = 0,
    timer_period: u11 = 0,
    timer: u16 = 0,
    envelope: Envelope = .{},
    sweep: Sweep = .{},
    length: LengthCounter = .{},

    fn clock_timer(self: *Pulse) void {
        if (self.timer == 0) {
            self.timer = self.timer_period;
            self.duty_pos +%= 1;
        } else {
            self.timer -= 1;
        }
    }

    fn output(self: *const Pulse) u8 {
        if (duty_table[self.duty][self.duty_pos] == 0) return 0;
        if (self.length.counter == 0) return 0;
        if (self.sweep.is_muting(self.timer_period)) return 0;
        return self.envelope.output();
    }

    fn write_register(self: *Pulse, reg: u2, value: u8) void {
        switch (reg) {
            0 => {
                self.duty = @truncate(value >> 6);
                self.length.halt = (value & 0x20) != 0;
                self.envelope.loop_flag = (value & 0x20) != 0;
                self.envelope.constant_volume = (value & 0x10) != 0;
                self.envelope.volume = @truncate(value);
            },
            1 => {
                self.sweep.enabled = (value & 0x80) != 0;
                self.sweep.period = @truncate(value >> 4);
                self.sweep.negate = (value & 0x08) != 0;
                self.sweep.shift = @truncate(value);
                self.sweep.reload = true;
            },
            2 => {
                // Timer low 8 bits
                self.timer_period = (self.timer_period & 0x700) | @as(u11, value);
            },
            3 => {
                // Timer high 3 bits + length counter load
                self.timer_period = (self.timer_period & 0xFF) | (@as(u11, @as(u3, @truncate(value))) << 8);
                self.length.load(@truncate(value >> 3));
                self.duty_pos = 0;
                self.envelope.start = true;
            },
        }
    }
};

// ─── Triangle Channel ───────────────────────────────────────────────────────

const Triangle = struct {
    timer_period: u11 = 0,
    timer: u16 = 0,
    sequence_pos: u5 = 0,
    linear_counter: u7 = 0,
    linear_counter_reload_value: u7 = 0,
    linear_counter_reload: bool = false,
    length: LengthCounter = .{},
    control_flag: bool = false,

    fn clock_timer(self: *Triangle) void {
        if (self.timer == 0) {
            self.timer = self.timer_period;
            if (self.linear_counter > 0 and self.length.counter > 0) {
                self.sequence_pos +%= 1;
            }
        } else {
            self.timer -= 1;
        }
    }

    fn clock_linear_counter(self: *Triangle) void {
        if (self.linear_counter_reload) {
            self.linear_counter = self.linear_counter_reload_value;
        } else if (self.linear_counter > 0) {
            self.linear_counter -= 1;
        }

        if (!self.control_flag) {
            self.linear_counter_reload = false;
        }
    }

    fn output(self: *const Triangle) u8 {
        if (self.length.counter == 0) return 0;
        if (self.linear_counter == 0) return 0;
        // Ultrasonic silencing - timer period < 2 produces inaudible frequencies
        if (self.timer_period < 2) return 0;
        return triangle_sequence[self.sequence_pos];
    }

    fn write_register(self: *Triangle, reg: u2, value: u8) void {
        switch (reg) {
            0 => {
                self.control_flag = (value & 0x80) != 0;
                self.length.halt = (value & 0x80) != 0;
                self.linear_counter_reload_value = @truncate(value);
            },
            // $4009 unused
            1 => {},
            2 => {
                self.timer_period = (self.timer_period & 0x700) | @as(u11, value);
            },
            3 => {
                self.timer_period = (self.timer_period & 0xFF) | (@as(u11, @as(u3, @truncate(value))) << 8);
                self.length.load(@truncate(value >> 3));
                self.linear_counter_reload = true;
            },
        }
    }
};

// ─── Noise Channel ──────────────────────────────────────────────────────────

const Noise = struct {
    shift_register: u15 = 1,
    mode: bool = false,
    timer_period: u12 = 0,
    timer: u16 = 0,
    envelope: Envelope = .{},
    length: LengthCounter = .{},

    fn clock_timer(self: *Noise) void {
        if (self.timer == 0) {
            self.timer = self.timer_period;
            // Clock the shift register
            const bit0: u1 = @truncate(self.shift_register);
            const other_bit: u1 = if (self.mode)
                @truncate(self.shift_register >> 6)
            else
                @truncate(self.shift_register >> 1);
            const feedback: u1 = bit0 ^ other_bit;
            self.shift_register >>= 1;
            self.shift_register |= @as(u15, feedback) << 14;
        } else {
            self.timer -= 1;
        }
    }

    fn output(self: *const Noise) u8 {
        if (self.shift_register & 1 != 0) return 0;
        if (self.length.counter == 0) return 0;
        return self.envelope.output();
    }

    fn write_register(self: *Noise, reg: u2, value: u8) void {
        switch (reg) {
            0 => {
                self.length.halt = (value & 0x20) != 0;
                self.envelope.loop_flag = (value & 0x20) != 0;
                self.envelope.constant_volume = (value & 0x10) != 0;
                self.envelope.volume = @truncate(value);
            },
            // $400D unused
            1 => {},
            2 => {
                self.mode = (value & 0x80) != 0;
                self.timer_period = @intCast(noise_period_table[value & 0x0F]);
            },
            3 => {
                self.length.load(@truncate(value >> 3));
                self.envelope.start = true;
            },
        }
    }
};

// ─── DMC Channel ────────────────────────────────────────────────────────────

const DMC = struct {
    irq_enabled: bool = false,
    loop_flag: bool = false,
    rate: u16 = 0,
    timer: u16 = 0,

    // Output unit
    output_level: u7 = 0,
    shift_register: u8 = 0,
    bits_remaining: u4 = 0,
    silence: bool = true,

    // Sample reader
    sample_address: u16 = 0xC000,
    sample_length: u16 = 0,
    current_address: u16 = 0,
    bytes_remaining: u16 = 0,
    sample_buffer: u8 = 0,
    sample_buffer_empty: bool = true,

    // IRQ
    irq_flag: bool = false,

    fn clock_timer(self: *DMC) void {
        // Output unit
        if (!self.silence) {
            if (self.shift_register & 1 != 0) {
                if (self.output_level <= 125) {
                    self.output_level += 2;
                }
            } else {
                if (self.output_level >= 2) {
                    self.output_level -= 2;
                }
            }
            self.shift_register >>= 1;
        }

        if (self.bits_remaining == 0) {
            self.bits_remaining = 8;
            if (self.sample_buffer_empty) {
                self.silence = true;
            } else {
                self.silence = false;
                self.shift_register = self.sample_buffer;
                self.sample_buffer_empty = true;
            }
        } else {
            self.bits_remaining -= 1;
        }
    }

    pub fn fill_sample_buffer(self: *DMC, data: u8) void {
        if (self.sample_buffer_empty and self.bytes_remaining > 0) {
            self.sample_buffer = data;
            self.sample_buffer_empty = false;
            self.current_address = if (self.current_address == 0xFFFF) 0x8000 else self.current_address + 1;
            self.bytes_remaining -= 1;
            if (self.bytes_remaining == 0) {
                if (self.loop_flag) {
                    self.restart();
                } else if (self.irq_enabled) {
                    self.irq_flag = true;
                }
            }
        }
    }

    fn restart(self: *DMC) void {
        self.current_address = self.sample_address;
        self.bytes_remaining = self.sample_length;
    }

    fn output(self: *const DMC) u8 {
        return self.output_level;
    }

    fn write_register(self: *DMC, reg: u2, value: u8) void {
        switch (reg) {
            0 => {
                self.irq_enabled = (value & 0x80) != 0;
                self.loop_flag = (value & 0x40) != 0;
                self.rate = dmc_rate_table[value & 0x0F];
                if (!self.irq_enabled) {
                    self.irq_flag = false;
                }
            },
            1 => {
                self.output_level = @truncate(value & 0x7F);
            },
            2 => {
                // Sample address = %11AAAAAA.AA000000 = $C000 + A * 64
                self.sample_address = 0xC000 + @as(u16, value) * 64;
            },
            3 => {
                // Sample length = %0000LLLL.LLLL0001 = L * 16 + 1
                self.sample_length = @as(u16, value) * 16 + 1;
            },
        }
    }
};

// ─── Ring Buffer ────────────────────────────────────────────────────────────

const RING_BUFFER_SIZE: usize = 16384;

const RingBuffer = struct {
    data: [RING_BUFFER_SIZE]f32 = [_]f32{0} ** RING_BUFFER_SIZE,
    write_pos: std.atomic.Value(usize) = std.atomic.Value(usize).init(0),
    read_pos: std.atomic.Value(usize) = std.atomic.Value(usize).init(0),

    pub fn push(self: *RingBuffer, sample: f32) void {
        const wp = self.write_pos.load(.acquire);
        const next = (wp + 1) % RING_BUFFER_SIZE;
        // Drop sample if buffer is full
        if (next == self.read_pos.load(.acquire)) return;
        self.data[wp] = sample;
        self.write_pos.store(next, .release);
    }

    pub fn pop(self: *RingBuffer) ?f32 {
        const rp = self.read_pos.load(.acquire);
        if (rp == self.write_pos.load(.acquire)) return null;
        const sample = self.data[rp];
        self.read_pos.store((rp + 1) % RING_BUFFER_SIZE, .release);
        return sample;
    }

    pub fn fill_level(self: *RingBuffer) usize {
        const wp = self.write_pos.load(.monotonic);
        const rp = self.read_pos.load(.monotonic);
        return (wp -% rp) % RING_BUFFER_SIZE;
    }
};

// ─── APU ────────────────────────────────────────────────────────────────────

pub const APU = struct {
    pulse1: Pulse = .{},
    pulse2: Pulse = .{},
    triangle: Triangle = .{},
    noise: Noise = .{},
    dmc: DMC = .{},

    // Frame counter
    frame_mode: u1 = 0, // 0 = 4-step, 1 = 5-step
    frame_counter: u16 = 0,
    frame_irq_inhibit: bool = false,
    frame_irq_flag: bool = false,
    frame_reset_timer: u8 = 0,

    // Cycle counter (counts at CPU rate)
    cycle: u64 = 0,

    // IRQ
    irq_pending: bool = false,

    // DMC read request
    dmc_read_pending: bool = false,
    dmc_read_addr: u16 = 0,

    // Mixer lookup tables
    pulse_table: [31]f32 = undefined,
    tnd_table: [203]f32 = undefined,

    // Downsampling — accumulate integer channel outputs per cycle,
    // only do float mixer math when outputting a sample (~every 40 cycles)
    pulse_acc: u32 = 0,
    tnd_acc: u32 = 0,
    sample_count: u32 = 0,
    cycles_per_sample: f32 = 1789773.0 / 44100.0,
    sample_counter: f32 = 0,

    // Output ring buffer
    ring_buffer: RingBuffer = .{},

    pub fn init() APU {
        var apu = APU{};

        // Init sweep complement modes
        apu.pulse1.sweep.ones_complement = true;
        apu.pulse2.sweep.ones_complement = false;

        // Pre-compute mixer lookup tables
        apu.pulse_table[0] = 0;
        for (1..31) |i| {
            apu.pulse_table[i] = 95.52 / (8128.0 / @as(f32, @floatFromInt(i)) + 100.0);
        }
        apu.tnd_table[0] = 0;
        for (1..203) |i| {
            apu.tnd_table[i] = 163.67 / (24329.0 / @as(f32, @floatFromInt(i)) + 100.0);
        }

        return apu;
    }

    pub fn set_sample_rate(self: *APU, rate: u32) void {
        self.cycles_per_sample = 1789773.0 / @as(f32, @floatFromInt(rate));
    }

    pub fn clock(self: *APU) void {
        // Triangle timer clocks at CPU rate
        self.triangle.clock_timer();

        // Pulse, noise, DMC clock at half CPU rate (every other cycle)
        if (self.cycle % 2 == 0) {
            self.pulse1.clock_timer();
            self.pulse2.clock_timer();
            self.noise.clock_timer();

            if (self.dmc.timer == 0) {
                self.dmc.timer = self.dmc.rate;
                self.dmc.clock_timer();
            } else {
                self.dmc.timer -= 1;
            }
        }

        // Frame counter
        self.clock_frame_counter();

        // Handle frame reset delay
        if (self.frame_reset_timer > 0) {
            self.frame_reset_timer -= 1;
            if (self.frame_reset_timer == 0) {
                self.frame_counter = 0;
                if (self.frame_mode == 1) {
                    self.clock_quarter_frame();
                    self.clock_half_frame();
                }
            }
        }

        // DMC read request
        if (self.dmc.sample_buffer_empty and self.dmc.bytes_remaining > 0) {
            self.dmc_read_pending = true;
            self.dmc_read_addr = self.dmc.current_address;
        }

        // IRQ
        self.irq_pending = (self.frame_irq_flag and !self.frame_irq_inhibit) or self.dmc.irq_flag;

        // Accumulate integer channel outputs (cheap per-cycle work)
        const p1: u32 = self.pulse1.output();
        const p2: u32 = self.pulse2.output();
        const t: u32 = self.triangle.output();
        const n: u32 = self.noise.output();
        const d: u32 = self.dmc.output();
        self.pulse_acc += p1 + p2;
        self.tnd_acc += t * 3 + n * 2 + d;
        self.sample_count += 1;
        self.sample_counter += 1;

        // Only do float mixer math when outputting a sample (~every 40 cycles)
        if (self.sample_counter >= self.cycles_per_sample) {
            self.sample_counter -= self.cycles_per_sample;
            const count_f: f32 = @floatFromInt(self.sample_count);
            const avg_pulse: usize = @intFromFloat(@as(f32, @floatFromInt(self.pulse_acc)) / count_f + 0.5);
            const avg_tnd: usize = @intFromFloat(@as(f32, @floatFromInt(self.tnd_acc)) / count_f + 0.5);
            const pulse_out = self.pulse_table[@min(avg_pulse, 30)];
            const tnd_out = self.tnd_table[@min(avg_tnd, 202)];
            self.ring_buffer.push(pulse_out + tnd_out);
            self.pulse_acc = 0;
            self.tnd_acc = 0;
            self.sample_count = 0;
        }

        self.cycle +%= 1;
    }

    fn clock_frame_counter(self: *APU) void {
        // Frame counter clocks at CPU rate
        // NTSC: 4-step = 14915 CPU cycles per frame, 5-step = 18641
        // Quarter frame at steps 3729, 7457, 11186, 14915 (4-step) or 18641 (5-step)

        self.frame_counter += 1;

        if (self.frame_mode == 0) {
            // 4-step mode
            switch (self.frame_counter) {
                3729 => self.clock_quarter_frame(),
                7457 => {
                    self.clock_quarter_frame();
                    self.clock_half_frame();
                },
                11186 => self.clock_quarter_frame(),
                14915 => {
                    self.clock_quarter_frame();
                    self.clock_half_frame();
                    if (!self.frame_irq_inhibit) {
                        self.frame_irq_flag = true;
                    }
                    self.frame_counter = 0;
                },
                else => {},
            }
        } else {
            // 5-step mode (no IRQ)
            switch (self.frame_counter) {
                3729 => self.clock_quarter_frame(),
                7457 => {
                    self.clock_quarter_frame();
                    self.clock_half_frame();
                },
                11186 => self.clock_quarter_frame(),
                18641 => {
                    self.clock_quarter_frame();
                    self.clock_half_frame();
                    self.frame_counter = 0;
                },
                else => {},
            }
        }
    }

    fn clock_quarter_frame(self: *APU) void {
        self.pulse1.envelope.clock();
        self.pulse2.envelope.clock();
        self.noise.envelope.clock();
        self.triangle.clock_linear_counter();
    }

    fn clock_half_frame(self: *APU) void {
        self.pulse1.length.clock();
        self.pulse2.length.clock();
        self.triangle.length.clock();
        self.noise.length.clock();
        self.pulse1.sweep.clock(&self.pulse1.timer_period);
        self.pulse2.sweep.clock(&self.pulse2.timer_period);
    }

    pub fn cpu_write(self: *APU, addr: u16, value: u8) void {
        switch (addr) {
            // Pulse 1: $4000-$4003
            0x4000 => self.pulse1.write_register(0, value),
            0x4001 => self.pulse1.write_register(1, value),
            0x4002 => self.pulse1.write_register(2, value),
            0x4003 => self.pulse1.write_register(3, value),

            // Pulse 2: $4004-$4007
            0x4004 => self.pulse2.write_register(0, value),
            0x4005 => self.pulse2.write_register(1, value),
            0x4006 => self.pulse2.write_register(2, value),
            0x4007 => self.pulse2.write_register(3, value),

            // Triangle: $4008-$400B
            0x4008 => self.triangle.write_register(0, value),
            0x4009 => self.triangle.write_register(1, value),
            0x400A => self.triangle.write_register(2, value),
            0x400B => self.triangle.write_register(3, value),

            // Noise: $400C-$400F
            0x400C => self.noise.write_register(0, value),
            0x400D => self.noise.write_register(1, value),
            0x400E => self.noise.write_register(2, value),
            0x400F => self.noise.write_register(3, value),

            // DMC: $4010-$4013
            0x4010 => self.dmc.write_register(0, value),
            0x4011 => self.dmc.write_register(1, value),
            0x4012 => self.dmc.write_register(2, value),
            0x4013 => self.dmc.write_register(3, value),

            // Status: $4015
            0x4015 => {
                self.pulse1.length.set_enabled(value & 0x01 != 0);
                self.pulse2.length.set_enabled(value & 0x02 != 0);
                self.triangle.length.set_enabled(value & 0x04 != 0);
                self.noise.length.set_enabled(value & 0x08 != 0);

                if (value & 0x10 != 0) {
                    if (self.dmc.bytes_remaining == 0) {
                        self.dmc.restart();
                    }
                } else {
                    self.dmc.bytes_remaining = 0;
                }
                self.dmc.irq_flag = false;
            },

            // Frame counter: $4017
            0x4017 => {
                self.frame_mode = @truncate(value >> 7);
                self.frame_irq_inhibit = (value & 0x40) != 0;
                if (self.frame_irq_inhibit) {
                    self.frame_irq_flag = false;
                }
                // Reset timer takes a few cycles
                self.frame_reset_timer = if (self.cycle % 2 == 0) 4 else 3;
            },

            else => {},
        }
    }

    pub fn cpu_read(self: *APU, addr: u16) u8 {
        if (addr == 0x4015) {
            var status: u8 = 0;
            if (self.pulse1.length.counter > 0) status |= 0x01;
            if (self.pulse2.length.counter > 0) status |= 0x02;
            if (self.triangle.length.counter > 0) status |= 0x04;
            if (self.noise.length.counter > 0) status |= 0x08;
            if (self.dmc.bytes_remaining > 0) status |= 0x10;
            if (self.frame_irq_flag) status |= 0x40;
            if (self.dmc.irq_flag) status |= 0x80;
            // Reading $4015 clears frame IRQ flag
            self.frame_irq_flag = false;
            return status;
        }
        return 0;
    }
};
