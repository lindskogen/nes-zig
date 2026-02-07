const std = @import("std");

const CPU = @import("cpu.zig").CPU;
const Rom = @import("rom.zig");

pub fn debug_op_code(code: u8) struct { []const u8, []const u8, u8 } {
    return switch (code) {
        0x00 => .{ "BRK", "BRK #n", 7 },
        0x01 => .{ "ORA", "ORA (z,X)", 6 },
        0x02 => .{ "KIL", "KIL", 2 },
        0x03 => .{ "SLO", "SLO (z,X)", 8 },
        0x04 => .{ "DOP", "DOP z", 3 },
        0x05 => .{ "ORA", "ORA z", 3 },
        0x06 => .{ "ASL", "ASL z", 5 },
        0x07 => .{ "SLO", "SLO z", 5 },
        0x08 => .{ "PHP", "PHP", 1 },
        0x09 => .{ "ORA", "ORA #n", 2 },
        0x0A => .{ "ASL", "ASL A", 2 },
        0x0B => .{ "AAC", "AAC #n", 2 },
        0x0C => .{ "TOP", "TOP abs", 4 },
        0x0D => .{ "ORA", "ORA a", 4 },
        0x0E => .{ "ASL", "ASL a", 6 },
        0x0F => .{ "SLO", "SLO abs", 6 },
        0x10 => .{ "BPL", "BPL r", 2 },
        0x11 => .{ "ORA", "ORA (z),Y", 5 },
        0x12 => .{ "KIL", "KIL", 2 },
        0x13 => .{ "SLO", "SLO (z),Y", 8 },
        0x14 => .{ "DOP", "DOP z,X", 4 },
        0x15 => .{ "ORA", "ORA z,X", 4 },
        0x16 => .{ "ASL", "ASL z,X", 6 },
        0x17 => .{ "SLO", "SLO z,X", 6 },
        0x18 => .{ "CLC", "CLC", 1 },
        0x19 => .{ "ORA", "ORA a,Y", 4 },
        0x1A => .{ "NOP", "NOP", 2 },
        0x1B => .{ "SLO", "SLO abs,Y", 7 },
        0x1C => .{ "TOP", "TOP abs,X", 4 },
        0x1D => .{ "ORA", "ORA a,X", 4 },
        0x1E => .{ "ASL", "ASL a,X", 7 },
        0x1F => .{ "SLO", "SLO abs,X", 7 },
        0x20 => .{ "JSR", "JSR a", 6 },
        0x21 => .{ "AND", "AND (z,X)", 6 },
        0x22 => .{ "KIL", "KIL", 2 },
        0x23 => .{ "RLA", "RLA (z,X)", 8 },
        0x24 => .{ "BIT", "BIT z", 2 },
        0x25 => .{ "AND", "AND z", 3 },
        0x26 => .{ "ROL", "ROL z", 5 },
        0x27 => .{ "RLA", "RLA z", 5 },
        0x28 => .{ "PLP", "PLP", 1 },
        0x29 => .{ "AND", "AND #n", 2 },
        0x2A => .{ "ROL", "ROL A", 2 },
        0x2B => .{ "AAC", "AAC #n", 2 },
        0x2C => .{ "BIT", "BIT a", 3 },
        0x2D => .{ "AND", "AND a", 4 },
        0x2E => .{ "ROL", "ROL a", 6 },
        0x2F => .{ "RLA", "RLA abs", 6 },
        0x30 => .{ "BMI", "BMI r", 2 },
        0x31 => .{ "AND", "AND (z),Y", 5 },
        0x32 => .{ "KIL", "KIL", 2 },
        0x33 => .{ "RLA", "RLA (z),Y", 8 },
        0x34 => .{ "DOP", "DOP z,X", 4 },
        0x35 => .{ "AND", "AND z,X", 4 },
        0x36 => .{ "ROL", "ROL z,X", 6 },
        0x37 => .{ "RLA", "RLA z,X", 6 },
        0x38 => .{ "SEC", "SEC", 1 },
        0x39 => .{ "AND", "AND a,Y", 4 },
        0x3A => .{ "NOP", "NOP", 2 },
        0x3B => .{ "RLA", "RLA abs,Y", 7 },
        0x3C => .{ "TOP", "TOP abs,X", 4 },
        0x3D => .{ "AND", "AND a,X", 4 },
        0x3E => .{ "ROL", "ROL a,X", 7 },
        0x3F => .{ "RLA", "RLA abs,X", 7 },
        0x40 => .{ "RTI", "RTI", 6 },
        0x41 => .{ "EOR", "EOR (z,X)", 6 },
        0x42 => .{ "KIL", "KIL", 2 },
        0x43 => .{ "SRE", "SRE (z,X)", 8 },
        0x44 => .{ "DOP", "DOP z", 3 },
        0x45 => .{ "EOR", "EOR z", 3 },
        0x46 => .{ "LSR", "LSR z", 5 },
        0x47 => .{ "SRE", "SRE z", 5 },
        0x48 => .{ "PHA", "PHA", 1 },
        0x49 => .{ "EOR", "EOR #n", 2 },
        0x4A => .{ "LSR", "LSR A", 2 },
        0x4B => .{ "ASR", "ASR #n", 2 },
        0x4C => .{ "JMP", "JMP a", 3 },
        0x4D => .{ "EOR", "EOR a", 4 },
        0x4E => .{ "LSR", "LSR a", 6 },
        0x4F => .{ "SRE", "SRE abs", 6 },
        0x50 => .{ "BVC", "BVC r", 2 },
        0x51 => .{ "EOR", "EOR (z),Y", 5 },
        0x52 => .{ "KIL", "KIL", 2 },
        0x53 => .{ "SRE", "SRE (z),Y", 8 },
        0x54 => .{ "DOP", "DOP z,X", 4 },
        0x55 => .{ "EOR", "EOR z,X", 4 },
        0x56 => .{ "LSR", "LSR z,X", 6 },
        0x57 => .{ "SRE", "SRE z,X", 6 },
        0x58 => .{ "CLI", "CLI", 2 },
        0x59 => .{ "EOR", "EOR a,Y", 4 },
        0x5A => .{ "NOP", "NOP", 2 },
        0x5B => .{ "SRE", "SRE abs,Y", 7 },
        0x5C => .{ "TOP", "TOP abs,X", 4 },
        0x5D => .{ "EOR", "EOR a,X", 4 },
        0x5E => .{ "LSR", "LSR a,X", 7 },
        0x5F => .{ "SRE", "SRE abs,X", 7 },
        0x60 => .{ "RTS", "RTS", 1 },
        0x61 => .{ "ADC", "ADC (z,X)", 6 },
        0x62 => .{ "KIL", "KIL", 2 },
        0x63 => .{ "RRA", "RRA (z,X)", 8 },
        0x64 => .{ "DOP", "DOP z", 3 },
        0x65 => .{ "ADC", "ADC z", 3 },
        0x66 => .{ "ROR", "ROR z", 5 },
        0x67 => .{ "RRA", "RRA z", 5 },
        0x68 => .{ "PLA", "PLA", 1 },
        0x69 => .{ "ADC", "ADC #n", 2 },
        0x6A => .{ "ROR", "ROR A", 2 },
        0x6B => .{ "ARR", "ARR #n", 2 },
        0x6C => .{ "JMP", "JMP (a)", 5 },
        0x6D => .{ "ADC", "ADC a", 4 },
        0x6E => .{ "ROR", "ROR a", 6 },
        0x6F => .{ "RRA", "RRA abs", 6 },
        0x70 => .{ "BVS", "BVS r", 2 },
        0x71 => .{ "ADC", "ADC (z),Y", 5 },
        0x72 => .{ "KIL", "KIL", 2 },
        0x73 => .{ "RRA", "RRA (z),Y", 8 },
        0x74 => .{ "DOP", "DOP z,X", 4 },
        0x75 => .{ "ADC", "ADC z,X", 4 },
        0x76 => .{ "ROR", "ROR z,X", 6 },
        0x77 => .{ "RRA", "RRA z,X", 6 },
        0x78 => .{ "SEI", "SEI", 1 },
        0x79 => .{ "ADC", "ADC a,Y", 4 },
        0x7A => .{ "NOP", "NOP", 2 },
        0x7B => .{ "RRA", "RRA abs,Y", 7 },
        0x7C => .{ "TOP", "TOP abs,X", 4 },
        0x7D => .{ "ADC", "ADC a,X", 4 },
        0x7E => .{ "ROR", "ROR a,X", 7 },
        0x7F => .{ "RRA", "RRA abs,X", 7 },
        0x80 => .{ "DOP", "DOP #n", 2 },
        0x81 => .{ "STA", "STA (z,X)", 6 },
        0x82 => .{ "DOP", "DOP #n", 2 },
        0x83 => .{ "AAX", "AAX (z,X)", 6 },
        0x84 => .{ "STY", "STY z", 3 },
        0x85 => .{ "STA", "STA z", 2 },
        0x86 => .{ "STX", "STX z", 2 },
        0x87 => .{ "AAX", "AAX z", 3 },
        0x88 => .{ "DEY", "DEY", 1 },
        0x89 => .{ "DOP", "DOP #n", 2 },
        0x8A => .{ "TXA", "TXA", 1 },
        0x8B => .{ "XAA", "XAA #n", 2 },
        0x8C => .{ "STY", "STY a", 4 },
        0x8D => .{ "STA", "STA a", 3 },
        0x8E => .{ "STX", "STX a", 4 },
        0x8F => .{ "AAX", "AAX abs", 4 },
        0x90 => .{ "BCC", "BCC r", 2 },
        0x91 => .{ "STA", "STA (z),Y", 6 },
        0x92 => .{ "KIL", "KIL", 2 },
        0x93 => .{ "AXA", "AXA (z),Y", 6 },
        0x94 => .{ "STY", "STY z,X", 4 },
        0x95 => .{ "STA", "STA z,X", 4 },
        0x96 => .{ "STX", "STX z,Y", 4 },
        0x97 => .{ "AAX", "AAX z,Y", 4 },
        0x98 => .{ "TYA", "TYA", 1 },
        0x99 => .{ "STA", "STA a,Y", 5 },
        0x9A => .{ "TXS", "TXS", 1 },
        0x9B => .{ "XAS", "XAS abs,Y", 5 },
        0x9C => .{ "SYA", "SYA abs,X", 5 },
        0x9D => .{ "STA", "STA a,X", 5 },
        0x9E => .{ "SXA", "SXA abs,Y", 5 },
        0x9F => .{ "AXA", "AXA abs,Y", 5 },
        0xA0 => .{ "LDY", "LDY #n", 2 },
        0xA1 => .{ "LDA", "LDA (z,X)", 6 },
        0xA2 => .{ "LDX", "LDX #n", 2 },
        0xA3 => .{ "LAX", "LAX (z,X)", 6 },
        0xA4 => .{ "LDY", "LDY z", 3 },
        0xA5 => .{ "LDA", "LDA z", 3 },
        0xA6 => .{ "LDX", "LDX z", 3 },
        0xA7 => .{ "LAX", "LAX z", 3 },
        0xA8 => .{ "TAY", "TAY", 1 },
        0xA9 => .{ "LDA", "LDA #n", 2 },
        0xAA => .{ "TAX", "TAX", 1 },
        0xAB => .{ "ATX", "ATX #n", 2 },
        0xAC => .{ "LDY", "LDY a", 4 },
        0xAD => .{ "LDA", "LDA a", 4 },
        0xAE => .{ "LDX", "LDX a", 4 },
        0xAF => .{ "LAX", "LAX abs", 4 },
        0xB0 => .{ "BCS", "BCS r", 2 },
        0xB1 => .{ "LDA", "LDA (z),Y", 5 },
        0xB2 => .{ "KIL", "KIL", 2 },
        0xB3 => .{ "LAX", "LAX (z),Y", 5 },
        0xB4 => .{ "LDY", "LDY z,X", 4 },
        0xB5 => .{ "LDA", "LDA z,X", 4 },
        0xB6 => .{ "LDX", "LDX z,Y", 4 },
        0xB7 => .{ "LAX", "LAX z,Y", 4 },
        0xB8 => .{ "CLV", "CLV", 1 },
        0xB9 => .{ "LDA", "LDA a,Y", 4 },
        0xBA => .{ "TSX", "TSX", 1 },
        0xBB => .{ "LAR", "LAR abs,Y", 4 },
        0xBC => .{ "LDY", "LDY a,X", 4 },
        0xBD => .{ "LDA", "LDA a,X", 4 },
        0xBE => .{ "LDX", "LDX a,Y", 4 },
        0xBF => .{ "LAX", "LAX abs,Y", 4 },
        0xC0 => .{ "CPY", "CPY #n", 2 },
        0xC1 => .{ "CMP", "CMP (z,X)", 6 },
        0xC2 => .{ "DOP", "DOP #n", 2 },
        0xC3 => .{ "DCP", "DCP (z,X)", 8 },
        0xC4 => .{ "CPY", "CPY z", 3 },
        0xC5 => .{ "CMP", "CMP z", 3 },
        0xC6 => .{ "DEC", "DEC z", 5 },
        0xC7 => .{ "DCP", "DCP z", 5 },
        0xC8 => .{ "INY", "INY", 1 },
        0xC9 => .{ "CMP", "CMP #n", 2 },
        0xCA => .{ "DEX", "DEX", 1 },
        0xCB => .{ "AXS", "AXS #n", 2 },
        0xCC => .{ "CPY", "CPY a", 4 },
        0xCD => .{ "CMP", "CMP a", 4 },
        0xCE => .{ "DEC", "DEC a", 6 },
        0xCF => .{ "DCP", "DCP abs", 6 },
        0xD0 => .{ "BNE", "BNE r", 2 },
        0xD1 => .{ "CMP", "CMP (z),Y", 5 },
        0xD2 => .{ "KIL", "KIL", 2 },
        0xD3 => .{ "DCP", "DCP (z),Y", 8 },
        0xD4 => .{ "DOP", "DOP z,X", 4 },
        0xD5 => .{ "CMP", "CMP z,X", 4 },
        0xD6 => .{ "DEC", "DEC z,X", 6 },
        0xD7 => .{ "DCP", "DCP z,X", 6 },
        0xD8 => .{ "CLD", "CLD", 1 },
        0xD9 => .{ "CMP", "CMP a,Y", 4 },
        0xDA => .{ "NOP", "NOP", 1 },
        0xDB => .{ "DCP", "DCP abs,Y", 7 },
        0xDC => .{ "TOP", "TOP abs,X", 4 },
        0xDD => .{ "CMP", "CMP a,X", 4 },
        0xDE => .{ "DEC", "DEC a,X", 7 },
        0xDF => .{ "DCP", "DCP abs,X", 7 },
        0xE0 => .{ "CPX", "CPX #n", 2 },
        0xE1 => .{ "SBC", "SBC (z,X)", 6 },
        0xE2 => .{ "DOP", "DOP #n", 2 },
        0xE3 => .{ "ISC", "ISC (z,X)", 8 },
        0xE4 => .{ "CPX", "CPX z", 3 },
        0xE5 => .{ "SBC", "SBC z", 3 },
        0xE6 => .{ "INC", "INC z", 5 },
        0xE7 => .{ "ISC", "ISC z", 5 },
        0xE8 => .{ "INX", "INX", 1 },
        0xE9 => .{ "SBC", "SBC #n", 2 },
        0xEA => .{ "NOP", "NOP", 1 },
        0xEB => .{ "SBC", "SBC #n", 2 },
        0xEC => .{ "CPX", "CPX a", 4 },
        0xED => .{ "SBC", "SBC a", 4 },
        0xEE => .{ "INC", "INC a", 6 },
        0xEF => .{ "ISC", "ISC abs", 6 },
        0xF0 => .{ "BEQ", "BEQ r", 2 },
        0xF1 => .{ "SBC", "SBC (z),Y", 5 },
        0xF2 => .{ "KIL", "KIL", 2 },
        0xF3 => .{ "ISC", "ISC (z),Y", 8 },
        0xF4 => .{ "DOP", "DOP z,X", 4 },
        0xF5 => .{ "SBC", "SBC z,X", 4 },
        0xF6 => .{ "INC", "INC z,X", 6 },
        0xF7 => .{ "ISC", "ISC z,X", 6 },
        0xF8 => .{ "SED", "SED", 1 },
        0xF9 => .{ "SBC", "SBC a,Y", 4 },
        0xFA => .{ "NOP", "NOP", 2 },
        0xFB => .{ "ISC", "ISC abs,Y", 7 },
        0xFC => .{ "TOP", "TOP abs,X", 4 },
        0xFD => .{ "SBC", "SBC a,X", 4 },
        0xFE => .{ "INC", "INC a,X", 7 },
        0xFF => .{ "ISC", "ISC abs,X", 7 },
    };
}

/// Addressing mode categories for debug display
const AddrModeType = enum {
    implied,
    accumulator,
    immediate,
    zero_page,
    zero_page_x,
    zero_page_y,
    absolute,
    absolute_x,
    absolute_y,
    indirect,
    indexed_indirect,
    indirect_indexed,
    relative,
};

fn get_addr_mode(opcode: u8) AddrModeType {
    return switch (opcode) {
        // Implied
        0x00, 0x08, 0x18, 0x28, 0x38, 0x40, 0x48, 0x58, 0x60, 0x68, 0x78, 0x88, 0x8A, 0x98, 0x9A, 0xA8, 0xAA, 0xB8, 0xBA, 0xC8, 0xCA, 0xD8, 0xE8, 0xEA, 0xF8 => .implied,
        0x1A, 0x3A, 0x5A, 0x7A, 0xDA, 0xFA => .implied,

        // Accumulator
        0x0A, 0x2A, 0x4A, 0x6A => .accumulator,

        // Immediate
        0x09, 0x29, 0x49, 0x69, 0xA0, 0xA2, 0xA9, 0xC0, 0xC9, 0xE0, 0xE9 => .immediate,
        0x80, 0x82, 0x89, 0xC2, 0xE2, 0xEB => .immediate,

        // Relative (branches)
        0x10, 0x30, 0x50, 0x70, 0x90, 0xB0, 0xD0, 0xF0 => .relative,

        // Zero page
        0x05, 0x06, 0x24, 0x25, 0x26, 0x45, 0x46, 0x65, 0x66, 0x84, 0x85, 0x86, 0xA4, 0xA5, 0xA6, 0xC4, 0xC5, 0xC6, 0xE4, 0xE5, 0xE6 => .zero_page,
        0x04, 0x44, 0x64 => .zero_page,

        // Zero page,X
        0x15, 0x16, 0x35, 0x36, 0x55, 0x56, 0x75, 0x76, 0x94, 0x95, 0xB4, 0xB5, 0xD5, 0xD6, 0xF5, 0xF6 => .zero_page_x,
        0x14, 0x34, 0x54, 0x74, 0xD4, 0xF4 => .zero_page_x,

        // Zero page,Y
        0x96, 0xB6 => .zero_page_y,

        // Absolute
        0x0D, 0x0E, 0x20, 0x2C, 0x2D, 0x2E, 0x4C, 0x4D, 0x4E, 0x6D, 0x6E, 0x8C, 0x8D, 0x8E, 0xAC, 0xAD, 0xAE, 0xCC, 0xCD, 0xCE, 0xEC, 0xED, 0xEE => .absolute,
        0x0C => .absolute,

        // Absolute,X
        0x1D, 0x1E, 0x3D, 0x3E, 0x5D, 0x5E, 0x7D, 0x7E, 0x9D, 0xBC, 0xBD, 0xDD, 0xDE, 0xFD, 0xFE => .absolute_x,
        0x1C, 0x3C, 0x5C, 0x7C, 0xDC, 0xFC => .absolute_x,

        // Absolute,Y
        0x19, 0x39, 0x59, 0x79, 0x99, 0xB9, 0xBE, 0xD9, 0xF9 => .absolute_y,

        // Indirect
        0x6C => .indirect,

        // (Indirect,X)
        0x01, 0x21, 0x41, 0x61, 0x81, 0xA1, 0xC1, 0xE1 => .indexed_indirect,

        // (Indirect),Y
        0x11, 0x31, 0x51, 0x71, 0x91, 0xB1, 0xD1, 0xF1 => .indirect_indexed,

        else => .implied,
    };
}

inline fn add_i8(a: u16, b: i8) u16 {
    return if (b >= 0) a + @abs(b) else a - @abs(b);
}

pub fn disassemble(rom_data: *Rom.Rom, writer: std.fs.File.Writer) !void {
    var instr_pos: usize = 0;

    while (instr_pos < rom_data.buffer.len) : (instr_pos += 1) {
        const instr = rom_data.buffer[instr_pos];

        const info = debug_op_code(instr);
        const name = info[1];
        const bytes = info[2];

        try writer.print("{X:0>4}  ", .{instr_pos});

        for (0..3) |offset| {
            if (offset < bytes) {
                try writer.print("{X:0>2} ", .{rom_data.buffer[instr_pos + @as(u16, @intCast(offset))]});
            } else {
                try writer.print("   ", .{});
            }
        }
        instr_pos += bytes;

        try writer.print(" {s}\n", .{name});
    }
}

/// Debug print at instruction boundary. Peeks operand bytes directly from bus.
pub fn debug_print(cpu: *CPU, writer: std.fs.File.Writer, instr_pos: u16, instr: u8) !void {
    const info = debug_op_code(instr);
    const name = info[0];
    const bytes = info[2];

    try writer.print("{X:0>4}  ", .{instr_pos});

    for (0..3) |offset| {
        if (offset < bytes) {
            try writer.print("{X:0>2} ", .{cpu.bus.?.read(instr_pos +% @as(u16, @intCast(offset)))});
        } else {
            try writer.print("   ", .{});
        }
    }

    try writer.print(" {s}", .{name});

    const mode = get_addr_mode(instr);
    switch (mode) {
        .implied => try writer.print("{s:<28}", .{""}),
        .accumulator => try writer.print("{s:<28}", .{""}),
        .immediate => {
            const operand = cpu.bus.?.read(instr_pos +% 1);
            try writer.print(" #${X:0>2}{s:<23}", .{ operand, "" });
        },
        .zero_page => {
            const operand = cpu.bus.?.read(instr_pos +% 1);
            try writer.print(" ${X:0>2}{s:<24}", .{ operand, "" });
        },
        .zero_page_x => {
            const operand = cpu.bus.?.read(instr_pos +% 1);
            try writer.print(" X,${X:0>2}{s:<22}", .{ operand, "" });
        },
        .zero_page_y => {
            const operand = cpu.bus.?.read(instr_pos +% 1);
            try writer.print(" Y,${X:0>2}{s:<22}", .{ operand, "" });
        },
        .absolute => {
            const lo: u16 = cpu.bus.?.read(instr_pos +% 1);
            const hi: u16 = cpu.bus.?.read(instr_pos +% 2);
            const addr = (hi << 8) | lo;
            try writer.print(" ${X:0>4}{s:<22}", .{ addr, "" });
        },
        .absolute_x => {
            const lo: u16 = cpu.bus.?.read(instr_pos +% 1);
            const hi: u16 = cpu.bus.?.read(instr_pos +% 2);
            const addr = (hi << 8) | lo;
            try writer.print(" X,${X:0>4}{s:<20}", .{ addr, "" });
        },
        .absolute_y => {
            const lo: u16 = cpu.bus.?.read(instr_pos +% 1);
            const hi: u16 = cpu.bus.?.read(instr_pos +% 2);
            const addr = (hi << 8) | lo;
            try writer.print(" Y,${X:0>4}{s:<20}", .{ addr, "" });
        },
        .indirect => {
            const lo: u16 = cpu.bus.?.read(instr_pos +% 1);
            const hi: u16 = cpu.bus.?.read(instr_pos +% 2);
            const addr = (hi << 8) | lo;
            try writer.print(" #${X:0>4}{s:<22}", .{ addr, "" });
        },
        .indexed_indirect => {
            const operand = cpu.bus.?.read(instr_pos +% 1);
            try writer.print(" (${X:0>2},X) @ {X:0>2}{s:<22}", .{ operand, operand +% cpu.x, "" });
        },
        .indirect_indexed => {
            const operand = cpu.bus.?.read(instr_pos +% 1);
            try writer.print(" (${X:0>2}),Y{s:<20}", .{ operand, "" });
        },
        .relative => {
            const offset: i8 = @bitCast(cpu.bus.?.read(instr_pos +% 1));
            const target = add_i8(instr_pos +% 2, offset);
            try writer.print(" ${X:0>4}{s:<22}", .{ target, "" });
        },
    }

    try writer.print(" A:{X:0>2} X:{X:0>2} Y:{X:0>2} P:{X:0>2} SP:{X:0>2} CYC:{d}\n", .{
        cpu.a,
        cpu.x,
        cpu.y,
        @as(u8, @bitCast(cpu.p)),
        cpu.sp,
        cpu.total_cycles,
    });
}
