
pub const AddrMode = union(enum) {
  /// Zero page addressing uses a single operand which serves as a pointer to an address in zero page ($0000-$00FF) where the data to be operated on can be found.
  /// By using zero page addressing, only one byte is needed for the operand, so the instruction is shorter and, therefore, faster to execute than with addressing modes which take two operands.
  zeroPage: u8,

  /// Indexed zero page addressing takes a single operand and adds the value of a register to it to give an address in zero page ($0000-$00FF) where the data can be found.
  /// There are two forms of indexed zero page addressing:
  // TODO: Wraparound is used when performing the addition so the address of the data will always be in zero page.
  indexedZeroPageX: u8,
  indexedZeroPageY: u8,
  /// In absolute addressing, the address of the data to operate on is specified by the two operands supplied, least significant byte first.
  absolute: u16,

  /// Indexed absolute addressing takes two operands, forming a 16-bit address, least significant byte first, and adds the value of a register to it to give the address where the data can be found.
  /// There are two forms of indexed absolute addressing:
  indexedAbsoluteX: u16,
  indexedAbsoluteY: u16,

  /// Indirect addressing takes two operands, forming a 16-bit address, which identifies the least significant byte of another address which is where the data can be found.
  /// On the 6502, only JMP (Jump) uses this addressing mode and an example is JMP ($1234). The diagram shows the general form of indirect addressing. However, with the JMP instruction, instead of yyxx pointing to the data and the program counter being increased by three, the program counter is set to yyxx and execution resumes from that address.
  indirect: u16,

  /// Implied - Many instructions do not require access to operands stored in memory. Examples of implied instructions are CLD (Clear Decimal Mode) and NOP (No Operation).
  implied: void,

  /// Some instructions operate directly on the contents of the accumulator. The only instructions to use this addressing mode are the shift instructions, ASL (Arithmetic Shift Left), LSR (Logical Shift Right), ROL (Rotate Left) and ROR (Rotate Right).
  accumulator: void,

  /// Instructions which use immediate addressing operate directly on a constant supplied as an operand to the instruction. Immediate instructions are indicated by prefacing the operand with #, for example AND #$12.
  immediate: u8,

  /// Relative addressing is used in branch instructions. This addressing mode causes the value of the program counter to change if a certain condition is met. The condition is dependant on the instruction. The program counter increments by two regardless of the outcome of the condition but if the condition is true the single operand is added to the program counter to give the new value.
  /// For this purpose, the operand is interpreted as a signed byte, that is in the range -128 to 127 to allow forward and backward branching.
  relative: i8,

  /// Indexed indirect (also known as pre-indexed) addressing takes a single byte as an operand and adds the value of the X register to it (with wraparound) to give the address of the least significant byte of the target address.
  indexedIndirect: u16,

  /// Indirect indexed (also known as post-indexed) addressing takes a single operand which gives the zero page address of the least significant byte of a 16-bit address which is then added to the Y register to give the target address.
  indirectIndexed: u16,
};
