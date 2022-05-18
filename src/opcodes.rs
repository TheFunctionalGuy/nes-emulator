//! Opcodes for 6502/2A03 processor
use std::collections::HashMap;

use crate::cpu::AddressingMode;

pub struct OpCode {
	pub code: u8,
	pub mnemonic: &'static str,
	pub length: u8,
	pub cycles: u8,
	pub addressing_mode: AddressingMode,
}

impl OpCode {
	pub fn new(
		code: u8,
		mnemonic: &'static str,
		length: u8,
		cycles: u8,
		addressing_mode: AddressingMode,
	) -> Self {
		OpCode {
			code,
			mnemonic,
			length,
			cycles,
			addressing_mode,
		}
	}
}

lazy_static! {
	pub static ref CPU_OPS_CODES: Vec<OpCode> = vec![
		// * Interrupts
		OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing),

		// * Transfer Instructions
		OpCode::new(0xAA, "TAX", 1, 2, AddressingMode::NoneAddressing),
		OpCode::new(0x8A, "TXA", 1, 2, AddressingMode::NoneAddressing),
		OpCode::new(0xA8, "TAY", 1, 2, AddressingMode::NoneAddressing),
		OpCode::new(0x98, "TYA", 1, 2, AddressingMode::NoneAddressing),

		// * Decrement & Increment Instructions
		OpCode::new(0xC6, "DEC", 2, 5, AddressingMode::ZeroPage),
		OpCode::new(0xD6, "DEC", 2, 6, AddressingMode::ZeroPage_X),
		OpCode::new(0xCE, "DEC", 3, 6, AddressingMode::Absolute),
		OpCode::new(0xDE, "DEC", 3, 7, AddressingMode::Absolute_X),
		OpCode::new(0xCA, "DEX", 1, 2, AddressingMode::NoneAddressing),
		OpCode::new(0x88, "DEY", 1, 2, AddressingMode::NoneAddressing),
		OpCode::new(0xE6, "INC", 2, 5, AddressingMode::ZeroPage),
		OpCode::new(0xF6, "INC", 2, 6, AddressingMode::ZeroPage_X),
		OpCode::new(0xEE, "INC", 3, 6, AddressingMode::Absolute),
		OpCode::new(0xFE, "INC", 3, 7, AddressingMode::Absolute_X),
		OpCode::new(0xE8, "INX", 1, 2, AddressingMode::NoneAddressing),
		OpCode::new(0xC8, "INY", 1, 2, AddressingMode::NoneAddressing),

		// * Load Instructions
		OpCode::new(0xA9, "LDA", 2, 2, AddressingMode::Immediate),
		OpCode::new(0xA5, "LDA", 2, 3, AddressingMode::ZeroPage),
		OpCode::new(0xB5, "LDA", 2, 4, AddressingMode::ZeroPage_X),
		OpCode::new(0xAD, "LDA", 3, 4, AddressingMode::Absolute),
		OpCode::new(0xBD, "LDA", 3, 4 /* +1 if page boundary crossed */, AddressingMode::Absolute_X),
		OpCode::new(0xB9, "LDA", 3, 4 /* +1 if page boundary crossed */, AddressingMode::Absolute_Y),
		OpCode::new(0xA1, "LDA", 2, 6, AddressingMode::Indirect_X),                                                   // ! Untested (Indirect_X)
		OpCode::new(0xB1, "LDA", 2, 5 /* +1 if page boundary crossed */, AddressingMode::Indirect_Y),                 // ! Untested (Indirect_Y)

		OpCode::new(0xA2, "LDX", 2, 2, AddressingMode::Immediate),
		OpCode::new(0xA6, "LDX", 2, 3, AddressingMode::ZeroPage),
		OpCode::new(0xB6, "LDX", 2, 4, AddressingMode::ZeroPage_Y),
		OpCode::new(0xAE, "LDX", 3, 4, AddressingMode::Absolute),
		OpCode::new(0xBE, "LDX", 3, 4 /* +1 if page boundary crossed */, AddressingMode::Absolute_Y),

		OpCode::new(0xA0, "LDY", 2, 2, AddressingMode::Immediate),
		OpCode::new(0xA4, "LDY", 2, 3, AddressingMode::ZeroPage),
		OpCode::new(0xB4, "LDY", 2, 4, AddressingMode::ZeroPage_X),
		OpCode::new(0xAC, "LDY", 3, 4, AddressingMode::Absolute),
		OpCode::new(0xBC, "LDY", 3, 4 /* +1 if page boundary crossed */, AddressingMode::Absolute_X),

		// * Store Instructions
		OpCode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage),
		OpCode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPage_X),
		OpCode::new(0x8D, "STA", 3, 4, AddressingMode::Absolute),
		OpCode::new(0x9D, "STA", 3, 5, AddressingMode::Absolute_X),
		OpCode::new(0x99, "STA", 3, 5, AddressingMode::Absolute_Y),
		OpCode::new(0x81, "STA", 2, 6, AddressingMode::Indirect_X),                                                   // ! Untested (Indirect_X)
		OpCode::new(0x91, "STA", 2, 6, AddressingMode::Indirect_Y),                                                   // ! Untested (Indirect_Y)

		OpCode::new(0x86, "STX", 2, 3, AddressingMode::ZeroPage),
		OpCode::new(0x96, "STX", 2, 4, AddressingMode::ZeroPage_Y),
		OpCode::new(0x8E, "STX", 3, 4, AddressingMode::Absolute),

		OpCode::new(0x84, "STY", 2, 3, AddressingMode::ZeroPage),
		OpCode::new(0x94, "STY", 2, 4, AddressingMode::ZeroPage_X),
		OpCode::new(0x8C, "STY", 3, 4, AddressingMode::Absolute),

		// * Logical Operation Instructions
		OpCode::new(0x29, "AND", 2, 2, AddressingMode::Immediate),
		OpCode::new(0x25, "AND", 2, 3, AddressingMode::ZeroPage),
		OpCode::new(0x35, "AND", 2, 4, AddressingMode::ZeroPage_X),
		OpCode::new(0x2D, "AND", 3, 4, AddressingMode::Absolute),
		OpCode::new(0x3D, "AND", 3, 4 /* +1 if page boundary crossed */, AddressingMode::Absolute_X),
		OpCode::new(0x39, "AND", 3, 4 /* +1 if page boundary crossed */, AddressingMode::Absolute_Y),
		OpCode::new(0x21, "AND", 2, 6, AddressingMode::Indirect_X),
		OpCode::new(0x31, "AND", 2, 5 /* +1 if page boundary crossed */, AddressingMode::Indirect_Y),

		// TODO: EOR
		// TODO: ORA

		// * Control Flow Instructions
		// * Branch Instructions
		OpCode::new(0x90, "BCC", 2, 2 /* +1/+2 if branch on same/different page */, AddressingMode::NoneAddressing),
		OpCode::new(0xB0, "BCS", 2, 2 /* +1/+2 if branch on same/different page */, AddressingMode::NoneAddressing),
		OpCode::new(0xF0, "BEQ", 2, 2 /* +1/+2 if branch on same/different page */, AddressingMode::NoneAddressing),
		OpCode::new(0x30, "BMI", 2, 2 /* +1/+2 if branch on same/different page */, AddressingMode::NoneAddressing),
		OpCode::new(0xD0, "BNE", 2, 2 /* +1/+2 if branch on same/different page */, AddressingMode::NoneAddressing),
		OpCode::new(0x10, "BPL", 2, 2 /* +1/+2 if branch on same/different page */, AddressingMode::NoneAddressing),
		OpCode::new(0x50, "BVC", 2, 2 /* +1/+2 if branch on same/different page */, AddressingMode::NoneAddressing),  // ! Untested
		OpCode::new(0x70, "BVS", 2, 2 /* +1/+2 if branch on same/different page */, AddressingMode::NoneAddressing),  // ! Untested

		// * Status Register Instructions
		OpCode::new(0x18, "CLC", 1, 2, AddressingMode::NoneAddressing),
		OpCode::new(0xD8, "CLD", 1, 2, AddressingMode::NoneAddressing),
		OpCode::new(0x58, "CLI", 1, 2, AddressingMode::NoneAddressing),
		OpCode::new(0xB8, "CLV", 1, 2, AddressingMode::NoneAddressing),

		OpCode::new(0x38, "SEC", 1, 2, AddressingMode::NoneAddressing),
		OpCode::new(0xF8, "SED", 1, 2, AddressingMode::NoneAddressing),
		OpCode::new(0x78, "SEI", 1, 2, AddressingMode::NoneAddressing),
	];

	pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
		let mut map = HashMap::new();

		for opcode in &*CPU_OPS_CODES {
			map.insert(opcode.code, opcode);
		}

		map
	};
}
