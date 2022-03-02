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
		// * None addressing instructions
		OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing),
		OpCode::new(0xAA, "TAX", 1, 2, AddressingMode::NoneAddressing),
		OpCode::new(0xE8, "INX", 1, 2, AddressingMode::NoneAddressing),
		// * STA
		OpCode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage),
		OpCode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPage_X),
		OpCode::new(0x8D, "STA", 3, 4, AddressingMode::Absolute),           // ! Untested
		OpCode::new(0x9D, "STA", 3, 5, AddressingMode::Absolute_X),         // ! Untested
		OpCode::new(0x99, "STA", 3, 5, AddressingMode::Absolute_Y),         // ! Untested
		OpCode::new(0x81, "STA", 2, 6, AddressingMode::Indirect_X),         // ! Untested
		OpCode::new(0x91, "STA", 2, 6, AddressingMode::Indirect_Y),         // ! Untested
		// * LDA
		OpCode::new(0xA9, "LDA", 2, 2, AddressingMode::Immediate),
		OpCode::new(0xA5, "LDA", 2, 3, AddressingMode::ZeroPage),
		OpCode::new(0xB5, "LDA", 2, 4, AddressingMode::ZeroPage_X),         // ! Untested
		OpCode::new(0xAD, "LDA", 3, 4, AddressingMode::Absolute),
		OpCode::new(0xBD, "LDA", 3, 4 /*+1*/, AddressingMode::Absolute_X),  // ! Untested
		OpCode::new(0xB9, "LDA", 3, 4 /*+1*/, AddressingMode::Absolute_Y),  // ! Untested
		OpCode::new(0xA1, "LDA", 2, 6, AddressingMode::Indirect_X),         // ! Untested
		OpCode::new(0xB1, "LDA", 2, 5 /*+1*/, AddressingMode::Indirect_Y),  // ! Untested
	];

	pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
		let mut map = HashMap::new();

		for opcode in &*CPU_OPS_CODES {
			map.insert(opcode.code, opcode);
		}

		map
	};
}
