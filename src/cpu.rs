//! CPU emulator for 6502/2A03 processor
use crate::opcodes::OPCODES_MAP;
use bitflags::bitflags;

// Bit masks for all flags
// 7 6 5 4 3 2 1 0
// ---------------
// N V s s D I Z C
// | | | | | | | |
// | | | | | | | +- Carry
// | | | | | | +--- Zero
// | | | | | +----- Interrupt Disable
// | | | | +------- Decimal
// | | | +--------- No CPU effect (B2 flag)
// | | +----------- No CPU effect (B1 flag)
// | +------------- Overflow
// +--------------- Negative
bitflags! {
	pub struct Flags: u8 {
		const NEGATIVE          = 0b1000_0000;
		const OVERFLOW          = 0b0100_0000;
		const B1                = 0b0010_0000;
		const B2                = 0b0001_0000;
		const DECIMAL           = 0b0000_1000;
		const INTERRUPT_DISABLE = 0b0000_0100;
		const ZERO              = 0b0000_0010;
		const CARRY             = 0b0000_0001;
	}
}

#[allow(clippy::upper_case_acronyms)]
pub struct CPU {
	pub register_a: u8,
	pub register_x: u8,
	pub register_y: u8,
	pub status: Flags,
	pub program_counter: u16,
	memory: [u8; 0xFFFF],
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
	Immediate,
	ZeroPage,
	ZeroPage_X,
	ZeroPage_Y,
	Absolute,
	Absolute_X,
	Absolute_Y,
	Indirect_X,
	Indirect_Y,
	NoneAddressing,
}

// * Memory Operations
trait Mem {
	fn mem_read(&self, addr: u16) -> u8;
	fn mem_read_u16(&self, addr: u16) -> u16;
	fn mem_write(&mut self, addr: u16, data: u8);
	fn mem_write_u16(&mut self, addr: u16, data: u16);
}

impl Mem for CPU {
	fn mem_read(&self, addr: u16) -> u8 {
		self.memory[addr as usize]
	}

	fn mem_read_u16(&self, addr: u16) -> u16 {
		// * Alternative implementation
		// u16::from_le_bytes([self.mem_read(addr), self.mem_read(addr + 1)])

		// * Reference implementation
		let lo = self.mem_read(addr) as u16;
		let hi = self.mem_read(addr + 1) as u16;

		(hi << 8) | lo
	}

	fn mem_write(&mut self, addr: u16, data: u8) {
		self.memory[addr as usize] = data;
	}

	fn mem_write_u16(&mut self, addr: u16, data: u16) {
		// * Alternative implementation
		// self.mem_write(addr, data.to_le_bytes()[0]);
		// self.mem_write(addr + 1, data.to_le_bytes()[1]);

		// * Reference implementation
		let hi = (data >> 8) as u8;
		let lo = (data & 0xFF) as u8;

		self.mem_write(addr, lo);
		self.mem_write(addr + 1, hi);
	}
}

impl CPU {
	pub fn new() -> Self {
		CPU {
			register_a: 0,
			register_x: 0,
			register_y: 0,
			status: Flags::empty(),
			program_counter: 0,
			memory: [0; 0xFFFF],
		}
	}

	// * CPU Operations
	pub fn load(&mut self, program: Vec<u8>) {
		self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
		self.mem_write_u16(0xFFFC, 0x8000);
	}

	pub fn reset(&mut self) {
		// Reset all registers
		self.register_a = 0;
		self.register_x = 0;
		self.register_y = 0;
		// TODO: Determine correct starting flags
		self.status = Flags::from_bits_truncate(0b0000_0000);

		// Initialize program counter to 2-byte value at 0xFFFC
		self.program_counter = self.mem_read_u16(0xFFFC);
	}

	pub fn load_and_run(&mut self, program: Vec<u8>) {
		self.load(program);
		self.reset();
		self.run();
	}

	// * Helper Operations
	fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
		match mode {
			// * Immediate
			AddressingMode::Immediate => self.program_counter,

			// * Zero Page (1-byte address)
			AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
			AddressingMode::ZeroPage_X => {
				let base = self.mem_read(self.program_counter);

				// Offset 1-byte base address by adding value of register X
				base.wrapping_add(self.register_x) as u16
			}
			AddressingMode::ZeroPage_Y => {
				let base = self.mem_read(self.program_counter);

				// Offset 1-byte base address by adding value of register Y
				base.wrapping_add(self.register_y) as u16
			}

			// * Absolute (2-byte address)
			AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
			AddressingMode::Absolute_X => {
				let base = self.mem_read_u16(self.program_counter);

				// Offset 2-byte base address by adding value of register X
				base.wrapping_add(self.register_x as u16)
			}
			AddressingMode::Absolute_Y => {
				let base = self.mem_read_u16(self.program_counter);

				// Offset 2-byte base address by adding value of register Y
				base.wrapping_add(self.register_y as u16)
			}

			// * Indirect
			AddressingMode::Indirect_X => {
				todo!("Indirect_X addressing not yet implemented!");
			}
			AddressingMode::Indirect_Y => {
				todo!("Indirect_Y addressing not yet implemented!");
			}

			// * Not supported
			AddressingMode::NoneAddressing => {
				panic!("mode {:?} is not supported", mode);
			}
		}
	}

	// * Instructions
	// * Transfer Instructions
	fn tax(&mut self) {
		self.register_x = self.register_a;
		self.update_zero_and_negative_flags(self.register_x);
	}

	fn txa(&mut self) {
		self.register_a = self.register_x;
		self.update_zero_and_negative_flags(self.register_a);
	}

	fn tay(&mut self) {
		self.register_y = self.register_a;
		self.update_zero_and_negative_flags(self.register_y);
	}

	fn tya(&mut self) {
		self.register_a = self.register_y;
		self.update_zero_and_negative_flags(self.register_a);
	}

	// * Load Instructions
	fn lda(&mut self, mode: &AddressingMode) {
		let addr = self.get_operand_address(mode);
		let value = self.mem_read(addr);

		self.register_a = value;
		self.update_zero_and_negative_flags(self.register_a);
	}

	fn ldx(&mut self, mode: &AddressingMode) {
		let addr = self.get_operand_address(mode);
		let value = self.mem_read(addr);

		self.register_x = value;
		self.update_zero_and_negative_flags(self.register_x);
	}

	fn ldy(&mut self, mode: &AddressingMode) {
		let addr = self.get_operand_address(mode);
		let value = self.mem_read(addr);

		self.register_y = value;
		self.update_zero_and_negative_flags(self.register_y);
	}

	// * Store Instructions
	fn sta(&mut self, mode: &AddressingMode) {
		let addr = self.get_operand_address(mode);

		self.mem_write(addr, self.register_a);
	}

	fn stx(&mut self, mode: &AddressingMode) {
		let addr = self.get_operand_address(mode);

		self.mem_write(addr, self.register_x);
	}

	fn sty(&mut self, mode: &AddressingMode) {
		let addr = self.get_operand_address(mode);

		self.mem_write(addr, self.register_y);
	}

	// * Decrement & Increment Instructions
	fn dec(&mut self, mode: &AddressingMode) {
		let addr = self.get_operand_address(mode);
		let value = self.mem_read(addr);
		let decreased_value = value.wrapping_sub(1);

		self.mem_write(addr, decreased_value);
		self.update_zero_and_negative_flags(decreased_value);
	}

	fn dex(&mut self) {
		self.register_x = self.register_x.wrapping_sub(1);
		self.update_zero_and_negative_flags(self.register_x);
	}

	fn dey(&mut self) {
		self.register_y = self.register_y.wrapping_sub(1);
		self.update_zero_and_negative_flags(self.register_y);
	}

	fn inc(&mut self, mode: &AddressingMode) {
		let addr = self.get_operand_address(mode);
		let value = self.mem_read(addr);
		let increased_value = value.wrapping_add(1);

		self.mem_write(addr, increased_value);
		self.update_zero_and_negative_flags(increased_value);
	}

	fn inx(&mut self) {
		self.register_x = self.register_x.wrapping_add(1);
		self.update_zero_and_negative_flags(self.register_x);
	}

	fn iny(&mut self) {
		self.register_y = self.register_y.wrapping_add(1);
		self.update_zero_and_negative_flags(self.register_y);
	}

	// * Logical Operations
	fn logical_instruction<F: Fn(u8, u8) -> u8>(&mut self, mode: &AddressingMode, f: F) {
		let addr = self.get_operand_address(mode);
		let value = self.mem_read(addr);

		self.register_a = f(self.register_a, value);
		self.update_zero_and_negative_flags(self.register_a);
	}

	fn and(&mut self, mode: &AddressingMode) {
		let addr = self.get_operand_address(mode);
		let value = self.mem_read(addr);

		self.register_a &= value;
		self.update_zero_and_negative_flags(self.register_a);
	}

	fn eor(&mut self, mode: &AddressingMode) {
		let addr = self.get_operand_address(mode);
		let value = self.mem_read(addr);

		self.register_a ^= value;
		self.update_zero_and_negative_flags(self.register_a);
	}

	fn ora(&mut self, mode: &AddressingMode) {
		let addr = self.get_operand_address(mode);
		let value = self.mem_read(addr);

		self.register_a |= value;
		self.update_zero_and_negative_flags(self.register_a);
	}

	// * Conditional Branch Instructions
	fn branch(&mut self, condition: bool) {
		if condition {
			let offset = self.mem_read(self.program_counter) as u16;
			let jump_target = self.program_counter.wrapping_add(offset);

			self.program_counter = jump_target;
		}
	}

	// * Helper function for instruction
	fn update_zero_and_negative_flags(&mut self, result: u8) {
		if result == 0 {
			self.status.insert(Flags::ZERO);
		} else {
			self.status.remove(Flags::ZERO);
		}

		if result & Flags::NEGATIVE.bits != 0 {
			self.status.insert(Flags::NEGATIVE);
		} else {
			self.status.remove(Flags::NEGATIVE);
		}
	}

	// TODO: Maybe helper function for arithmetics which update A register

	pub fn run(&mut self) {
		let opcodes = &(*OPCODES_MAP);

		loop {
			let code = self.mem_read(self.program_counter);
			let operation = opcodes
				.get(&code)
				.unwrap_or_else(|| panic!("Opcode 0x{:02X} not yet implemented!", code));
			self.program_counter += 1;

			match code {
				// * Transfer Instructions
				0xAA => self.tax(),
				0x8A => self.txa(),
				0xA8 => self.tay(),
				0x98 => self.tya(),

				// * Load Instructions
				// * LDA
				0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => {
					self.lda(&operation.addressing_mode);
				}
				// * LDX
				0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => {
					self.ldx(&operation.addressing_mode);
				}
				// * LDY
				0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => {
					self.ldy(&operation.addressing_mode);
				}

				// * Store Instructions
				// * STA
				0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => {
					self.sta(&operation.addressing_mode);
				}
				// * STX
				0x86 | 0x96 | 0x8E => {
					self.stx(&operation.addressing_mode);
				}
				// * STY
				0x84 | 0x94 | 0x8C => {
					self.sty(&operation.addressing_mode);
				}

				// * Decrement & Increment Instructions
				// * DEC
				0xC6 | 0xD6 | 0xCE | 0xDE => {
					self.dec(&operation.addressing_mode);
				}
				0xCA => self.dex(),
				0x88 => self.dey(),
				// * INC
				0xE6 | 0xF6 | 0xEE | 0xFE => {
					self.inc(&operation.addressing_mode);
				}
				0xE8 => self.inx(),
				0xC8 => self.iny(),

				// * Logical Operations
				// * AND
				0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => {
					self.and(&operation.addressing_mode);
				}
				// * EOR
				0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => {
					self.eor(&operation.addressing_mode);
				}
				// * ORA
				0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => {
					self.ora(&operation.addressing_mode);
				}

				// * Status Register Instructions
				// * CLC
				0x18 => self.status.remove(Flags::CARRY),
				// * CLD
				0xD8 => self.status.remove(Flags::DECIMAL),
				// * CLI
				0x58 => self.status.remove(Flags::INTERRUPT_DISABLE),
				// * CLV
				0xB8 => self.status.remove(Flags::OVERFLOW),

				// * SEC
				0x38 => self.status.insert(Flags::CARRY),
				// * SED
				0xF8 => self.status.insert(Flags::DECIMAL),
				// * SEI
				0x78 => self.status.insert(Flags::INTERRUPT_DISABLE),

				// * Conditional Branch Instructions
				0x90 => self.branch(!self.status.contains(Flags::CARRY)),
				0xB0 => self.branch(self.status.contains(Flags::CARRY)),
				0xF0 => self.branch(self.status.contains(Flags::ZERO)),
				0x30 => self.branch(self.status.contains(Flags::NEGATIVE)),
				0xD0 => self.branch(!self.status.contains(Flags::ZERO)),
				0x10 => self.branch(!self.status.contains(Flags::NEGATIVE)),
				0x50 => self.branch(!self.status.contains(Flags::OVERFLOW)),
				0x70 => self.branch(self.status.contains(Flags::OVERFLOW)),

				// * Interrupts
				// TODO: Set interrupt bit
				0x00 => return,

				// * Error case
				_ => todo!("Operation with opcode 0x{:02x} not yet implemented", code),
			}

			if operation.length > 1 {
				self.program_counter += (operation.length - 1) as u16;
			}
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	// * Transfer Instruction Tests
	#[test]
	fn test_transfer_0xaa_tax_move_a_to_x() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A9 0A                LDA #$0A
		0002   AA                   TAX
		0003   00                   BRK */
		let binary = vec![0xA9, 0x0A, 0xAA, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_x, 0x0A);
	}

	#[test]
	fn test_transfer_0x8a_txa_move_x_to_a() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A2 0A                LDX #$0A
		0002   8A                   TXA
		0003   00                   BRK */
		let binary = vec![0xA2, 0x0A, 0x8A, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_a, 0x0A);
	}

	#[test]
	fn test_transfer_0xa8_tay_move_a_to_y() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A9 0A                LDA #$0A
		0002   A8                   TAY
		0003   00                   BRK */
		let binary = vec![0xA9, 0x0A, 0xA8, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_y, 0x0A);
	}

	#[test]
	fn test_transfer_0x98_tya_move_y_to_a() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A0 0A                LDY #$0A
		0002   98                   TYA
		0003   00                   BRK */
		let binary = vec![0xA0, 0x0A, 0x98, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_a, 0x0A);
	}

	// * Load Instruction Tests
	#[test]
	fn test_load_0xa9_lda_immediate_flags() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A9 05                LDA #$05
		0002   00                   BRK */
		let binary = vec![0xA9, 0x05, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_a, 0x05);
		// Z flag unset
		assert!(!cpu.status.contains(Flags::ZERO));
		// N flag unset
		assert!(!cpu.status.contains(Flags::NEGATIVE));
	}

	#[test]
	fn test_load_0xa9_lda_zero_flag() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A9 00                LDA #$00
		0002   00                   BRK */
		let binary = vec![0xA9, 0x00, 0x00];
		cpu.load_and_run(binary);

		// Z flag set
		assert!(cpu.status.contains(Flags::ZERO));
	}

	#[test]
	fn test_load_0xa5_lda_zero_page() {
		let mut cpu = CPU::new();
		cpu.mem_write(0x10, 0x55);

		/* Disassembly:
		0000   A5 10                LDA $10
		0002   00                   BRK */
		let binary = vec![0xA5, 0x10, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_a, 0x55);
	}

	#[test]
	fn test_load_0xa6_ldx_zero_page() {
		let mut cpu = CPU::new();
		cpu.mem_write(0x00AF, 0x1F);

		/* Disassembly:
		0000   A6 AF                LDX $AF
		0002   00                   BRK */
		let binary = vec![0xA6, 0xAF, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_x, 0x1F);
	}

	#[test]
	fn test_load_0xad_lda_from_memory() {
		let mut cpu = CPU::new();
		cpu.mem_write(0x0102, 0xFF);

		/* Disassembly:
		0000   AD 02 01             LDA $0102
		0003   00                   BRK */
		let binary = vec![0xAD, 0x02, 0x01, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_a, 0xFF);
	}

	#[test]
	fn test_load_0xb6_ldx_zero_page_y() {
		let mut cpu = CPU::new();
		cpu.mem_write(0x0012, 0xBA);

		/* Disassembly:
		0000   A0 13                LDY #$13
		0002   B6 FF                LDX $FF,Y
		0004   00                   BRK */
		let binary = vec![0xA0, 0x13, 0xB6, 0xFF, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_x, 0xBA);
	}

	#[test]
	fn test_load_0xbc_ldy_absolute_x() {
		let mut cpu = CPU::new();
		cpu.mem_write(0x4157, 0x05);

		/* Disassembly:
		0000   A2 15                LDX #$15
		0002   BC 42 41             LDY $4142,X
		0005   00                   BRK */
		let binary = vec![0xA2, 0x15, 0xBC, 0x42, 0x41, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_y, 0x05);
	}

	// * Store Instruction Tests
	#[test]
	fn test_store_0x85_sta_zero_page() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A9 20                LDA #$20
		0002   85 01                STA $01
		0004   00                   BRK */
		let binary = vec![0xA9, 0x20, 0x85, 0x01, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.memory[0x01], 0x20);
	}

	#[test]
	fn test_store_0x95_sta_zero_page_x() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A9 20                LDA #$20
		0002   AA                   TAX
		0003   A9 40                LDA #$40
		0005   95 01                STA $01,X
		0007   00                   BRK */
		let binary = vec![0xA9, 0x20, 0xAA, 0xA9, 0x40, 0x95, 0x01, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.memory[0x21], 0x40);
	}

	#[test]
	fn test_store_0x99_sta_absolute_y() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A0 F1                LDY #$F1
		0002   98                   TYA
		0003   99 34 12             STA $1234,Y
		0006   00                   BRK */
		let binary = vec![0xA0, 0xF1, 0x98, 0x99, 0x34, 0x12, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.memory[0x1325], 0xF1);
	}

	#[test]
	fn test_store_0x86_stx_zero_page() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A2 87                LDX #$87
		0002   86 13                STX $13
		0004   00                   BRK */
		let binary = vec![0xA2, 0x87, 0x86, 0x13, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.memory[0x0013], 0x87);
	}

	#[test]
	fn test_store_0x84_sty_zero_page() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A0 11                LDY #$11
		0002   84 17                STY $17
		0004   00                   BRK */
		let binary = vec![0xA0, 0x11, 0x84, 0x17, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.memory[0x0017], 0x11);
	}

	// * Decrement & Increment Instruction Tests
	// TODO: Test DEC & INC instructions
	#[test]
	fn test_dec_inc_0xca_dex_underflow() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A2 01                LDX #$01
		0002   CA                   DEX
		0003   CA                   DEX
		0004   00                   BRK */
		let binary = vec![0xA2, 0x01, 0xCA, 0xCA, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_x, 0xFF);
	}

	#[test]
	fn test_dec_inc_0xe8_inx_overflow() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A9 FF                LDA #$FF
		0002   AA                   TAX
		0003   E8                   INX
		0004   E8                   INX
		0005   00                   BRK */
		let binary = vec![0xA9, 0xFF, 0xAA, 0xE8, 0xE8, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_x, 0x01);
	}

	#[test]
	fn test_dec_inc_0x88_dey_underflow() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A0 01                LDY #$01
		0002   88                   DEY
		0003   88                   DEY
		0004   00                   BRK */
		let binary = vec![0xA0, 0x01, 0x88, 0x88, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_y, 0xFF);
	}

	#[test]
	fn test_dec_inc_0xc8_iny_overflow() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A0 FF                LDY #$FF
		0002   C8                   INY
		0003   C8                   INY
		0004   00                   BRK */
		let binary = vec![0xA0, 0xFF, 0xC8, 0xC8, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_y, 0x01);
	}

	// * TODO: Logical Operation Tests

	// * Status Register Instruction Tests
	#[test]
	fn test_flag_0x18_clc() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   18                   CLC
		0001   00                   BRK */
		let binary = vec![0x18, 0x00];
		// Prepare CPU state
		cpu.load(binary);
		cpu.reset();
		cpu.status.insert(Flags::CARRY);
		cpu.run();

		assert!(!cpu.status.contains(Flags::CARRY));
	}

	#[test]
	fn test_flag_0xd8_cld() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   D8                   CLD
		0001   00                   BRK */
		let binary = vec![0xD8, 0x00];
		// Prepare CPU state
		cpu.load(binary);
		cpu.reset();
		cpu.status.insert(Flags::DECIMAL);
		cpu.run();

		assert!(!cpu.status.contains(Flags::DECIMAL));
	}

	#[test]
	fn test_flag_0x58_cli() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   58                   CLI
		0001   00                   BRK */
		let binary = vec![0x58, 0x00];
		// Prepare CPU state
		cpu.load(binary);
		cpu.reset();
		cpu.status.insert(Flags::INTERRUPT_DISABLE);
		cpu.run();

		assert!(!cpu.status.contains(Flags::INTERRUPT_DISABLE));
	}

	#[test]
	fn test_flag_0xb8_clv() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   B8                   CLV
		0001   00                   BRK */
		let binary = vec![0xB8];
		// Prepare CPU state
		cpu.load(binary);
		cpu.reset();
		cpu.status.insert(Flags::OVERFLOW);
		cpu.run();

		assert!(!cpu.status.contains(Flags::OVERFLOW));
	}

	#[test]
	fn test_flag_0x38_sec() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   38                   SEC
		0001   00                   BRK */
		let binary = vec![0x38, 0x00];
		cpu.load_and_run(binary);

		assert!(cpu.status.contains(Flags::CARRY));
	}

	#[test]
	fn test_flag_0xf8_sed() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   F8                   SED
		0001   00                   BRK */
		let binary = vec![0xF8, 0x00];
		cpu.load_and_run(binary);

		assert!(cpu.status.contains(Flags::DECIMAL));
	}

	#[test]
	fn test_flag_0x78_sei() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   78                   SEI
		0001   00                   BRK */
		let binary = vec![0x78, 0x00];
		cpu.load_and_run(binary);

		assert!(cpu.status.contains(Flags::INTERRUPT_DISABLE));
	}

	// * Conditional Branch Instruction Tests
	#[test]
	fn test_branch_0x90_bcc() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   18                   CLC
		0001   90 03                BCC L0006
		0003   A9 42                LDA #$42
		0005   00                   BRK
		0006   A9 24      L0006     LDA #$24
		0008   00                   BRK */
		let binary = vec![0x18, 0x90, 0x03, 0xA9, 0x42, 0x00, 0xA9, 0x24, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_a, 0x24);
	}

	#[test]
	fn test_branch_0xb0_bcs() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   38                   SEC
		0001   B0 03                BCS L0006
		0003   A9 42                LDA #$42
		0005   00                   BRK
		0006   A9 24      L0006     LDA #$24
		0008   00                   BRK */
		let binary = vec![0x38, 0xB0, 0x03, 0xA9, 0x42, 0x00, 0xA9, 0x24, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_a, 0x24);
	}

	#[test]
	fn test_branch_0xf0_beq() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A2 00                LDX #$00
		0002   F0 03                BEQ L0007
		0004   A9 42                LDA #$42
		0006   00                   BRK
		0007   A9 24      L0007     LDA #$24
		0009   00                   BRK */
		let binary = vec![0xA2, 0x00, 0xF0, 0x03, 0xA9, 0x42, 0x00, 0xA9, 0x24, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_a, 0x24);
	}

	#[test]
	fn test_branch_0xd0_bne() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A2 00                LDX #$00
		0002   D0 03                BNE L0007
		0004   A9 42                LDA #$42
		0006   00                   BRK
		0007   A9 24      L0007     LDA #$24
		0009   00                   BRK */
		let binary = vec![0xA2, 0x00, 0xD0, 0x03, 0xA9, 0x42, 0x00, 0xA9, 0x24, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_a, 0x42);
	}

	#[test]
	fn test_branch_0x30_bmi() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A2 FF                LDX #$FF
		0002   30 03                BMI L0007
		0004   A9 42                LDA #$42
		0006   00                   BRK
		0007   A9 24      L0007     LDA #$24
		0009   00                   BRK */
		let binary = vec![0xA2, 0xFF, 0x30, 0x03, 0xA9, 0x42, 0x00, 0xA9, 0x24, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_a, 0x24);
	}

	#[test]
	fn test_branch_0x10_bpl() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A2 FF                LDX #$FF
		0002   10 03                BPL L0007
		0004   A9 42                LDA #$42
		0006   00                   BRK
		0007   A9 24      L0007     LDA #$24
		0009   00                   BRK */
		let binary = vec![0xA2, 0xFF, 0x10, 0x03, 0xA9, 0x42, 0x00, 0xA9, 0x24, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_a, 0x42);
	}

	// * General Tests
	#[test]
	fn test_5_ops_working_together() {
		let mut cpu = CPU::new();

		/* Disassembly:
		0000   A9 C0                LDA #$C0
		0002   AA                   TAX
		0003   E8                   INX
		0004   00                   BRK */
		let binary = vec![0xA9, 0xC0, 0xAA, 0xE8, 0x00];
		cpu.load_and_run(binary);

		assert_eq!(cpu.register_x, 0xC1);
	}

	#[test]
	fn test_snake_opcodes_supported() {
		let mut cpu = CPU::new();

		let game_code = vec![
			0x20, 0x06, 0x06, 0x20, 0x38, 0x06, 0x20, 0x0d, 0x06, 0x20, 0x2a, 0x06, 0x60, 0xa9,
			0x02, 0x85, 0x02, 0xa9, 0x04, 0x85, 0x03, 0xa9, 0x11, 0x85, 0x10, 0xa9, 0x10, 0x85,
			0x12, 0xa9, 0x0f, 0x85, 0x14, 0xa9, 0x04, 0x85, 0x11, 0x85, 0x13, 0x85, 0x15, 0x60,
			0xa5, 0xfe, 0x85, 0x00, 0xa5, 0xfe, 0x29, 0x03, 0x18, 0x69, 0x02, 0x85, 0x01, 0x60,
			0x20, 0x4d, 0x06, 0x20, 0x8d, 0x06, 0x20, 0xc3, 0x06, 0x20, 0x19, 0x07, 0x20, 0x20,
			0x07, 0x20, 0x2d, 0x07, 0x4c, 0x38, 0x06, 0xa5, 0xff, 0xc9, 0x77, 0xf0, 0x0d, 0xc9,
			0x64, 0xf0, 0x14, 0xc9, 0x73, 0xf0, 0x1b, 0xc9, 0x61, 0xf0, 0x22, 0x60, 0xa9, 0x04,
			0x24, 0x02, 0xd0, 0x26, 0xa9, 0x01, 0x85, 0x02, 0x60, 0xa9, 0x08, 0x24, 0x02, 0xd0,
			0x1b, 0xa9, 0x02, 0x85, 0x02, 0x60, 0xa9, 0x01, 0x24, 0x02, 0xd0, 0x10, 0xa9, 0x04,
			0x85, 0x02, 0x60, 0xa9, 0x02, 0x24, 0x02, 0xd0, 0x05, 0xa9, 0x08, 0x85, 0x02, 0x60,
			0x60, 0x20, 0x94, 0x06, 0x20, 0xa8, 0x06, 0x60, 0xa5, 0x00, 0xc5, 0x10, 0xd0, 0x0d,
			0xa5, 0x01, 0xc5, 0x11, 0xd0, 0x07, 0xe6, 0x03, 0xe6, 0x03, 0x20, 0x2a, 0x06, 0x60,
			0xa2, 0x02, 0xb5, 0x10, 0xc5, 0x10, 0xd0, 0x06, 0xb5, 0x11, 0xc5, 0x11, 0xf0, 0x09,
			0xe8, 0xe8, 0xe4, 0x03, 0xf0, 0x06, 0x4c, 0xaa, 0x06, 0x4c, 0x35, 0x07, 0x60, 0xa6,
			0x03, 0xca, 0x8a, 0xb5, 0x10, 0x95, 0x12, 0xca, 0x10, 0xf9, 0xa5, 0x02, 0x4a, 0xb0,
			0x09, 0x4a, 0xb0, 0x19, 0x4a, 0xb0, 0x1f, 0x4a, 0xb0, 0x2f, 0xa5, 0x10, 0x38, 0xe9,
			0x20, 0x85, 0x10, 0x90, 0x01, 0x60, 0xc6, 0x11, 0xa9, 0x01, 0xc5, 0x11, 0xf0, 0x28,
			0x60, 0xe6, 0x10, 0xa9, 0x1f, 0x24, 0x10, 0xf0, 0x1f, 0x60, 0xa5, 0x10, 0x18, 0x69,
			0x20, 0x85, 0x10, 0xb0, 0x01, 0x60, 0xe6, 0x11, 0xa9, 0x06, 0xc5, 0x11, 0xf0, 0x0c,
			0x60, 0xc6, 0x10, 0xa5, 0x10, 0x29, 0x1f, 0xc9, 0x1f, 0xf0, 0x01, 0x60, 0x4c, 0x35,
			0x07, 0xa0, 0x00, 0xa5, 0xfe, 0x91, 0x00, 0x60, 0xa6, 0x03, 0xa9, 0x00, 0x81, 0x10,
			0xa2, 0x00, 0xa9, 0x01, 0x81, 0x10, 0x60, 0xa2, 0x00, 0xea, 0xea, 0xca, 0xd0, 0xfb,
			0x60,
		];
		cpu.load_and_run(game_code);
	}
}
