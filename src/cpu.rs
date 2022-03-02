/// CPU emulator for 6502/2A03
// Constants
// Bit masks for all flags and inverted masks
// Flag bit order: N(egative) V 1 B D I Z(ero) C(arry)
const NEGATIVE_FLAG: u8 = 0b1000_0000;
const NEGATIVE_FLAG_INV: u8 = 0b0111_1111;
const ZERO_FLAG: u8 = 0b0000_0010;
const ZERO_FLAG_INV: u8 = 0b1111_1101;

pub struct CPU {
	pub register_a: u8,
	pub register_x: u8,
	pub register_y: u8,
	pub status: u8,
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

impl CPU {
	pub fn new() -> Self {
		CPU {
			register_a: 0,
			register_x: 0,
			register_y: 0,
			status: 0,
			program_counter: 0,
			memory: [0; 0xFFFF],
		}
	}

	// TODO: Move to trait
	// Memory methods
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

	// CPU operations
	pub fn load(&mut self, program: Vec<u8>) {
		self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
		self.mem_write_u16(0xFFFC, 0x8000);
	}

	pub fn reset(&mut self) {
		// Reset all registers
		self.register_a = 0;
		self.register_x = 0;
		self.register_y = 0;
		self.status = 0;

		// Initialize program counter to 2-byte value at 0xFFFC
		self.program_counter = self.mem_read_u16(0xFFFC);
	}

	pub fn load_and_run(&mut self, program: Vec<u8>) {
		self.load(program);
		self.reset();
		self.run();
	}

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

	// Instructions
	fn lda(&mut self, mode: &AddressingMode) {
		let addr = self.get_operand_address(mode);
		let value = self.mem_read(addr);

		self.register_a = value;
		self.update_zero_and_negative_flags(self.register_a);
	}

	fn tax(&mut self) {
		self.register_x = self.register_a;
		self.update_zero_and_negative_flags(self.register_x);
	}

	fn inx(&mut self) {
		self.register_x = self.register_x.wrapping_add(1);
		self.update_zero_and_negative_flags(self.register_x);
	}

	fn update_zero_and_negative_flags(&mut self, result: u8) {
		if result == 0 {
			self.status |= ZERO_FLAG;
		} else {
			self.status &= ZERO_FLAG_INV;
		}

		if result & NEGATIVE_FLAG != 0 {
			self.status |= NEGATIVE_FLAG;
		} else {
			self.status &= NEGATIVE_FLAG_INV;
		}
	}

	pub fn run(&mut self) {
		loop {
			let opcode = self.mem_read(self.program_counter);
			self.program_counter += 1;

			match opcode {
				// LDA Zero Page (0xA5)
				0xA5 => {
					self.lda(&AddressingMode::ZeroPage);
					self.program_counter += 1;
				}
				// LDA Immediate (0xA9)
				0xA9 => {
					self.lda(&AddressingMode::Immediate);
					self.program_counter += 1;
				}
				// LDA Absolute (0xAD)
				0xAD => {
					self.lda(&AddressingMode::Absolute);
					self.program_counter += 2;
				}
				// TAX (0xAA)
				0xAA => self.tax(),
				// INX (0xE8)
				0xE8 => self.inx(),
				// BRK (0x00)
				0x00 => {
					return;
				}
				_ => todo!("opcode not yet implemented!"),
			}
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_0xa9_lda_immediate_load_data() {
		let mut cpu = CPU::new();

		let test_binary = vec![0xA9, 0x05, 0x00];
		cpu.load_and_run(test_binary);

		assert_eq!(cpu.register_a, 0x05);
		// Z flag unset
		assert!(cpu.status & 0b0000_0010 == 0b00);
		// N flag unset
		assert!(cpu.status & 0b1000_0000 == 0);
	}

	#[test]
	fn test_0xa5_lda_from_memory() {
		let mut cpu = CPU::new();
		cpu.mem_write(0x10, 0x55);

		let test_binary = vec![0xA5, 0x10, 0x00];
		cpu.load_and_run(test_binary);

		assert_eq!(cpu.register_a, 0x55);
	}

	#[test]
	fn test_0xa9_lda_zero_flag() {
		let mut cpu = CPU::new();

		let test_binary = vec![0xA9, 0x00, 0x00];
		cpu.load_and_run(test_binary);

		// Z flag set
		assert!(cpu.status & 0b0000_0010 == 0b10);
	}

	#[test]
	fn test_0xaa_tax_move_a_to_x() {
		let mut cpu = CPU::new();

		let test_binary = vec![0xA9, 0x0A, 0xAA, 0x00];
		cpu.load_and_run(test_binary);

		assert_eq!(cpu.register_x, 10);
	}

	#[test]
	fn test_0xad_lda_from_memory() {
		let mut cpu = CPU::new();
		cpu.mem_write(0x0102, 0xFF);

		let test_binary = vec![0xAD, 0x02, 0x01, 0x00];
		cpu.load_and_run(test_binary);

		assert_eq!(cpu.register_a, 0xFF);
	}

	#[test]
	fn test_inx_overflow() {
		let mut cpu = CPU::new();

		let test_binary = vec![0xA9, 0xFF, 0xAA, 0xE8, 0xE8, 0x00];
		cpu.load_and_run(test_binary);

		assert_eq!(cpu.register_x, 1);
	}

	#[test]
	fn test_5_ops_working_together() {
		let mut cpu = CPU::new();

		let test_binary = vec![0xA9, 0xC0, 0xAA, 0xE8, 0x00];
		cpu.load_and_run(test_binary);

		assert_eq!(cpu.register_x, 0xC1);
	}
}
