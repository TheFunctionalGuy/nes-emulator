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
    pub status: u8,
    pub program_counter: u16,
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            status: 0,
            program_counter: 0,
        }
    }

    fn lda(&mut self, value: u8) {
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

    pub fn interpret(&mut self, program: Vec<u8>) {
        self.program_counter = 0;

        loop {
            let opcode = program[self.program_counter as usize];
            self.program_counter += 1;

            match opcode {
                // LDA (0xA9)
                0xA9 => {
                    let param = program[self.program_counter as usize];
                    self.program_counter += 1;

                    self.lda(param);
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
        cpu.interpret(test_binary);

        assert_eq!(cpu.register_a, 0x05);
        // Z flag unset
        assert!(cpu.status & 0b0000_0010 == 0b00);
        // N flag unset
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();

        let test_binary = vec![0xA9, 0x00, 0x00];
        cpu.interpret(test_binary);

        // Z flag set
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.register_a = 10;

        let test_binary = vec![0xAA, 0x00];
        cpu.interpret(test_binary);

        assert_eq!(cpu.register_x, 10);
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.register_x = 0xFF;

        let test_binary = vec![0xE8, 0xE8, 0x00];
        cpu.interpret(test_binary);

        assert_eq!(cpu.register_x, 1);
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();

        let test_binary = vec![0xA9, 0xC0, 0xAA, 0xE8, 0x00];
        cpu.interpret(test_binary);

        assert_eq!(cpu.register_x, 0xC1);
    }
}
