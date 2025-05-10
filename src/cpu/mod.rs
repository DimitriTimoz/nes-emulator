// https://www.nesdev.org/wiki/Instruction_reference
// 6502 CPU
// https://www.nesdev.org/wiki/Emulator_tests Tests

pub mod memory;
use crate::{cpu::memory::Memory, Ines};
const CPU_FREQUENCY: usize = 1_789_773; 

pub struct StatusFlag(u8);

impl StatusFlag {
    fn set(&mut self, flag: StatusFlags) {
        self.0 |= flag as u8;
    }

    fn set_state(&mut self, flag: StatusFlags, value: bool) {
        if value {
            self.set(flag);
        } else {
            self.clear(flag);
        }
    }

    fn set_last_op_neg_zero(&mut self, value: u8) {
        self.set_state(StatusFlags::Zero, value == 0);
        self.set_state(StatusFlags::Negative, (value & (1 << 7)) != 0)
    }

    fn clear(&mut self, flag: StatusFlags) {
        self.0 &= !(flag as u8);
    }

    fn is_set(&self, flag: StatusFlags) -> bool {
        (self.0 & flag as u8) != 0
    }
}

impl Default for StatusFlag {
    fn default() -> Self {
        StatusFlag(0b001_0000)
    }
}

pub enum StatusFlags {
    Carry = 0b000_0001,
    Zero = 0b000_0010,
    InterruptDisable = 0b000_0100,
    Decimal = 0b000_1000,
    BFlag = 0b001_0000,
    // Not used
    Overflow = 0b010_0000,
    Negative = 0b100_0000,
}


pub struct CPU {
    a: u8, // Accumulator
    x: u8, // X Register
    y: u8, // Y Register
    pc: u16, // Program Counter
    s: u8, // Stack Pointer
    p: StatusFlag, // Processor Status
    memory: Memory, // Memory
}

impl Default for CPU {
    fn default() -> Self {
        CPU {
            a: 0,
            x: 0,
            y: 0,
            pc: 0x8000, // Start of ROM
            s: 0xFD, // Stack Pointer
            p: StatusFlag::default(),
            memory: Memory::default(),
        }
    }
}

impl CPU {
    pub fn new() -> Self {
        CPU::default()
    }

    fn wait_n_cycle(&self, n: u8) {

    }

    fn get_next_byte(&mut self) -> u8 {
        let byte = self.memory.read(self.pc);
        self.pc += 1;
        self.wait_n_cycle(1);
        byte
    }

    fn get_next_u16(&mut self) -> u16 {
        let word = self.memory.read_u16(self.pc);
        self.pc += 2;
        self.wait_n_cycle(2);
        word
    }

    pub fn reset(&mut self) {
        self.pc = self.memory.read_reset_vector();
        self.s = 0xFD; 
        self.p = StatusFlag::default();
    }

    pub fn load_ines(&mut self, ines: Ines) {
        self.memory.load_prg(&ines.prg_rom);
    }

    pub fn step(&mut self) {
        // https://www.nesdev.org/wiki/Instruction_reference
        // https://www.nesdev.org/2A03%20technical%20reference.txt
        let opcode = self.get_next_byte();
        let block = opcode & 0b11;

        match block {
            0 => self.instruction_step(opcode),
            1 => self.alu_step(opcode),
            2 => self.rmw_step(opcode),
            3 => self.unofficial_step(opcode),
            _ => unreachable!(),   
        }
    }

    fn instruction_step(&mut self, opcode: u8) {
        println!("INS OPCODE: {:#X}", opcode);

        match opcode {
            0x78 => { // SEI
                self.p.set(StatusFlags::InterruptDisable);
            }
            0xD8 => { // CLD
                self.p.clear(StatusFlags::BFlag);
            },
            0xA0 => { // LDY - Load Y #Immediate
                self.y = self.get_next_byte();
                self.p.set_last_op_neg_zero(self.y);
            },
            0xD0 => { // BNE - Branch if Not Equal
                let address = self.get_next_byte();
                if !self.p.is_set(StatusFlags::Zero) {
                    let value = self.memory.read(address as u16) as i8;
                    let page = self.pc / 256;
                    self.pc = ((self.pc as i32) + value as i32) as u16;
                    let new_age = self.pc / 256;
                    if new_age != page {
                        self.wait_n_cycle(2);
                    } else {
                        self.wait_n_cycle(1);
                    }
                }
            },
            _ => {
                println!("Unknown instruction: {:#X}", opcode);
            }
        } 
    }

    fn alu_step(&mut self, opcode: u8) {
        println!("ALU OPCODE: {:#X}", opcode);
        match opcode {
            0xA9 => { // LDA - Load A #Immediate
                self.a = self.get_next_byte();
                self.p.set_last_op_neg_zero(self.a);
            },
            0x8D | 0x85 => { // STA - Store A (Absolute	| Zero Page)
                let address = self.get_next_u16();
                self.memory.write(address, self.a);
                if opcode == 0x8D {
                    self.wait_n_cycle(1);
                }
            },
            0x91 => { // STA - Store A #(Indirect),Y
                let mut address  = self.get_next_byte() as u16;
                address += self.y as u16;
                self.memory.write(address, self.a);
                self.wait_n_cycle(4);
            },
            0x01 => { // ORA - Bitwise OR #(Indirect,X)
                let value = self.get_next_byte();
                let zp_address =  value + self.x;
                let effective_addr = self.memory.read(zp_address as u16);
                let value = self.memory.read(effective_addr as u16);
                self.a |= value;
                self.p.set_last_op_neg_zero(self.a);
                self.wait_n_cycle(4);
            }
            _ => {
                println!("Unknown ALU instruction: {:#X}", opcode);
            }
        }

    }

    fn rmw_step(&mut self, opcode: u8) {
        println!("RMW OPCODE: {:#X}", opcode);
        match opcode {
            0xA2 => { // LDX #Immediate	
                self.x = self.get_next_byte();
                self.p.set_last_op_neg_zero(self.x);
            },
            0x9A => { // TXS - Transfer X to Stack Pointer
                self.s = self.x;
                self.wait_n_cycle(1);
            }
            0xCA => { // DEX - Decrement X
                self.x -= 1;
                self.p.set_last_op_neg_zero(self.x);
                self.wait_n_cycle(1);
            }
            _ => {
                println!("Unknown RMW instruction: {:#X}", opcode);
            }
            
        }
    }

    fn unofficial_step(&mut self, opcode: u8) {
        unimplemented!("UNOFFICIAL OPCODE: {:#X}", opcode);
    }

}