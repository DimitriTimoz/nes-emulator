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
    BFrlag = 0b000_1000,
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

    pub fn reset(&mut self) {
        self.pc = self.memory.read_reset_vector();
        self.s = 0xFD; 
        self.p = StatusFlag::default();
    }

    pub fn load_ines(&mut self, ines: Ines) {
        self.memory.load_prg(&ines.prg_rom);
    }

    pub fn step(&mut self) {
        let opcode = self.memory.read(self.pc);
        let block = opcode & 0b11;

        self.pc += 1;
        match block {
            0 => self.instruction_step(opcode),
            1 => self.alu_step(opcode),
            2 => self.rmw_step(opcode),
            3 => self.unofficial_step(opcode),
            _ => unreachable!(),   
        }
    }

    fn instruction_step(&mut self, opcode: u8) {
    }

    fn alu_step(&mut self, opcode: u8) {
    }

    fn rmw_step(&mut self, opcode: u8) {
    }

    fn unofficial_step(&mut self, opcode: u8) {
        unimplemented!("UNOFFICIAL OPCODE: {:#X}", opcode);
    }

}