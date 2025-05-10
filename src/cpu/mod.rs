// https://www.nesdev.org/wiki/Instruction_reference
// 6502 CPU

const CPU_FREQUENCY: usize = 1_789_773; 

struct StatusFlag(u8);

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

enum StatusFlags {
    Carry = 0b000_0001,
    Zero = 0b000_0010,
    InterruptDisable = 0b000_0100,
    BFrlag = 0b000_1000,
    // Not used
    Overflow = 0b010_0000,
    Negative = 0b100_0000,
}


struct CPU {
    a: u8, // Accumulator
    x: u8, // X Register
    y: u8, // Y Register
    pc: u16, // Program Counter
    s: u8, // Stack Pointer
    p: StatusFlag, // Processor Status
    memory: [u8; 0xFFFF + 1], // Memory
}

