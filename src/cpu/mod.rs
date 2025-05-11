// https://www.nesdev.org/wiki/Instruction_reference
// 6502 Cpu
// https://www.nesdev.org/wiki/Emulator_tests Tests

pub mod memory;
use std::time::Duration;

pub use memory::*;
pub mod logic;
pub use logic::*;

macro_rules! trace_log {
    ($self:ident, $fmt:literal $(, $arg:expr)* $(,)?) => {
        #[cfg(debug_assertions)]
        {
            println!(concat!("{:04X}: ", $fmt), $self.pc-1 $(, $arg)*);
        }
    };
}

macro_rules! debug_log {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println!($($arg)*);
        }
    };
}


use crate::Ines;
const Cpu_FREQUENCY: usize = 1_789_773; 
const STACK_START: u16 = 0x0100; 
const STACK_LAST: u16 = 0x01FF; 

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

    fn test_zero(&mut self, value: u8) {
        self.set_state(StatusFlags::Zero, value == 0);
    }

    fn test_negative(&mut self, value: u8) {
        self.set_state(StatusFlags::Negative, (value & (1 << 7)) != 0)
    }

    fn test_overflow(&mut self, value: u8) {
        self.set_state(StatusFlags::Overflow, (value & (1 << 6)) != 0);
    }

    fn test_carry(&mut self, value: u8) {
        self.set_state(StatusFlags::Carry, (value & (1 << 7)) != 0);
    }

    fn set_zn(&mut self, value: u8) {
        self.test_zero(value);
        self.test_negative(value);
    }

    fn set_all(&mut self, value: u8, carry: bool) {
        self.set_zn(value);
        self.set_overflow(value);
        self.set_state(StatusFlags::Carry, carry);
    }

    fn set_overflow(&mut self, value: u8) {
        self.set_state(StatusFlags::Overflow, (value & (1 << 6)) != 0);
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
    Overflow = 0b0100_0000,
    Negative = 0b1000_0000,
}


pub struct Cpu {
    a: u8, // Accumulator
    x: u8, // X Register
    y: u8, // Y Register
    pc: u16, // Program Counter
    s: u8, // Stack Pointer
    p: StatusFlag, // Processor Status
    memory: Memory, // Memory
    last_cycle_time: Duration
}

impl Default for Cpu {
    fn default() -> Self {
        Cpu {
            a: 0,
            x: 0,
            y: 0,
            pc: 0x8000, // Start of ROM
            s: 0xFD, // Stack Pointer
            p: StatusFlag::default(),
            memory: Memory::default(),
            last_cycle_time: Duration::new(0, 0),
        }
    }
}

impl Cpu {
    pub fn new() -> Self {
        Cpu::default()
    }

    fn wait_n_cycle(&mut self, n: u8) {
        let cycle_time = Duration::from_nanos((1_000_000_000 / Cpu_FREQUENCY) as u64);
        let wait_time = cycle_time * n as u32;
        std::thread::sleep(wait_time);
        // TODO: take into account the emulator speed
        self.last_cycle_time = wait_time;
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

    /// Stacks
    pub fn push(&mut self, value: u8) {
        let stack_addr =  self.s as u16 + STACK_START;
        self.memory.write(stack_addr, value);
        self.s = self.s.wrapping_sub(1);
    }

    pub fn push_u16(&mut self, value: u16) {
        self.push((value >> 8) as u8); 
        self.push((value & 0xFF) as u8);
    }

    pub fn pull(&mut self) -> u8 {
        self.s = self.s.wrapping_add(1);
        let addr = self.s as u16 + STACK_START;
        self.memory.read(addr)
    }

    pub fn pull_u16(&mut self) -> u16 {
        let low = self.pull() as u16;
        let high = self.pull() as u16;
        (high << 8) | low
    }

    pub fn reset(&mut self) {
        let lo = self.memory.read(0xFFFC) as u16;
        let hi = self.memory.read(0xFFFD) as u16;
        self.pc = (hi << 8) | lo;

        self.s = 0xFD; 
        self.p = StatusFlag::default();
    }

    pub fn load_ines(&mut self, ines: Ines) {
        self.memory.load_prg(&ines.prg_rom);
    }

     pub fn load_rom(&mut self, room: &[u8]) {
        self.memory.load_rom(room);
        self.memory.write(0xFFFC, 0x00);
        self.memory.write(0xFFFD, 0x04);
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
        match opcode {
            0x58 => { // CLI - Clear Interrupt Disable
                trace_log!(self, "CLI");
                self.p.clear(StatusFlags::InterruptDisable);
                self.wait_n_cycle(1);
            },
            0x78 => { // SEI
                trace_log!(self, "SEI");
                self.p.set(StatusFlags::InterruptDisable);
                self.wait_n_cycle(1);
            },
            0xF8 => { // SED - Set Decimal
                trace_log!(self, "SED");
                self.p.set(StatusFlags::Decimal);
                self.wait_n_cycle(1);
            },
            0xD8 => { // CLD
                trace_log!(self, "CLD");
                self.p.clear(StatusFlags::Decimal);
                self.wait_n_cycle(1);
            },
            0xB8 => { // CLV - Clear Overflow
                trace_log!(self, "CLV");
                self.p.clear(StatusFlags::Overflow);
                self.wait_n_cycle(1);
            },
            0xA0 => { // LDY #Immediate	
                trace_log!(self, "LDY");
                self.y = self.get_next_byte();
                self.p.set_zn(self.y);
            },
            0xA4 => { // LDY - Load X # Zero Page	
                trace_log!(self, "LDY");
                let addr = self.get_next_byte() as u16;
                self.y = self.memory.read(addr);
                self.p.set_zn(self.y);
                self.wait_n_cycle(1);
            },
            0xB4 => { // LDY - Load Y #Zero Page,x	
                trace_log!(self, "LDY zp,X");
                let addr = (self.get_next_byte() as u16).wrapping_add(self.x as u16) & 0xFF;
                self.y = self.memory.read(addr);
                self.p.set_zn(self.y);
                self.wait_n_cycle(1);
            },
            0xAC => { // LDY - Load Y #Absolute
                trace_log!(self, "LDY");
                let addr = self.get_next_u16();
                self.y = self.memory.read(addr);
                self.p.set_zn(self.y);
                self.wait_n_cycle(1);
            },
            0xBC => { // LDY - Load Y #Absolute,X
                trace_log!(self, "LDY");
                let base_addr = self.get_next_u16();
                let addr= base_addr.wrapping_add(self.x as u16);
                self.y = self.memory.read(addr);
                self.p.set_zn(self.y);
                if (base_addr & 0xFF00) != (addr & 0xFF00) {
                    self.wait_n_cycle(2);
                } else {
                    self.wait_n_cycle(1);
                }
            },
            0x8C => { // STY - Store Y #Absolute    
                trace_log!(self, "STY");
                let addr = self.get_next_u16();
                self.memory.write(addr, self.y);
                self.wait_n_cycle(1);
            },
            0x84 => { // STY - Store Y #Zero Page
                trace_log!(self, "STY");
                let addr = self.get_next_byte() as u16;
                self.memory.write(addr, self.y);
                self.wait_n_cycle(1);
            },
            0x94 => { // STY - Store Y #Zero Page,X
                trace_log!(self, "STY zp,x");
                let base = self.get_next_byte();        
                let addr = (base.wrapping_add(self.x)) as u16;
                self.memory.write(addr, self.y);
                self.wait_n_cycle(2);
            },
            0xD0 => { trace_log!(self,"BNE"); self.branch(!self.p.is_set(StatusFlags::Zero)); }
            0xF0 => { trace_log!(self,"BEQ"); self.branch( self.p.is_set(StatusFlags::Zero)); }
            0x10 => { trace_log!(self,"BPL"); self.branch(!self.p.is_set(StatusFlags::Negative)); }
            0x30 => { trace_log!(self,"BMI"); self.branch( self.p.is_set(StatusFlags::Negative)); }
            0x90 => { trace_log!(self,"BCC"); self.branch(!self.p.is_set(StatusFlags::Carry)); }
            0xB0 => { trace_log!(self,"BCS"); self.branch( self.p.is_set(StatusFlags::Carry)); }
            0x50 => { trace_log!(self,"BVC"); self.branch(!self.p.is_set(StatusFlags::Overflow)); }
            0x70 => { trace_log!(self,"BVS"); self.branch( self.p.is_set(StatusFlags::Overflow)); }
            0x24 => { // BIT - Bit Test (Zero Page)
                trace_log!(self, "BIT zp");
                let addr = self.get_next_byte() as u16;
                let value = self.memory.read(addr);
                let result = value & self.a;
                self.p.test_zero(result);
                self.p.test_negative(value);
                self.p.test_overflow(value);
                self.wait_n_cycle(1);
            },
            0x2C => { // BIT - Bit Test (Absolute)
                trace_log!(self, "BIT");
                let addr = self.get_next_u16();
                let value = self.memory.read(addr);
                let result = value & self.a;
                self.p.test_zero(result);
                self.p.test_negative(value);
                self.p.test_overflow(value);
                self.wait_n_cycle(1);
            },
            0x20 => { // JSR - Jump to Subroutine
                trace_log!(self, "JSR");
                let addr = self.get_next_u16(); 
                self.push_u16(self.pc - 1);
                self.pc = addr;
                self.wait_n_cycle(3);
            },
            0x4C => { // JMP - Jump
                trace_log!(self, "JMP");
                let addr = self.get_next_u16(); 
                if addr == (self.pc-3) {
                    panic!("JUMP TO SAME ADDRESS: {:#X}", addr);
                } 
                self.pc = addr;
            },
            0x6C => { // JMP - Jump (indirect buggy)
                trace_log!(self, "JMP");
                let addr = self.get_next_u16(); 
                let pc = self.memory.read_u16_buggy(addr);
                self.pc = pc;
                self.wait_n_cycle(2);
            },
            0x60 => { // RTS - Return from Subroutine
                trace_log!(self, "RTS");
                let addr = self.pull_u16();
                self.pc = addr.wrapping_add(1);
                self.wait_n_cycle(5);
            },
            0x18 => { // CLC - Clear Carry
                trace_log!(self, "CLC");
                self.p.clear(StatusFlags::Carry);
                self.wait_n_cycle(1);
            },
            0x38 => { // SEC - Set Carry
                trace_log!(self, "SEC");
                self.p.set(StatusFlags::Carry);
                self.wait_n_cycle(1);
            },
            0x88 => { // DEY - Decrement Y
                trace_log!(self, "DEY");
                self.y = self.y.wrapping_sub(1);
                self.p.set_zn(self.y);
                self.wait_n_cycle(1);
            },
            0x48 => { // PHA - Push A
                trace_log!(self, "PHA");
                self.push(self.a);
                self.wait_n_cycle(2);
            },
            0x68 => { // PLA - Pull A
                trace_log!(self, "PLA");
                self.a = self.pull();
                self.p.set_zn(self.a);
                self.wait_n_cycle(3);
            },
            0xE0 => { // CPX - Compare X #Immediate	
                trace_log!(self, "CPX");
                let value = self.get_next_byte();
                self.cmp(self.x,value);
            },
            0xE4 => { // CPX - Compare X Zero Page	
                trace_log!(self, "CPX");
                let addr = self.get_next_byte() as u16;
                let value = self.memory.read(addr);
                self.cmp(self.x,value);
                self.wait_n_cycle(1);
            },
            0xEC => { // CPX - Compare X Absolute	
                trace_log!(self, "CPX");
                let addr = self.get_next_u16();
                let value = self.memory.read(addr);
                self.cmp(self.x,value);
                self.wait_n_cycle(1);
            },
            0xC0 => { // CPY - Compare Y #Immediate	
                trace_log!(self, "CPY");
                let value = self.get_next_byte();
                self.cmp(self.y,value);
            },
            0xC4 => { // CPY - Compare Y Zero Page	
                trace_log!(self, "CPY");
                let addr = self.get_next_byte() as u16;
                let value = self.memory.read(addr);
                self.cmp(self.y,value);
                self.wait_n_cycle(1);
            },
            0xCC => { // CPY - Compare Y Absolute	
                trace_log!(self, "CPY");
                let addr = self.get_next_u16();
                let value = self.memory.read(addr);
                self.cmp(self.y,value);
                self.wait_n_cycle(1);
            },
            0x98 => { // TYA - Transfer Y to A
                trace_log!(self, "TYA");
                self.a = self.y;
                self.p.set_zn(self.a);
            },
            0xA8 => { // TAY - Transfer A to Y
                trace_log!(self, "TAY");
                self.y = self.a;
                self.p.set_zn(self.y);
            },
            0x28 => { // PLP - Pull Processor Status
                trace_log!(self, "PLP");
                self.p.0 = self.pull() | 0x20;
                self.wait_n_cycle(2);
            },
            0x08 => { // PHP - Push Processor Status
                trace_log!(self, "PHP");
                self.push(self.p.0 | 0b00110000);
                self.wait_n_cycle(2);
            },
            0xE8 => { // INX - Increment X
                trace_log!(self, "INX");
                self.x = self.x.wrapping_add(1);
                self.p.set_zn(self.x);
                self.wait_n_cycle(1);
            },
            0xC8 => { // INY - Increment Y
                trace_log!(self, "INY");
                self.y = self.y.wrapping_add(1);
                self.p.set_zn(self.y);
                self.wait_n_cycle(1);
            },
            0x00 => { // BRK - Break (software IRQ)
                trace_log!(self, "BRK");
                self.push_u16(self.pc+1);
                self.push(self.p.0 | 0b0011_0000);
                self.pc = self.memory.read_u16(0xFFFE);

                self.p.set(StatusFlags::InterruptDisable);
                self.p.set(StatusFlags::BFlag);
                self.wait_n_cycle(6);
            },
            0x40 => { // RTI - Return from Interrupt
                trace_log!(self, "RTI");
                let flags = self.pull();
                self.p.0 &= !0b11001111;
                self.p.0 |= flags & 0b11001111;
                let pc = self.pull_u16();
                self.pc = pc;
                self.wait_n_cycle(5);
            }
            _ => {
                panic!("{:#X} Instruction step: Unknown instruction: {:#X}", self.pc, opcode);
            }
        } 
    }

    fn alu_step(&mut self, opcode: u8) {
        match opcode {
            0xA9 => { // LDA - Load A #Immediate
                trace_log!(self, "LDA");
                let value = self.get_next_byte();
                self.a = value;
                self.p.set_zn(self.a);
            },
            0xA5 => { // LDA - Load A #Zero Page	
                trace_log!(self, "LDA");
                let addr = self.get_next_byte() as u16;
                self.a = self.memory.read(addr);
                self.p.set_zn(self.a);
                self.wait_n_cycle(1);
            },
            0xB5 => { // LDA - Load A #Zero Page,X	
                trace_log!(self, "LDA");
                let base =  self.get_next_byte();
                let addr = self.zp_x(base);
                self.a = self.memory.read(addr);
                self.p.set_zn(self.a);
                self.wait_n_cycle(1);
            },
            0xAD => { // LDA - Load A #Absolute
                trace_log!(self, "LDA");
                let addr = self.get_next_u16();
                self.a = self.memory.read(addr);
                self.p.set_zn(self.a);
                self.wait_n_cycle(1);
            },
            0xBD => { // LDA - Load A (Absolute,X)
                trace_log!(self, "LDA");
                let base_addr = self.get_next_u16();
                let addr = base_addr.wrapping_add(self.x as u16);
                if (addr & 0xFF00) != (base_addr & 0xFF00) {
                    self.wait_n_cycle(1);
                }
                self.a = self.memory.read(addr);
                self.p.set_zn(self.a);
                self.wait_n_cycle(1);
            },
            0xB9 => { // LDA - Load A (Absolute,Y)
                trace_log!(self, "LDA");
                let base_addr = self.get_next_u16();
                let addr = base_addr.wrapping_add(self.y as u16);
                if (addr & 0xFF00) != (base_addr & 0xFF00) {
                    self.wait_n_cycle(1);
                }
                self.a = self.memory.read(addr);
                self.p.set_zn(self.a);
                self.wait_n_cycle(1);
            },
            0xA1 => { // LDA - Load A (Indirect,X)	
                trace_log!(self, "LDA (ind,X)");
                let zp   = self.get_next_byte();
                let addr = self.zp_ptr_x(zp, self.x);
                self.a   = self.memory.read(addr);
                self.p.set_zn(self.a);
                self.wait_n_cycle(4);              
            },
            0xB1 => {
                trace_log!(self, "LDA (ind),Y");
                let zp = self.get_next_byte();
                let base = self.zp_ptr(zp);
                let addr = base.wrapping_add(self.y as u16);
                self.a = self.memory.read(addr);
                self.p.set_zn(self.a);
                if (base & 0xFF00) != (addr & 0xFF00) { self.wait_n_cycle(2) } else { self.wait_n_cycle(1) }
            },
            0x8D => { // STA - Store A (Absolute)
                trace_log!(self, "STA");
                let addr = self.get_next_u16();
                self.memory.write(addr, self.a);
                self.wait_n_cycle(1);
            },
            0x85 => { // STA - Store A  (Zero Page)
                trace_log!(self, "STA zp");
                let addr = self.get_next_byte() as u16;
                self.memory.write(addr, self.a);
                self.wait_n_cycle(1);
            },
            0x95 => { // STA - Store A  (Zero Page,X)
                trace_log!(self, "STA zp,x");
                let base = self.get_next_byte();
                let addr = self.zp_x(base);
                self.memory.write(addr, self.a);
                self.wait_n_cycle(1);
            },
            0x91 => {
                trace_log!(self, "STA (ind),Y");
                let zp = self.get_next_byte();
                let base = self.zp_ptr(zp);
                let addr = base.wrapping_add(self.y as u16);
                self.memory.write(addr, self.a);
                self.wait_n_cycle(4);
            },
            0x81 => {
                trace_log!(self, "STA (ind,X)");
                let zp   = self.get_next_byte();
                let addr = self.zp_ptr_x(zp, self.x);
                self.memory.write(addr, self.a);
                self.wait_n_cycle(5);
            },
            0x9D => { // STA - Store A #Absolute,X	
                trace_log!(self, "STA");
                let addr = self.get_next_u16().wrapping_add(self.x as u16);
                self.memory.write(addr, self.a);
                self.wait_n_cycle(2);
            },
            0x99 => { // STA - Store A #Absolute,Y	
                trace_log!(self, "STA");
                let addr = self.get_next_u16().wrapping_add(self.y as u16);
                self.memory.write(addr, self.a);
                self.wait_n_cycle(2);
            },
            0x01 => { // ORA - Bitwise OR #(Indirect,X)
                trace_log!(self, "ORA");
                let zp   = self.get_next_byte();
                let addr = self.zp_ptr_x(zp, self.x);
                let val  = self.memory.read(addr);
                self.a |= val;
                self.p.set_zn(self.a);
                self.wait_n_cycle(4);
            },
            0x09 => { // ORA - Bitwise OR #Immediate
                trace_log!(self, "ORA");
                let value = self.get_next_byte();
                self.a |= value;
                self.p.set_zn(self.a);
                self.wait_n_cycle(4);
            },
            // Bitwise Exclusive OR
            0x49 => {
                trace_log!(self, "EOR");
                let value = self.get_next_byte();
                self.bitwise_xor(value);
            },
            0x45 => { // Zero Page
                trace_log!(self, "EOR");
                let addr = self.get_next_byte() as u16;
                let value = self.memory.read(addr);
                self.bitwise_xor(value);
                self.wait_n_cycle(1);
            },
            0x55 => { // Zero Page,X
                trace_log!(self, "EOR");
                let base = self.get_next_byte();
                let addr = self.zp_x(base);
                let value = self.memory.read(addr);
                self.bitwise_xor(value);
                self.wait_n_cycle(2);
            },
            0x4D => { // Absolute
                trace_log!(self, "EOR");
                let addr = self.get_next_u16();
                let value = self.memory.read(addr);
                self.bitwise_xor(value);
                self.wait_n_cycle(1);
            },
            0x5D => { // Absolute,x
                trace_log!(self, "EOR");
                let addr = self.get_next_u16().wrapping_add(self.x as u16);
                let value = self.memory.read(addr);
                self.bitwise_xor(value);
                self.wait_n_cycle(1);
            },
            0x59 => { // Absolute,y
                trace_log!(self, "EOR");
                let addr = self.get_next_u16().wrapping_add(self.y as u16);
                let value = self.memory.read(addr);
                self.bitwise_xor(value);
                self.wait_n_cycle(1);
            },
            0x41 => { // (Indirect,X)	
                trace_log!(self,"EOR (ind,X)");
                let ptr   = self.get_next_byte();
                let zp_ptr = ptr.wrapping_add(self.x);

                let lo = self.memory.read(zp_ptr as u16);
                let hi = self.memory.read(zp_ptr.wrapping_add(1) as u16);
                let addr = ((hi as u16) << 8) | lo as u16;

                let value = self.memory.read(addr);
                self.bitwise_xor(value);

                self.wait_n_cycle(4);
            }
            0xE5 => { // SBC - Subtract with Carry
                trace_log!(self, "SBC");
                let addr = self.get_next_byte() as u16;
                let value  = self.memory.read(addr);
                let (value, carry) = (!value).overflowing_add(self.p.is_set(StatusFlags::Carry) as u8); 
                let (a, carry2)  = self.a.overflowing_add(value);
                self.a = a;
                self.p.set_all(a, carry || carry2);
                self.wait_n_cycle(1);
            },
            0x69 => { // ADC - Add with Carry
                trace_log!(self, "ADC");
                let value = self.get_next_byte();
                let (value, carry) = value.overflowing_add(self.p.is_set(StatusFlags::Carry) as u8); 
                let (a, carry2)  = self.a.overflowing_add(value);
                self.a = a;
                self.p.set_all(a, carry || carry2);
            },
            0xC9 => { // CMP - Compare A
                trace_log!(self, "CMP");
                let value = self.get_next_byte();
                self.cmp(self.a, value);
                self.wait_n_cycle(1);
            },
            0xC5 => { // CMP - Compare Zero Page
                trace_log!(self, "CMP (Zero Page)");
                let addr = self.get_next_byte() as u16;
                let value = self.memory.read(addr);
                self.cmp(self.a, value);
                self.wait_n_cycle(1);
            },
            0xD5 => { // CMP - Compare (Zero Page,x)
                trace_log!(self, "CMP zp,X");
                let base = self.get_next_byte();
                let addr = self.zp_x(base);
                let value = self.memory.read(addr);
                self.cmp(self.a, value);
                self.wait_n_cycle(1);
            },
            0xCD => { // CMP - Compare A Absolute
                trace_log!(self, "CMP (Absolute)");
                let addr = self.get_next_u16();
                let value = self.memory.read(addr);
                self.cmp(self.a, value);
                self.wait_n_cycle(1);
            },
            0xDD => { // CMP - Compare A Absolute,x
                trace_log!(self, "CMP (Absolute, x)");
                
                let base_addr = self.get_next_u16();
                let addr = base_addr.wrapping_add(self.x as u16);
                let crossed = (base_addr & 0xFF00) != (addr & 0xFF00);
                
                let value = self.memory.read(addr);
                self.cmp(self.a, value);
                if crossed {
                    self.wait_n_cycle(2);
                } else {
                    self.wait_n_cycle(1);
                }
            },
            0xD9 => { // CMP - Compare A Absolute,y
                trace_log!(self, "CMP Absolute, y");
                
                let base_addr = self.get_next_u16();
                let addr = base_addr.wrapping_add(self.y as u16);
                let crossed = (base_addr & 0xFF00) != (addr & 0xFF00);
                
                let value = self.memory.read(addr);
                self.cmp(self.a, value);
                if crossed {
                    self.wait_n_cycle(2);
                } else {
                    self.wait_n_cycle(1);
                }
            },
            0xC1 => { // CMP - (Indirect,X)	
                trace_log!(self, "CMP (Indirect, X)");
                let zp   = self.get_next_byte();
                let addr = self.zp_ptr_x(zp, self.x);
                let val  = self.memory.read(addr);
                self.cmp(self.a, val);
                self.wait_n_cycle(4);
            },
            0xD1 => { // CMP - (Indirect),Y	
                trace_log!(self, "CMP (Indirect),Y");
                let zp = self.get_next_byte();
                let base = self.zp_ptr(zp);
                let addr = base.wrapping_add(self.y as u16);
                let value = self.memory.read(addr);
                self.cmp(self.a, value);
                if (base & 0xFF00) != (addr & 0xFF00) {
                    self.wait_n_cycle(2);
                } else {
                    self.wait_n_cycle(1);
                }
            },
            _ => {
                panic!("{:X} Unknown ALU instruction: {:#X}", self.pc, opcode);
            }
        }

    }

    fn rmw_step(&mut self, opcode: u8) {
        match opcode {
            0xA2 => { // LDX #Immediate	
                trace_log!(self, "LDX");
                self.x = self.get_next_byte();
                self.p.set_zn(self.x);
            },
            0xA6 => { // LDX - Load X # Zero Page	
                trace_log!(self, "LDX");
                let addr = self.get_next_byte() as u16;
                self.x = self.memory.read(addr);
                self.p.set_zn(self.x);
                self.wait_n_cycle(1);
            },
            0xB6 => { // LDX - Load X #Zero Page,Y	
                trace_log!(self, "LDX zp,Y");
                let addr = (self.get_next_byte() as u16).wrapping_add(self.y as u16) & 0xFF;
                self.x = self.memory.read(addr);
                self.p.set_zn(self.x);
                self.wait_n_cycle(1);
            },
            0xAE => { // LDX - Load X #Absolute
                trace_log!(self, "LDX");
                let addr = self.get_next_u16();
                self.x = self.memory.read(addr);
                self.p.set_zn(self.x);
                self.wait_n_cycle(1);
            },
            0xBE => { // LDX - Load X #Absolute,Y
                trace_log!(self, "LDX");
                let base_addr = self.get_next_u16();
                let addr= base_addr.wrapping_add(self.y as u16);
                self.x = self.memory.read(addr);
                self.p.set_zn(self.x);
                if (base_addr & 0xFF00) != (addr & 0xFF00) {
                    self.wait_n_cycle(2);
                } else {
                    self.wait_n_cycle(1);
                }
            },
            0xCA => { // DEX - Decrement X
                trace_log!(self, "DEX");
                self.x = self.x.wrapping_sub(1);
                self.p.set_zn(self.x);
                self.wait_n_cycle(1);
            },
            0xE6 => { // INC - Increment Memory
                trace_log!(self, "INC");
                let addr = self.get_next_byte() as u16;
                let mut value = self.memory.read(addr);
                value = value.wrapping_add(1);
                self.memory.write(addr, value);
                self.p.set_zn(value);
                self.wait_n_cycle(3);
            },
            0x8E => { // STX - Store X #Absolute    
                trace_log!(self, "STX");
                let addr = self.get_next_u16();
                self.memory.write(addr, self.x);
                self.wait_n_cycle(1);
            },
            0x86 => { // STX - Store X #Zero Page
                trace_log!(self, "STX");
                let addr = self.get_next_byte() as u16;
                self.memory.write(addr, self.x);
                self.wait_n_cycle(1);
            },
            0x96 => { // STX - Store X #Zero Page,Y
                trace_log!(self, "STX zp,y");
                let base = self.get_next_byte();        
                let addr = (base.wrapping_add(self.y)) as u16;
                self.memory.write(addr, self.x);
                self.wait_n_cycle(2);
            },
            0xEA => { // NOP - No Operation
                trace_log!(self, "NOP");
                self.wait_n_cycle(1);
            },
            0xAA => { // TAX - Transfer A to X
                trace_log!(self, "TAX");
                self.x = self.a;
                self.p.set_zn(self.x);
                self.wait_n_cycle(1);
            },
            0xA8 => { // TAY - Transfer A to Y
                trace_log!(self, "TAY");
                self.y = self.a;
                self.p.set_zn(self.y);
                self.wait_n_cycle(1);
            },
            0x8A => { // TXA - Transfer X to A
                trace_log!(self, "TXA");
                self.a = self.x;
                self.p.set_zn(self.a);
                self.wait_n_cycle(1);
            },
            0xBA => { // TSX - Transfer S to X
                trace_log!(self, "TSX");
                self.x = self.s;
                self.p.set_zn(self.x);
                self.wait_n_cycle(1);
            },
            0x9A => { // TXS - Transfer X to S
                trace_log!(self, "TXS");
                self.s= self.x;
                self.wait_n_cycle(1);
            }
            0x0A => { // ASL - Arithmetic Shift Left
                trace_log!(self, "ASL a");
                let old = self.a;
                self.p.set_state(StatusFlags::Carry, old & 0x80 != 0);
                self.a = old << 1;
                self.p.set_zn(self.a);
                self.wait_n_cycle(1);
            },
            0x06 => { // ASL - Arithmetic Shift Left (Zero Page)
                trace_log!(self, "ASL zp");
                let addr = self.get_next_byte() as u16;
                let value = self.memory.read(addr);
                let (result, carry) = value.overflowing_shl(1);
                self.p.test_negative(result);
                self.p.test_zero(result);
                self.p.set_state(StatusFlags::Carry, carry);
                self.memory.write(addr, result);
                self.wait_n_cycle(3);
            },
            0x16 => { // ASL - Arithmetic Shift Left (Zero Page,X)
                trace_log!(self, "ASL zp,x");
                let base = self.get_next_byte();
                let addr = self.zp_x(base);
                let value = self.memory.read(addr);
                let (result, carry) = value.overflowing_shl(1);
                self.p.test_negative(result);
                self.p.test_zero(result);
                self.p.set_state(StatusFlags::Carry, carry);
                self.memory.write(addr, result);
                self.wait_n_cycle(4);
            },
            0x0E => { // ASL - Arithmetic Shift Left (Absolute)
                trace_log!(self, "ASL");
                let addr = self.get_next_u16();
                let value = self.memory.read(addr);
                let (result, carry) = value.overflowing_shl(1);
                self.p.test_negative(result);
                self.p.test_zero(result);
                self.p.set_state(StatusFlags::Carry, carry);
                self.memory.write(addr, result);
                self.wait_n_cycle(3);
            },
            0x1E => { // ASL - Arithmetic Shift Left (Absolute,X)
                trace_log!(self, "ASL");
                let base_addr = self.get_next_u16();
                let addr = base_addr.wrapping_add(self.x as u16);
                let value = self.memory.read(addr);
                let (result, carry) = value.overflowing_shl(1);
                self.p.test_negative(result);
                self.p.test_zero(result);
                self.p.set_state(StatusFlags::Carry, carry);
                self.memory.write(addr, result);
                if (base_addr & 0xFF00) != (addr & 0xFF00) {
                    self.wait_n_cycle(4);
                } else {
                    self.wait_n_cycle(3);
                }
            },
            _ => {
                panic!("Unknown RMW instruction: {:#X}", opcode);
            }
            
        }
    }

    fn unofficial_step(&mut self, opcode: u8) {
        trace_log!(self, "{:X}", opcode);
        unimplemented!("UNOFFICIAL OPCODE: {:#X}", opcode);
    }

}
