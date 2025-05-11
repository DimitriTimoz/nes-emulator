use core::panic;

use super::*;

impl Cpu {
    pub(crate) fn cmp(&mut self, a: u8, b: u8) {
        let result = a.wrapping_sub(b);
        self.p.test_negative(result);
        self.p.test_zero(result);
        self.p.set_state(StatusFlags::Carry, a >= b);
    }

    pub(crate) fn branch(&mut self, cond: bool) {
        let offset = self.get_next_byte() as i8;   
        if cond {
            let old_pc = self.pc;       
            self.pc = self.pc.wrapping_add(offset as i16 as u16);
            if self.pc + 2 == old_pc {
                panic!("Branch loop");
            }
            self.wait_n_cycle(1);
            if (old_pc & 0xFF00) != (self.pc & 0xFF00) {
                self.wait_n_cycle(1);
            }
        }
    }

    pub(crate) fn bitwise_xor(&mut self, value: u8) {
        self.a ^= value;
        self.p.set_zn(self.a);
    }

    pub(crate) fn zp_ptr(&self, zp: u8) -> u16 {
        let lo = self.memory.read(zp as u16) as u16;
        let hi = self.memory.read(zp.wrapping_add(1) as u16) as u16;
        (hi << 8) | lo
    }

    #[inline]
    pub(crate) fn zp_x(&mut self, base: u8) -> u16 { base.wrapping_add(self.x) as u16 }
    #[inline]
    pub(crate) fn zp_y(&mut self, base: u8) -> u16 { base.wrapping_add(self.y) as u16 }
    #[inline]
    pub(crate) fn zp_ptr_x(&self, zp: u8, x: u8) -> u16 {
        self.zp_ptr(zp.wrapping_add(x))
    }
}
