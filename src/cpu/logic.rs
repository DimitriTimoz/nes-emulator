use super::*;

impl CPU {
    pub(crate) fn cmp(&mut self, a: u8, b: u8) {
        let result = a.wrapping_sub(b);
        self.p.test_negative(result);
        self.p.test_zero(result);
        self.p.set_state(StatusFlags::Carry, a >= b);
    }

    pub(crate) fn branch(&mut self, cond: bool) {
        let offset = self.get_next_byte() as i8;   // <- signé
        if cond {
            let old_pc = self.pc;                  // PC pointe déjà après l’opérande
            self.pc = self.pc.wrapping_add(offset as i16 as u16);

            self.wait_n_cycle(1);                  // +1 cycle si branche prise
            if (old_pc & 0xFF00) != (self.pc & 0xFF00) {
                self.wait_n_cycle(1);              // +1 cycle si changement de page
            }
        }
    }

    pub(crate) fn bitwise_xor(&mut self, value: u8) {
        self.a ^= value;
        self.p.set_zn(self.a);
    }

}