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
    pub (crate) fn a_x(&self, base: u16) -> u16 {
        base.wrapping_add(self.x as u16)
    }
    #[inline]
    pub (crate) fn a_y(&self, base: u16) -> u16 {
        base.wrapping_add(self.y as u16)
    }

    #[inline]
    pub(crate) fn zp_ptr_x(&self, zp: u8, x: u8) -> u16 {
        self.zp_ptr(zp.wrapping_add(x))
    }

    pub(crate) fn rmw_modify<F>(&mut self, addr: u16, op: F)
    where
        F: Fn(u8, &mut StatusFlag) -> u8,
    {
        let mut val = self.memory.read(addr);
        self.wait_n_cycle(1);                 

        self.memory.write(addr, val);
        self.wait_n_cycle(1);

        val = op(val, &mut self.p);

        self.memory.write(addr, val);
        self.wait_n_cycle(1);                 
    }

    pub(crate) fn asl(value: u8, p: &mut StatusFlag) -> u8 {
        let carry = value & 0x80 != 0;
        let res   = value << 1;
        p.set_state(StatusFlags::Carry, carry);
        p.set_zn(res);
        res
    }

    pub(crate) fn rol(value: u8, p: &mut StatusFlag) -> u8 {
        let carry_in  = p.is_set(StatusFlags::Carry) as u8;
        let carry_out = value & 0x80 != 0;
        let res       = (value << 1) | carry_in;
        p.set_state(StatusFlags::Carry, carry_out);
        p.set_zn(res);
        res
    }

    pub(crate) fn lsr(value: u8, p: &mut StatusFlag) -> u8 {
        let carry = value & 0x01 != 0;
        let res   = value >> 1;
        p.set_state(StatusFlags::Carry, carry);
        p.set_zn(res);
        res
    }

    pub(crate) fn ror(value: u8, p: &mut StatusFlag) -> u8 {
        let carry_in  = p.is_set(StatusFlags::Carry) as u8;
        let carry_out = value & 0x01 != 0;
        let res       = (value >> 1) | (carry_in << 7);
        p.set_state(StatusFlags::Carry, carry_out);
        p.set_zn(res);
        res
    }

    pub(crate) fn adc(&mut self, rhs: u8) {
        let a        = self.a;
        let carry_in = self.p.is_set(StatusFlags::Carry) as u8;

        let (t, c1)  = a.overflowing_add(rhs);
        let (r, c2)  = t.overflowing_add(carry_in);

        /* C, Z, N */
        self.p.set_state(StatusFlags::Carry, c1 || c2);
        self.p.set_zn(r);

        /* V  (addition rule) */
        let ovf = ((a ^ r) & (rhs ^ r) & 0x80) != 0;
        self.p.set_state(StatusFlags::Overflow, ovf);

        self.a = r;
    }

    /* ----------  Subtract with Carry (fixed) ---------- */
    pub(crate) fn sbc(&mut self, m: u8) {
        let a        = self.a;
        let carry_in = self.p.is_set(StatusFlags::Carry) as u8;

        // A + (¬M) + C
        let inv      = m ^ 0xFF;
        let (t, c1)  = a.overflowing_add(inv);
        let (r, c2)  = t.overflowing_add(carry_in);

        /* C  (set if NO borrow) */
        self.p.set_state(StatusFlags::Carry, c1 || c2);

        /* Z, N */
        self.p.set_zn(r);

        /* V  (subtraction rule) */
        let ovf = ((a ^ r) & (a ^ m) & 0x80) != 0;
        self.p.set_state(StatusFlags::Overflow, ovf);

        self.a = r;
    }

    #[inline]
    pub(crate) fn page_cross(&mut self, base: u16, addr: u16, same: u8, crossed: u8) {
        if (base & 0xFF00) != (addr & 0xFF00) {
            self.wait_n_cycle(crossed);
        } else {
            self.wait_n_cycle(same);
        }
    }
}
