
pub struct Memory([u8; 0xFFFF + 1]);

impl Default for Memory {
    fn default() -> Self {
        Memory([0; 0xFFFF + 1])
    }
}

impl Memory {
    pub fn read(&self, addr: u16) -> u8 {
        self.0[addr as usize]
    }

    pub fn read_u16(&self, addr: u16) -> u16 {
        let low = self.read(addr) as u16;
        let high = self.read(addr + 1) as u16;
        (high << 8) | low
    }

    pub fn read_u16_buggy(&self, addr: u16) -> u16 {
        let low = self.read(addr) as u16;
        let high_addr = if addr & 0x00FF == 0x00FF {
            addr & 0xFF00
        } else {
            addr + 1
        };
        let high = self.read(high_addr) as u16;
        (high << 8) | low
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        self.0[addr as usize] = value;
    }

    pub fn write_u16(&mut self, addr: u16, value: u16) {
        let low = (value & 0x00FF) as u8;
        let high = ((value >> 8) & 0x00FF) as u8;
        self.write(addr, low);
        self.write(addr + 1, high);
    }
    
    pub fn load_prg(&mut self, prg_rom: &[u8]) {
        let start_addr = 0x8000;
        let end_addr = start_addr + (prg_rom.len()-1) as u32;
        self.0[start_addr as usize..=end_addr as usize].copy_from_slice(prg_rom);
    }

    pub fn read_reset_vector(&self) -> u16 {
        self.read_u16(0xFFFC)
    }
}
