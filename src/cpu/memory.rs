
pub struct Memory([u8; 0xFFFF + 1]);

impl Default for Memory {
    fn default() -> Self {
        Memory([0; 0xFFFF + 1])
    }
}

impl Memory {
    pub fn read(&self, address: u16) -> u8 {
        self.0[address as usize]
    }

    pub fn read_u16(&self, address: u16) -> u16 {
        let low = self.read(address) as u16;
        let high = self.read(address + 1) as u16;
        (high << 8) | low
    }

    pub fn read_u16_buggy(&self, address: u16) -> u16 {
        let low = self.read(address) as u16;
        let high_address = if address & 0x00FF == 0x00FF {
            address & 0xFF00
        } else {
            address + 1
        };
        let high = self.read(high_address) as u16;
        (high << 8) | low
    }

    pub fn write(&mut self, address: u16, value: u8) {
        self.0[address as usize] = value;
    }

    pub fn write_u16(&mut self, address: u16, value: u16) {
        let low = (value & 0x00FF) as u8;
        let high = ((value >> 8) & 0x00FF) as u8;
        self.write(address, low);
        self.write(address + 1, high);
    }
    
    pub fn load_prg(&mut self, prg_rom: &[u8]) {
        let start_address = 0x8000;
        let end_address = start_address + (prg_rom.len()-1) as u32;
        self.0[start_address as usize..=end_address as usize].copy_from_slice(prg_rom);
    }

    pub fn read_reset_vector(&self) -> u16 {
        self.read_u16(0xFFFC)
    }
}
