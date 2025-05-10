pub mod cpu;


pub struct INesHeader {
    prg_rom_size: u8,
    chr_rom_size: u8,
    flags_6: u8,
    flags_7: u8,
    prg_ram_size: u8,
    flags_9: u8,
    flags_10: u8,
}
impl INesHeader {
    pub fn new(data: &[u8]) -> Self {
        INesHeader {
            prg_rom_size: data[4],
            chr_rom_size: data[5],
            flags_6: data[6],
            flags_7: data[7],
            prg_ram_size: data[8],
            flags_9: data[9],
            flags_10: data[10],
        }
    }
}

pub struct Ines {
    header: INesHeader,
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
}

pub fn read_ines(data: &[u8]) -> Result<Ines, &'static str> {
    if data.len() < 16 {
        return Err("Data too short");
    }
    if data[0..=3] != [0x4E, 0x45, 0x53, 0x1A] {
        return Err("Invalid INES header");
    }
    let header = INesHeader::new(data);
    let mut position = 16;
    // Read Trainer if present 
    if header.flags_6 & 0x04 != 0 {
        // Trainer present, read 512 bytes
        let trainer_size = 512;
        let _trainer_data = &data[position..position + trainer_size];
        position += trainer_size;
    }

    // Read PRG ROM size 16384 * x bytes
    let prg_rom_size = header.prg_rom_size as usize * 16384;
    let prg_rom_data = &data[position..position + prg_rom_size];
    position += prg_rom_size;

    // Read CHR ROM size 8192 * y bytes
    let chr_rom_size = header.chr_rom_size as usize * 8192;
    let chr_rom_data = &data[position..position + chr_rom_size];
    position += chr_rom_size;
    // PlayChoice INST-ROM TODO 
    // PlayChoice PROM, if present TODO

    let inest = Ines {
        header,
        prg_rom: prg_rom_data.to_vec(),
        chr_rom: chr_rom_data.to_vec(),
    };

    Ok(inest)
}

fn main() {
    // Initialize CPU
    let mut cpu = cpu::CPU::default();

    // Get ROM path from command line arguments
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <path_to_rom>", args[0]);
        std::process::exit(1);
    }
    let rom_path = &args[1];

    // Load ROM into memory
    let rom_data = std::fs::read(rom_path).expect("Failed to read ROM file");
    let ines = read_ines(&rom_data).expect("Failed to read INES header");
    cpu.load_ines(ines);
    cpu.reset();
    loop {
        cpu.step();
    }
    
}
