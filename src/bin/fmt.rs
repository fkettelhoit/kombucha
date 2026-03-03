use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: fmt <file.kb>");
        std::process::exit(1);
    }
    let path = &args[1];
    let code = fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("cannot read {path}: {e}");
        std::process::exit(1);
    });
    match kombucha::v0::format(&code) {
        Ok(formatted) => print!("{formatted}"),
        Err(e) => {
            eprintln!("parse error: {e}");
            std::process::exit(1);
        }
    }
}
