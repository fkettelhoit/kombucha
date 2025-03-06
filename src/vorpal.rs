use std::{
    env::args,
    fs::{File, read_to_string},
    io::Write,
    process::ExitCode,
};

use vorpal::parse;

fn main() -> ExitCode {
    let mut args = args();
    let bin = args.next().unwrap_or_else(|| "vorpal".into());
    let usage = format!("Usage: {bin} fmt <file>");
    enum Command {
        Fmt,
    }
    let cmd = match args.next().as_ref().map(|s| s.as_str()) {
        Some("fmt") => Command::Fmt,
        Some(cmd) => {
            eprintln!("Invalid command: '{cmd}'");
            eprintln!("{usage}");
            return ExitCode::FAILURE;
        }
        None => {
            println!("{usage}");
            return ExitCode::SUCCESS;
        }
    };
    let Some(file_name) = args.next() else {
        eprintln!("{usage}");
        return ExitCode::FAILURE;
    };
    match read_to_string(&file_name) {
        Err(e) => {
            eprintln!("Could not read {file_name}: {e}");
            ExitCode::FAILURE
        }
        Ok(code) => match cmd {
            Command::Fmt => match parse(&code) {
                Ok(expr) => match File::create(&file_name) {
                    Ok(mut f) => match writeln!(f, "{expr}") {
                        Ok(_) => ExitCode::SUCCESS,
                        Err(e) => {
                            eprintln!("Could not write to file {file_name}: {e}");
                            ExitCode::FAILURE
                        }
                    },
                    Err(e) => {
                        eprintln!("Could not write {file_name}: {e}");
                        ExitCode::FAILURE
                    }
                },
                Err(e) => {
                    eprintln!("{e}");
                    ExitCode::FAILURE
                }
            },
        },
    }
}
