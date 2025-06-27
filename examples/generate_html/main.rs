use std::{
    env::args,
    fs::{self, read_to_string},
    process::ExitCode,
};

use kombucha::{compile::compile, run::State};

fn main() -> ExitCode {
    let mut args = args();
    let bin = args.next().unwrap_or_else(|| "kombucha".into());
    let usage = format!("Usage: {bin} <input_file> <output_file>");
    let Some(input_file) = args.next() else {
        eprintln!("{usage}");
        return ExitCode::FAILURE;
    };
    let Some(output_file) = args.next() else {
        eprintln!("{usage}");
        return ExitCode::FAILURE;
    };
    match compile(include_str!("main.kb")) {
        Err(e) => {
            eprintln!("{e}");
            ExitCode::FAILURE
        }
        Ok(bytecode) => {
            let mut result = bytecode.run();
            loop {
                match result {
                    Err(e) => {
                        eprintln!("{e}");
                        return ExitCode::FAILURE;
                    }
                    Ok(State::Resumable(mut vm)) => match vm.effect() {
                        "read" => match read_to_string(&input_file) {
                            Err(e) => {
                                eprintln!("Could not read {input_file}: {e}");
                                return ExitCode::FAILURE;
                            }
                            Ok(file) => match vm.arg.bytecode.load(&file) {
                                Err(e) => {
                                    eprintln!("{e}");
                                    return ExitCode::FAILURE;
                                }
                                Ok(start) => result = vm.resume_at(start),
                            },
                        },
                        "escape" => match vm.arg.deserialize::<String>() {
                            Ok(str) => {
                                let escaped = str
                                    .replace("&", "&amp;")
                                    .replace("<", "&lt;")
                                    .replace(">", "&gt;")
                                    .replace("\"", "&quot;")
                                    .replace("'", "&apos;");
                                let arg = vm.serialize(&escaped).unwrap();
                                result = vm.resume(arg);
                            }
                            Err(e) => {
                                eprintln!("Could not deserialize argument as string: {e}");
                                return ExitCode::FAILURE;
                            }
                        },
                        "quine" => match read_to_string(&input_file) {
                            Err(e) => {
                                eprintln!("Could not read {input_file}: {e}");
                                return ExitCode::FAILURE;
                            }
                            Ok(file) => match vm.serialize(&file) {
                                Err(e) => {
                                    eprintln!("Could not serialize code: {e}");
                                    return ExitCode::FAILURE;
                                }
                                Ok(code) => result = vm.resume(code),
                            },
                        },
                        eff => {
                            eprintln!("{eff}!(...)");
                            return ExitCode::FAILURE;
                        }
                    },
                    Ok(State::Done(v)) => {
                        let result: Vec<String> = v.deserialize().expect("Expected a Vec<String>");
                        match fs::write(&output_file, result.join("")) {
                            Ok(_) => return ExitCode::SUCCESS,
                            Err(e) => {
                                eprintln!("Could not write to file {output_file}: {e}");
                                return ExitCode::FAILURE;
                            }
                        }
                    }
                }
            }
        }
    }
}
