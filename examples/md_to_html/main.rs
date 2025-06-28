use std::{
    env::args,
    fs::{self, read_to_string},
    path::PathBuf,
    process::ExitCode,
};

use kombucha::{compile::compile, run::State};

fn main() -> ExitCode {
    let mut args = args();
    let bin = args.next().unwrap_or_else(|| "<bin>".into());
    let usage = format!("Usage: {bin} <input_file>");
    let Some(input_file) = args.next() else {
        eprintln!("{usage}");
        return ExitCode::FAILURE;
    };
    let f = PathBuf::from(&input_file);
    let input_file_name = f.file_stem().unwrap_or_default().to_string_lossy();
    let segments = input_file_name.split("-").collect::<Vec<_>>();
    if segments.len() != 3 {
        eprintln!("Expected input file name to be of form: YYYY-MM-DD");
        return ExitCode::FAILURE;
    }
    let date = segments.join("/");
    let output_file = f.with_extension("html");
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
                        "print" => {
                            println!("{}", vm.arg.pretty());
                            let nil = vm.arg.bytecode.serialize(&()).unwrap();
                            result = vm.resume(nil)
                        }
                        "date" => {
                            let date = vm.arg.bytecode.serialize(&date).unwrap();
                            result = vm.resume(date)
                        }
                        "chars" => match read_to_string(&input_file) {
                            Err(e) => {
                                eprintln!("Could not read {input_file_name}: {e}");
                                return ExitCode::FAILURE;
                            }
                            Ok(file) => {
                                let chars: Vec<char> = file.chars().collect();
                                match vm.arg.bytecode.serialize(&chars) {
                                    Err(e) => {
                                        eprintln!("{e}");
                                        return ExitCode::FAILURE;
                                    }
                                    Ok(arg) => result = vm.resume(arg),
                                }
                            }
                        },
                        "join" => match vm.arg.deserialize::<Vec<String>>() {
                            Ok(strs) => {
                                let arg = vm.serialize(&strs.join("")).unwrap();
                                result = vm.resume(arg);
                            }
                            Err(e) => {
                                eprintln!("Could not deserialize argument as Vec<String>: {e}");
                                return ExitCode::FAILURE;
                            }
                        },
                        "escape" => match vm.arg.deserialize::<Vec<String>>() {
                            Ok(strs) => {
                                let escaped = strs
                                    .into_iter()
                                    .map(|s| {
                                        s.replace("&", "&amp;")
                                            .replace("<", "&lt;")
                                            .replace(">", "&gt;")
                                            .replace("\"", "&quot;")
                                            .replace("'", "&apos;")
                                    })
                                    .collect::<Vec<String>>();
                                let arg = vm.serialize(&escaped).unwrap();
                                result = vm.resume(arg);
                            }
                            Err(e) => {
                                eprintln!("Could not deserialize argument as Vec<String>: {e}");
                                return ExitCode::FAILURE;
                            }
                        },
                        eff => {
                            eprintln!("{eff}!({})", vm.arg.to_string());
                            return ExitCode::FAILURE;
                        }
                    },
                    Ok(State::Done(v)) => {
                        let result: Vec<String> = v.deserialize().expect("Expected a Vec<String>");
                        match fs::write(&output_file, result.join("")) {
                            Ok(_) => return ExitCode::SUCCESS,
                            Err(e) => {
                                eprintln!("Could not write to file {}: {e}", output_file.display());
                                return ExitCode::FAILURE;
                            }
                        }
                    }
                }
            }
        }
    }
}
