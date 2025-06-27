use std::{
    env::args,
    fs::{self, read_to_string},
    process::ExitCode,
};

use kombucha::{compile::compile, run::State};

fn main() -> ExitCode {
    let mut args = args();
    let bin = args.next().unwrap_or_else(|| "<bin>".into());
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
                        "print" => {
                            println!("{}", vm.arg.pretty());
                            let nil = vm.arg.bytecode.serialize(&()).unwrap();
                            result = vm.resume(nil)
                        }
                        "chars" => match read_to_string(&input_file) {
                            Err(e) => {
                                eprintln!("Could not read {input_file}: {e}");
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
                                eprintln!("Could not deserialize argument as string: {e}");
                                return ExitCode::FAILURE;
                            }
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
