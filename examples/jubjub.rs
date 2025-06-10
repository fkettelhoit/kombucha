use std::{env::args, fs::read_to_string, process::ExitCode};

use vorpal::{compile::compile, run::State};

fn main() -> ExitCode {
    let mut args = args();
    let bin = args.next().unwrap_or_else(|| "vorpal".into());
    let usage = format!("Usage: {bin} <input_file>");
    let Some(input_file) = args.next() else {
        eprintln!("{usage}");
        return ExitCode::FAILURE;
    };
    match compile(include_str!("jubjub.vo")) {
        Err(e) => {
            eprintln!("{e}");
            ExitCode::FAILURE
        }
        Ok(bytecode) => {
            let result = bytecode.run();
            loop {
                match result {
                    Err(e) => {
                        eprintln!("{e}");
                        return ExitCode::FAILURE;
                    }
                    Ok(State::Resumable(vm)) => match vm.effect() {
                        "read" => match read_to_string(&input_file) {
                            Err(e) => {
                                eprintln!("Could not read {input_file}: {e}");
                                return ExitCode::FAILURE;
                            }
                            Ok(_file) => {
                                todo!()
                                // match vm.bytecode.load(&file) {
                                // Err(e) => {
                                //     eprintln!("{e}");
                                //     return ExitCode::FAILURE;
                                // }
                                // Ok(v) => {
                                //     // result = vm.resume(v)
                                //     todo!()
                                // }
                            }
                        },
                        "write" => todo!(),
                        eff => {
                            eprintln!("{eff}!({})", vm.arg_pretty());
                            return ExitCode::FAILURE;
                        }
                    },
                    Ok(State::Done(_)) => return ExitCode::SUCCESS,
                }
            }
        }
    }
}
