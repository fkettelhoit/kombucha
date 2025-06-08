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
    match read_to_string(&input_file) {
        Err(e) => {
            eprintln!("Could not read {input_file}: {e}");
            ExitCode::FAILURE
        }
        Ok(code) => match compile(&code) {
            Ok(vm) => {
                let result = vm.run();
                loop {
                    match result {
                        Ok(State::Resumable(vm)) => match vm.effect() {
                            "write" => todo!(),
                            eff => {
                                eprintln!("{eff}!({})", vm.arg_pretty());
                                return ExitCode::FAILURE;
                            }
                        },
                        Ok(State::Done(_)) => return ExitCode::SUCCESS,
                        Err(e) => {
                            eprintln!("{e}");
                            return ExitCode::FAILURE;
                        }
                    }
                }
            }
            Err(e) => {
                eprintln!("{e}");
                ExitCode::FAILURE
            }
        },
    }
}
