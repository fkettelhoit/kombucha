use std::{env::args, fs::read_to_string, process::ExitCode, thread::sleep, time::Duration};

use vorpal::{
    bytecode::Bytecode,
    compile::compile,
    run::{VmState, pretty},
};

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
                let mut result = vm.run();
                loop {
                    match result {
                        Ok(VmState::Resumable(arg, vm)) => match vm.get_effect_name().unwrap() {
                            "write" => todo!(),
                            eff => {
                                eprintln!("{eff}!({})", pretty(&arg, vm.strings()));
                                return ExitCode::FAILURE;
                            }
                        },
                        Ok(VmState::Done(_, _)) => return ExitCode::SUCCESS,
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
