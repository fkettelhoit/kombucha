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
                        Ok(VmState::Resumable(vm)) => match vm.effect() {
                            "print" => {
                                println!("{}", pretty(vm.arg(), vm.strings()));
                                result = vm.run_uncached(Bytecode::nil());
                            }
                            "sleep" => {
                                sleep(Duration::from_millis(500));
                                result = vm.run_uncached(Bytecode::nil());
                            }
                            "reload" => {
                                if let Ok(code) = read_to_string(&input_file) {
                                    if !code.trim().is_empty() {
                                        match compile(&code) {
                                            Ok(bytecode) => {
                                                if &bytecode != vm.code() {
                                                    println!("--- RELOADING CODE ---");
                                                    println!("{code}");
                                                    println!("--- END OF CODE ---\n");
                                                    result = vm.reload(bytecode);
                                                    continue;
                                                }
                                            }
                                            Err(e) => {
                                                eprintln!("{e}");
                                            }
                                        }
                                    }
                                }
                                let code = vm.code().clone();
                                result = vm.reload(code);
                            }
                            eff => {
                                eprintln!("{eff}!({})", pretty(vm.arg(), vm.strings()));
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
