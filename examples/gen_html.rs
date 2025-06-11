use std::{
    env::args,
    fs::{self, read_to_string},
    process::ExitCode,
};

use vorpal::{compile::compile, run::State};

fn main() -> ExitCode {
    let mut args = args();
    let bin = args.next().unwrap_or_else(|| "vorpal".into());
    let usage = format!("Usage: {bin} <input_file> <output_file>");
    let Some(input_file) = args.next() else {
        eprintln!("{usage}");
        return ExitCode::FAILURE;
    };
    let Some(output_file) = args.next() else {
        eprintln!("{usage}");
        return ExitCode::FAILURE;
    };
    match compile(include_str!("gen_html.vo")) {
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
                            Ok(file) => match vm.bytecode.load(&file) {
                                Err(e) => {
                                    eprintln!("{e}");
                                    return ExitCode::FAILURE;
                                }
                                Ok(start) => result = vm.resume_at(start),
                            },
                        },
                        eff => {
                            eprintln!("{eff}!({})", vm.arg_pretty());
                            return ExitCode::FAILURE;
                        }
                    },
                    Ok(State::Done(v)) => {
                        let result: Vec<Vec<String>> = v
                            .deserialize()
                            .expect("Expected a Vec<Vec<String>> as the program result");
                        let joined =
                            result.iter().map(|l| l.join("")).collect::<Vec<_>>().join("\n");
                        match fs::write(&output_file, joined) {
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
