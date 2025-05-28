use std::{
    env::args,
    fs::{File, read_to_string},
    io::Write,
    process::ExitCode,
};

use vorpal::{V, VmState, compile, pretty};

const SVG_HEADER: &str = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
<svg width=\"900\" height=\"900\" viewBox=\"0 0 900 900\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">";

#[derive(Debug)]
enum Expr {
    I,
    K,
    S,
    App(Box<Expr>, Vec<Expr>),
}

fn convert_value(v: &V, strs: &Vec<String>) -> Expr {
    fn convert_str(s: usize, strs: &Vec<String>) -> Expr {
        match strs[s].as_str() {
            "I" => Expr::I,
            "K" => Expr::K,
            "S" => Expr::S,
            _ => panic!("Unexpected string: {s}"),
        }
    }
    match v {
        V::String(s) => convert_str(*s, strs),
        V::Record(s, args) => Expr::App(
            Box::new(convert_str(*s, strs)),
            args.iter().map(|arg| convert_value(arg, strs)).collect(),
        ),
        v => panic!("Unexpected value: {v:?}"),
    }
}

fn draw_expr(x: usize, y: usize, size: usize, expr: &Expr) -> String {
    match expr {
        Expr::I => format!(
            "<circle cx=\"{x}\" cy=\"{y}\" r=\"{size}\" fill=\"none\" stroke=\"orange\" stroke-width=\"4\" />\n"
        ),
        Expr::K => {
            let x = x - size / 2;
            let y = y - size / 2;
            format!(
                "<rect x=\"{x}\" y=\"{y}\" width=\"{size}\" height=\"{size}\" fill=\"yellow\" stroke-width=\"4\" stroke=\"pink\" />\n"
            )
        }
        Expr::S => format!(
            "<circle cx=\"{x}\" cy=\"{y}\" r=\"{size}\" fill=\"red\" stroke=\"red\" stroke-width=\"4\" />\n"
        ),
        Expr::App(f, args) => {
            let mut s = draw_expr(x, y, size, f);
            let step = (size as f64 * 1.7) as usize;
            let x1 = x;
            let y1 = y;
            let x2 = x + args.len() * step;
            let y2 = y;
            s += &format!(
                "<line x1=\"{x1}\" y1=\"{y1}\" x2=\"{x2}\" y2=\"{y2}\" stroke=\"black\" stroke-width=\"4\" />\n"
            );
            let mut x = x;
            for arg in args {
                x += step;
                let x1 = x;
                let y1 = y;
                let x2 = x;
                let y2 = y + size;
                s += &format!(
                    "<line x1=\"{x1}\" y1=\"{y1}\" x2=\"{x2}\" y2=\"{y2}\" stroke=\"black\" stroke-width=\"4\" stroke-linecap=\"square\" />\n"
                );
                s += &draw_expr(x, y + size, size / 2, arg);
            }
            s
        }
    }
}

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
    match read_to_string(&input_file) {
        Err(e) => {
            eprintln!("Could not read {input_file}: {e}");
            ExitCode::FAILURE
        }
        Ok(code) => match compile(&code) {
            Ok(vm) => {
                let mut result = vm.run();
                let mut generation = 0;
                let mut svg = String::new();
                loop {
                    match result {
                        Ok(VmState::Resumable(res, arg)) => match res.get_effect_name().unwrap() {
                            eff if eff == "trace" => {
                                generation += 1;
                                println!("{}", pretty(&arg, res.strings()));
                                let expr = convert_value(&arg, res.strings());
                                let step = 100;
                                let x = generation * step;
                                let y = generation * step;
                                svg += &draw_expr(x, y, 30, &expr);
                                result = res.run(arg);
                            }
                            name => panic!("Unexpected effect: {name}!"),
                        },
                        Ok(VmState::Done(v, strs)) => {
                            let _expr = pretty(&v, &strs);
                            return match File::create(&output_file) {
                                Ok(mut f) => match writeln!(f, "{SVG_HEADER}{svg}</svg>") {
                                    Ok(_) => ExitCode::SUCCESS,
                                    Err(e) => {
                                        eprintln!("Could not write to file {output_file}: {e}");
                                        ExitCode::FAILURE
                                    }
                                },
                                Err(e) => {
                                    eprintln!("Could not write {output_file}: {e}");
                                    ExitCode::FAILURE
                                }
                            };
                        }
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
