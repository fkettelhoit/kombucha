use std::{env, fs, io::Write, iter::once, path::PathBuf};

use vorpal::{
    bytecode::{Bytecode, Ctx, NIL, Op},
    compile::{A, Ast, Expr, codegen, desugar, parse},
    run::State,
};

fn pretty_ast<'c>(prg: &[Ast]) -> String {
    fn pretty<'c>(ast: &Ast, lvl: usize, buf: &mut String) {
        let indent = "  ";
        match &ast.1 {
            A::Var(s) => buf.push_str(s),
            A::Atom(s) => buf.push_str(s),
            A::String(s) => buf.push_str(&format!("\"{s}\"")),
            A::Binding(lvl, s) => buf.push_str(&(":".repeat(lvl + 1) + s)),
            A::Block(items) => {
                buf.push_str("{ ");
                for (i, item) in items.iter().enumerate() {
                    if i != 0 {
                        buf.push('\n');
                        buf.push_str(&indent.repeat(lvl + 1));
                    }
                    pretty(&item, lvl + 1, buf);
                }
                buf.push_str(" }")
            }
            A::List(items) => {
                buf.push_str("[ ");
                for (i, item) in items.iter().enumerate() {
                    if i != 0 {
                        buf.push('\n');
                        buf.push_str(&indent.repeat(lvl + 1));
                    }
                    pretty(&item, lvl + 1, buf);
                }
                buf.push_str(" ]")
            }
            A::Call(f, args) => {
                buf.push('(');
                pretty(&f, lvl, buf);
                for arg in args {
                    buf.push('\n');
                    buf.push_str(&indent.repeat(lvl + 1));
                    pretty(&arg, lvl + 1, buf);
                }
                buf.push(')')
            }
        }
    }
    let mut buf = String::new();
    for item in prg.iter() {
        pretty(item, 0, &mut buf);
        buf.push('\n');
    }
    buf
}

fn pretty_expr(expr: &Expr, strs: &Vec<String>) -> String {
    fn pretty(expr: &Expr, strs: &Vec<String>, lvl: usize, buf: &mut String) {
        let indent = "  ";
        match expr {
            Expr::Var(v) => buf.push_str(&v.to_string()),
            Expr::String(s) if strs[*s] == NIL => buf.push_str("[]"),
            Expr::String(s) => buf.push_str(&strs[*s]),
            Expr::Effect(eff) => buf.push_str(&format!("{}!", strs[*eff])),
            Expr::Abs(expr) => {
                buf.push_str("=>\n");
                buf.push_str(&indent.repeat(lvl + 1));
                pretty(expr, strs, lvl + 1, buf);
            }
            Expr::Rec(expr) => {
                buf.push_str("~>\n");
                buf.push_str(&indent.repeat(lvl + 1));
                pretty(expr, strs, lvl + 1, buf);
            }
            Expr::App(f, arg) => {
                buf.push_str("( ");
                pretty(f, strs, lvl + 1, buf);
                buf.push('\n');
                buf.push_str(&indent.repeat(lvl + 1));
                pretty(arg, strs, lvl + 1, buf);
                buf.push_str(" )");
            }
            Expr::Unpack([v, t, f]) => {
                buf.push_str("( pop");
                for expr in [v, t, f] {
                    buf.push('\n');
                    buf.push_str(&indent.repeat(lvl + 1));
                    pretty(expr, strs, lvl + 1, buf);
                }
                buf.push_str(" )");
            }
            Expr::Handle([v, e, h]) => {
                buf.push_str("( try");
                for expr in [v, e, h] {
                    buf.push('\n');
                    buf.push_str(&indent.repeat(lvl + 1));
                    pretty(expr, strs, lvl + 1, buf);
                }
                buf.push_str(" )");
            }
            Expr::Compare([a, b, t, f]) => {
                buf.push_str("( if");
                for expr in [a, b, t, f] {
                    buf.push('\n');
                    buf.push_str(&indent.repeat(lvl + 1));
                    pretty(expr, strs, lvl + 1, buf);
                }
                buf.push_str(" )");
            }
        }
    }
    let mut buf = String::new();
    pretty(expr, strs, 0, &mut buf);
    buf
}

fn pretty_bytecode(bytecode: &Bytecode) -> String {
    let mut buf = String::new();
    for (i, op) in bytecode.ops.iter().enumerate() {
        match op {
            Op::Return => buf.push_str(&format!("{i:05}:   Return\n")),
            Op::LoadString(s) => match bytecode.ctx.strs.get(*s) {
                Some(s) => buf.push_str(&format!("{i:05}: PushString(\"{s}\")\n")),
                None => buf.push_str(&format!("{i:05}: {op:05?}\n")),
            },
            Op::LoadEffect(s) => match bytecode.ctx.strs.get(*s) {
                Some(s) => buf.push_str(&format!("{i:05}: PushEffect(\"{s}\")\n")),
                None => buf.push_str(&format!("{i:05}: {op:05?}\n")),
            },
            _ => buf.push_str(&format!("{i:05}: {op:05?}\n")),
        }
    }
    buf
}

#[derive(Debug, Clone)]
struct Failure {
    code: String,
    expected: String,
    actual: String,
}

const STAGES: [&str; 4] = ["parse", "desugar", "compile", "run"];
const TEST_SEP: &str = "\n\n---\n\n";
const STAGE_SEP: &str = "\n\n";

fn parse_tests(path: PathBuf) -> Result<Vec<(String, Vec<String>)>, String> {
    let tests = fs::read_to_string(&path)
        .map_err(|e| format!("Failed to read file {}: {e}", path.display()))?;
    Ok(tests
        .split(TEST_SEP)
        .map(|t| {
            let mut stages = t.split(STAGE_SEP).map(|p| p.to_string());
            let code = stages.next().unwrap_or_default();
            (code, stages.collect())
        })
        .collect())
}

fn test_without_run(code: &str) -> (Vec<String>, Vec<Bytecode>) {
    let mut results = vec![];
    let mut compiled = vec![];
    match parse(code) {
        Err(e) => results.push(e),
        Ok(parsed) => {
            results.push(pretty_ast(&parsed));
            let mut ctx = Ctx::default();
            match desugar(parsed.clone(), code, &mut ctx) {
                Err(e) => results.push(e),
                Ok(expr) => {
                    results.push(pretty_expr(&expr, &ctx.strs));
                    let bytecode = codegen(expr, ctx);
                    let bytes = bytecode.as_bytes().unwrap();
                    let bytecode = Bytecode::parse(&bytes).unwrap();
                    results.push(pretty_bytecode(&bytecode));
                    compiled.push(bytecode);
                }
            }
        }
    }
    let results = results.iter().map(|x| x.trim().to_string()).collect();
    (results, compiled)
}

fn report(mut p: PathBuf, res: Vec<(String, Vec<String>, Vec<String>)>) -> Result<(), String> {
    let mut failed = vec![vec![]; STAGES.len()];
    for (code, expected, actual) in res.iter() {
        for (stage, (expected, actual)) in expected.iter().zip(actual.iter()).enumerate() {
            if expected != actual {
                failed[stage].push(Failure {
                    code: code.clone(),
                    expected: expected.clone(),
                    actual: actual.clone(),
                })
            }
        }
    }
    if failed.iter().all(|stage| stage.is_empty()) {
        return Ok(());
    }
    let overwrite_tests = env::var("OVERWRITE").unwrap_or_default() == "INTERMEDIATE";
    if overwrite_tests {
        for (c, expected, actual) in res.iter() {
            let expected_stages = expected.len();
            let actual_stages = actual.len();
            if expected_stages > actual_stages {
                eprintln!("Code:\n{c}\n");
                eprintln!("Expected {expected_stages} stages, but found {actual_stages}:");
                eprintln!("{}", actual.last().cloned().unwrap_or_default());
                return Err(format!("Wrong number of stages in {}", p.display()));
            }
            match (expected.last(), actual.get(expected.len() - 1)) {
                (Some(exp), Some(act)) if exp.trim() != "?" && exp.trim() != act.trim() => {
                    eprintln!("Code:\n{c}\n");
                    eprintln!("Expected:\n{exp}\n");
                    eprintln!("Actual:\n{act}\n");
                    return Err(format!("Different end results in {}", p.display()));
                }
                _ => {}
            }
        }
    } else {
        p = p.with_extension(format!("actual.txt"))
    }
    let Ok(mut file) = fs::File::create(&p) else {
        return Err(format!("Could not create file {}", p.display()));
    };
    let tests = res
        .into_iter()
        .map(|(c, expected, actual)| {
            once(c)
                .chain(actual.iter().cloned().take(expected.len()))
                .collect::<Vec<_>>()
                .join(STAGE_SEP)
        })
        .collect::<Vec<_>>()
        .join(TEST_SEP);
    if let Err(e) = file.write_all(&tests.as_bytes()) {
        return Err(format!("Could not write file {}: {e}", p.display()));
    }
    if overwrite_tests {
        return Ok(());
    }
    for (failed, stage) in failed.into_iter().zip(STAGES) {
        if failed.is_empty() {
            continue;
        }
        let n = failed.len();
        for failure in failed {
            eprintln!("\n--- {} ---\n", p.display());
            eprintln!("Code:\n{}\n", failure.code);
            eprintln!("Expected:\n{}\n", failure.expected);
            eprintln!("Actual:\n{}\n", failure.actual);
        }
        if n == 1 {
            return Err(format!("test failed while trying to {stage} code."));
        } else {
            return Err(format!("{n} tests failed while trying to {stage} code."));
        }
    }
    Ok(())
}

fn test(path: PathBuf) -> Result<(), String> {
    let tests = parse_tests(path.clone())?;
    let mut results = vec![];
    for (code, expected) in tests {
        let (mut actual, compiled) = test_without_run(&code);
        for vm in compiled {
            match vm.run() {
                Ok(State::Done(v)) => actual.push(v.to_string()),
                Ok(State::Resumable(vm)) => {
                    actual.push(format!("{}!({})", vm.effect(), vm.arg.to_string()))
                }
                Err(e) => actual.push(format!("Error at op {e}")),
            }
        }
        results.push((code, expected, actual));
    }
    report(path, results)
}

#[test]
fn parse_ok() -> Result<(), String> {
    test(PathBuf::from("tests/parse_ok.txt"))
}

#[test]
fn parse_err() -> Result<(), String> {
    test(PathBuf::from("tests/parse_err.txt"))
}

#[test]
fn desugar_err() -> Result<(), String> {
    test(PathBuf::from("tests/desugar_err.txt"))
}

#[test]
fn run_apply() -> Result<(), String> {
    test(PathBuf::from("tests/run_apply.txt"))
}

#[test]
fn run_fn() -> Result<(), String> {
    test(PathBuf::from("tests/run_fn.txt"))
}

#[test]
fn run_block() -> Result<(), String> {
    test(PathBuf::from("tests/run_block.txt"))
}

#[test]
fn run_if() -> Result<(), String> {
    test(PathBuf::from("tests/run_if.txt"))
}

#[test]
fn run_macro() -> Result<(), String> {
    test(PathBuf::from("tests/run_macro.txt"))
}

#[test]
fn run_pop() -> Result<(), String> {
    test(PathBuf::from("tests/run_pop.txt"))
}

#[test]
fn run_rec() -> Result<(), String> {
    test(PathBuf::from("tests/run_rec.txt"))
}

fn test_with_effects(path: PathBuf) -> Result<(), String> {
    let tests = parse_tests(path.clone())?;
    let mut results = vec![];
    for (code, expected) in tests {
        let (mut actual, compiled) = test_without_run(&code);
        for vm in compiled {
            let mut printed = vec![];
            let mut result = vm.run();
            loop {
                match result {
                    Ok(State::Resumable(mut vm)) => {
                        let arg = vm.arg.to_string();
                        match vm.effect() {
                            "print" => {
                                printed.push(format!("\"{arg}\"\n"));
                                let nil = vm.serialize(&()).unwrap();
                                result = vm.resume(nil);
                            }
                            "load" => {
                                let code = vm.arg.deserialize::<String>().unwrap();
                                let start = vm.arg.bytecode.load(&code).unwrap();
                                result = vm.resume_at(start);
                            }
                            name => break actual.push(format!("{name}!({arg})")),
                        }
                    }
                    Ok(State::Done(v)) => {
                        break actual.push(printed.join("") + &v.to_string());
                    }
                    Err(e) => break actual.push(format!("Error at op {e}")),
                }
            }
        }
        results.push((code, expected, actual));
    }
    report(path, results)
}

#[test]
fn run_with_effects() -> Result<(), String> {
    test_with_effects(PathBuf::from("tests/run_with_effects.txt"))
}

#[test]
fn run_with_rec_effects() -> Result<(), String> {
    test_with_effects(PathBuf::from("tests/run_with_rec_effects.txt"))
}

#[test]
fn run_with_std_fns() -> Result<(), String> {
    test(PathBuf::from("tests/run_with_std_fns.txt"))
}
