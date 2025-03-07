use std::{
    collections::HashMap,
    env::args,
    fs::{File, read_to_string},
    io::Write,
    path::Path,
    process::ExitCode,
    sync::Arc,
};

use vorpal::{Eval, Resumable, Val, parse};

fn main() -> ExitCode {
    let mut args = args();
    let bin = args.next().unwrap_or_else(|| "voblog".into());
    let usage = format!("Usage: {bin} <file>");
    let Some(file_name) = args.next() else {
        eprintln!("{usage}");
        return ExitCode::FAILURE;
    };
    let file_path = Path::new(&file_name);
    match read_to_string(&file_path) {
        Err(e) => {
            eprintln!("Could not read {file_name}: {e}");
            ExitCode::FAILURE
        }
        Ok(md) => match parse(include_str!("../vorpal/blog.vo")) {
            Ok(parsed) => {
                let mut chars = md.chars();
                let handlers = Arc::new(HashMap::from_iter(vec![
                    ("read-char!".to_string(), 1),
                    ("write-strs!".to_string(), 1),
                ]));
                let mut result = match parsed.eval_with_handlers(&handlers) {
                    Ok(r) => r,
                    Err(e) => {
                        eprintln!("Eval error: {e}");
                        return ExitCode::FAILURE;
                    }
                };
                loop {
                    match result {
                        Eval::Pending(mut cont, handler) => match cont.name(&handler) {
                            Ok("read-char!") => {
                                let c = match chars.next() {
                                    Some(c) => c.to_string(),
                                    None => "".to_string(),
                                };
                                let c = cont.intern(c);
                                match cont.resume(c, &handlers) {
                                    Ok(r) => result = r,
                                    Err(e) => {
                                        eprintln!("{e}");
                                        return ExitCode::FAILURE;
                                    }
                                }
                            }
                            Ok("write-strs!") => {
                                let Some(strs) = handler.args().pop() else {
                                    eprintln!("Handler write-strs! was called without argument!");
                                    return ExitCode::FAILURE;
                                };
                                let Some(cons_tag) = cont.str_to_tag("Cons") else {
                                    eprintln!("Expected \"Cons\" to exist as a tag!");
                                    return ExitCode::FAILURE;
                                };
                                let Some(nil_tag) = cont.str_to_tag("Nil") else {
                                    eprintln!("Expected \"Nil\" to exist as a tag!");
                                    return ExitCode::FAILURE;
                                };
                                fn as_vec(
                                    mut v: Val,
                                    cons: usize,
                                    nil: usize,
                                ) -> Result<Vec<Val>, Val> {
                                    let mut result = vec![];
                                    loop {
                                        match v {
                                            Val::Tag(t, _, _) if t == nil => {
                                                break Ok(result);
                                            }
                                            Val::Tag(t, vals, m) if t == cons => {
                                                let mut vals = vals.into_iter();
                                                if let Some(x) = vals.next() {
                                                    if let Some(xs) = vals.next() {
                                                        result.push(x);
                                                        v = xs;
                                                        continue;
                                                    }
                                                }
                                                break Err(Val::Tag(t, vals.collect(), m));
                                            }
                                            v => break Err(v),
                                        }
                                    }
                                }
                                fn join(res: &Resumable, vals: Vec<Val>) -> String {
                                    let mut str = String::new();
                                    for v in vals {
                                        match v {
                                            Val::Tag(t, _, _) => match res.tag_to_str(t) {
                                                Some(t) => str += t,
                                                None => str += &format!("'{t}"),
                                            },
                                            v => str += &format!("{v}"),
                                        }
                                    }
                                    str
                                }
                                let v = match as_vec(strs, cons_tag, nil_tag) {
                                    Ok(v) => v,
                                    Err(v) => {
                                        eprintln!("Invalid cons list, found {v}");
                                        return ExitCode::FAILURE;
                                    }
                                };
                                let out_path = file_path.with_extension("html");
                                match File::create(&out_path) {
                                    Ok(mut f) => match write!(f, "{}", join(&cont, v)) {
                                        Ok(_) => return ExitCode::SUCCESS,
                                        Err(e) => {
                                            eprintln!("Could not write to file {file_name}: {e}");
                                            return ExitCode::FAILURE;
                                        }
                                    },
                                    Err(e) => {
                                        eprintln!("Could not write {file_name}: {e}");
                                        return ExitCode::FAILURE;
                                    }
                                }
                            }
                            Ok(unhandled) => {
                                eprintln!("Uncaught handler: {unhandled}");
                                return ExitCode::FAILURE;
                            }
                            Err(h) => {
                                eprintln!("Unknown handler: {h}");
                                return ExitCode::FAILURE;
                            }
                        },
                        Eval::Val(val) => {
                            println!("{val}");
                            return ExitCode::SUCCESS;
                        }
                    }
                }
            }
            Err(e) => {
                eprintln!("Parse error: {e}");
                ExitCode::FAILURE
            }
        },
    }
}
