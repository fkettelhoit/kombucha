use kombucha::compile2::Ast;

fn pretty(ast: &Ast, lvl: usize, buf: &mut String) {
    let indent = "  ";
    match ast {
        Ast::Var(_, s) => buf.push_str(s),
        Ast::String(_, s) => buf.push_str(&format!("\"{s}\"")),
        Ast::List(_, items) => {
            if items.is_empty() {
                return buf.push_str("[]");
            }
            buf.push_str("[\n");
            for (i, item) in items.iter().enumerate() {
                if i != 0 {
                    buf.push('\n');
                }
                buf.push_str(&indent.repeat(lvl + 1));
                pretty(item, lvl + 1, buf);
            }
            buf.push('\n');
            buf.push_str(&indent.repeat(lvl));
            buf.push(']');
        }
        Ast::Tuple(_, items) => {
            if items.is_empty() {
                return buf.push_str("()");
            }
            buf.push_str("(\n");
            for (i, item) in items.iter().enumerate() {
                if i != 0 {
                    buf.push('\n');
                }
                buf.push_str(&indent.repeat(lvl + 1));
                pretty(item, lvl + 1, buf);
            }
            buf.push('\n');
            buf.push_str(&indent.repeat(lvl));
            buf.push(')');
        }
        Ast::Block(_, items) => {
            if items.is_empty() {
                return buf.push_str("{}");
            }
            buf.push_str("{\n");
            for (i, item) in items.iter().enumerate() {
                if i != 0 {
                    buf.push('\n');
                }
                buf.push_str(&indent.repeat(lvl + 1));
                pretty(item, lvl + 1, buf);
            }
            buf.push('\n');
            buf.push_str(&indent.repeat(lvl));
            buf.push('}');
        }
        Ast::Infix(op, [left, right], trailing) => {
            buf.push('(');
            pretty(&op, lvl, buf);
            buf.push('\n');
            buf.push_str(&indent.repeat(lvl + 1));
            pretty(&left, lvl + 1, buf);
            buf.push('\n');
            buf.push_str(&indent.repeat(lvl + 1));
            pretty(&right, lvl + 1, buf);
            if let Some(trailing) = trailing {
                buf.push('\n');
                buf.push_str(&indent.repeat(lvl + 1));
                pretty(&trailing, lvl + 1, buf);
            }
            buf.push(')');
        }
        Ast::Prefix(f, args) => {
            buf.push('(');
            pretty(&f, lvl, buf);
            for arg in args {
                buf.push('\n');
                buf.push_str(&indent.repeat(lvl + 1));
                pretty(arg, lvl + 1, buf);
            }
            buf.push(')');
        }
    }
}

fn parse(code: &str) -> String {
    match kombucha::compile2::parse(code) {
        Ok(exprs) => {
            let mut buf = String::new();
            for item in exprs.iter() {
                pretty(item, 0, &mut buf);
                buf.push('\n');
            }
            buf
        }
        Err(e) => e,
    }
}

fn test(tests: &str) -> Result<(), String> {
    let tests = tests.split("\n\n---\n\n").collect::<Vec<_>>();
    let all = tests.len();
    let mut failed = 0;
    for test in tests {
        let test = test.split("\n\n").collect::<Vec<_>>();
        let (code, expected) = (test[0], test[1]);
        let actual = parse(code);
        if expected.trim() != actual.trim() {
            failed += 1;
            println!("\n{code}\n");
            println!("EXPECTED:\n{expected}");
            println!("ACTUAL:\n{actual}");
        }
    }
    if failed > 0 { Err(format!("{failed}/{all} tests failed")) } else { Ok(()) }
}

#[test]
fn parse_ok() -> Result<(), String> {
    test(include_str!("./_parse_ok.txt"))
}

#[test]
fn parse_err() -> Result<(), String> {
    test(include_str!("./_parse_err.txt"))
}
