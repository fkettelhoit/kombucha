use std::{
    cmp::max,
    iter::{Peekable, once},
    vec::IntoIter,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tok {
    Symbol,
    String,
    Keyword,
    Binding,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Separator,
}

fn scan(code: &str) -> Vec<(Tok, usize, &str)> {
    let mut toks = vec![];
    let mut i = 0;
    for (j, c) in code.chars().chain(once(' ')).enumerate() {
        let tok = match c {
            '(' => Some(Tok::LParen),
            ')' => Some(Tok::RParen),
            '{' => Some(Tok::LBrace),
            '}' => Some(Tok::RBrace),
            ',' | '\n' => Some(Tok::Separator),
            _ => None,
        };
        if tok.is_some() || c.is_ascii_whitespace() {
            if let (Some(n), Some(l)) = (code[i..j].chars().next(), code[i..j].chars().last()) {
                let tok = match (n, l) {
                    (n, _) if n.is_ascii_uppercase() => Tok::String,
                    ('\'', _) => Tok::Binding,
                    (_, ':') => Tok::Keyword,
                    _ => Tok::Symbol,
                };
                toks.push((tok, i, &code[i..j]));
            }
            i = j + 1;
        }
        if let Some(tok) = tok {
            toks.push((tok, j, &code[j..j + 1]));
        }
    }
    toks
}

#[derive(Debug, Clone)]
struct Ast<'code>(usize, A<'code>);

#[derive(Debug, Clone)]
enum A<'code> {
    Var(&'code str),
    String(&'code str),
    Binding(&'code str),
    Block(Vec<Ast<'code>>),
    PrefixCall(Box<Ast<'code>>, Vec<Ast<'code>>),
    InfixCall(&'code str, Vec<Ast<'code>>),
    KeywordCall(Vec<(&'code str, Ast<'code>)>),
}

#[derive(Debug, Clone)]
pub struct Prg<'code>(Vec<Ast<'code>>, &'code str);

fn pos_at(i: usize, code: &str) -> String {
    let (mut line, mut col) = (1, 1);
    for c in code.chars().take(i) {
        if c == '\n' {
            line += 1;
            col = 0;
        }
        col += 1;
    }
    format!("line {line}, col {col}")
}

pub fn parse(code: &str) -> Result<Prg<'_>, String> {
    struct E<'c>(usize, Option<(usize, &'c str)>, _E<'c>);
    enum _E<'c> {
        RParen,
        RBrace,
        Sep,
        Value,
        InfixFn(&'c str),
        InfixArg(&'c str),
        KeywordArg(&'c str),
    }
    type Toks<'c> = Peekable<IntoIter<(Tok, usize, &'c str)>>;
    fn _expr<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, E<'c>> {
        let Some((Tok::Keyword, pos, _)) = toks.peek().copied() else {
            return _infix(toks);
        };
        let mut args = vec![];
        while let Some((Tok::Keyword, i, s)) = toks.peek().copied() {
            toks.next();
            match _infix(toks) {
                Ok(arg) => args.push((s, arg)),
                Err(E(_, tok, _E::Value)) => return Err(E(i, tok, _E::KeywordArg(s))),
                Err(e) => return Err(e),
            }
        }
        Ok(Ast(pos, A::KeywordCall(args)))
    }
    fn _infix<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, E<'c>> {
        let expr = _prefix(toks)?;
        let Some((Tok::Symbol, i, f)) = toks.peek().copied() else {
            return Ok(expr);
        };
        let mut args = vec![expr];
        while let Some((Tok::Symbol, j, s)) = toks.peek().copied() {
            toks.next();
            if f != s {
                return Err(E(i, Some((j, s)), _E::InfixFn(f)));
            }
            match _prefix(toks) {
                Ok(arg) => args.push(arg),
                Err(E(_, tok, _E::Value)) => return Err(E(j, tok, _E::InfixArg(s))),
                Err(e) => return Err(e),
            }
        }
        Ok(Ast(i, A::InfixCall(f, args)))
    }
    fn _prefix<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, E<'c>> {
        let mut expr = _value(toks)?;
        let pos = expr.0;
        while let Some((Tok::LParen, i, _)) = toks.peek().copied() {
            toks.next();
            let f = A::PrefixCall(Box::new(expr), _exprs(toks, i, Some(Tok::RParen))?);
            expr = Ast(pos, f);
        }
        Ok(expr)
    }
    fn _value<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, E<'c>> {
        match toks.next() {
            None => Err(E(0, None, _E::Value)),
            Some((t, i, s)) => match t {
                Tok::Symbol => Ok(Ast(i, A::Var(s))),
                Tok::String => Ok(Ast(i, A::String(s))),
                Tok::Binding => Ok(Ast(i, A::Binding(s))),
                Tok::LParen => match _expr(toks) {
                    Err(E(i, tok, _E::Value)) => Err(E(i, tok, _E::RParen)),
                    Err(e) => Err(e),
                    Ok(expr) => match toks.next() {
                        Some((Tok::RParen, _, _)) => Ok(expr),
                        tok => Err(E(i, tok.map(|(_, i, s)| (i, s)), _E::RParen)),
                    },
                },
                Tok::LBrace => Ok(Ast(i, A::Block(_exprs(toks, i, Some(Tok::RBrace))?))),
                Tok::RParen | Tok::RBrace | Tok::Separator | Tok::Keyword => {
                    Err(E(i, Some((i, s)), _E::Value))
                }
            },
        }
    }
    fn _exprs<'c>(toks: &mut Toks<'c>, i: usize, t: Option<Tok>) -> Result<Vec<Ast<'c>>, E<'c>> {
        let mut exprs = vec![];
        let mut needs_sep = false;
        loop {
            match toks.peek().copied() {
                tok if tok.map(|t| t.0) == t => {
                    toks.next();
                    return Ok(exprs);
                }
                Some((Tok::Separator, _, _)) => {
                    toks.next();
                    needs_sep = false;
                }
                Some((_, i, s)) if needs_sep => return Err(E(i, Some((i, s)), _E::Sep)),
                _ => match _expr(toks) {
                    Ok(expr) => {
                        exprs.push(expr);
                        needs_sep = true;
                    }
                    Err(E(_, tok, _E::Value)) => match t {
                        Some(Tok::RParen) => return Err(E(i, tok, _E::RParen)),
                        _ => return Err(E(i, tok, _E::RBrace)),
                    },
                    Err(e) => return Err(e),
                },
            }
        }
    }
    let mut toks = scan(code).into_iter().peekable();
    match _exprs(&mut toks, 0, None) {
        Ok(exprs) => Ok(Prg(exprs, code)),
        Err(E(i, actual, expected)) => {
            let p = pos_at(i, code);
            let expected = match expected {
                _E::RParen => format!("the '(' at {p} to be closed with ')'"),
                _E::RBrace => format!("the '{{' at {p} to be closed with '}}'"),
                _E::Sep => format!("a ',' or '\\n' to separate the expressions starting at {p}"),
                _E::Value => "a value".to_string(),
                _E::InfixFn(f) => format!("all infix functions starting at {p} to be named '{f}'"),
                _E::InfixArg(f) => format!("an infix argument after the function '{f}' at {p}"),
                _E::KeywordArg(k) => format!("an argument after the keyword '{k}' at {p}"),
            };
            let instead = match actual {
                None => "but the code just ended".to_string(),
                Some((j, s)) => format!("but found '{s}' at {}", pos_at(j, code)),
            };
            Err(format!("Expected {expected}, {instead}"))
        }
    }
}

impl Ast<'_> {
    fn size(&self) -> usize {
        match &self.1 {
            A::Var(_) | A::String(_) | A::Binding(_) => 1,
            A::Block(xs) => xs.iter().map(|x| x.size()).sum::<usize>() + 1,
            A::PrefixCall(f, xs) => f.size() + xs.iter().map(|x| x.size()).sum::<usize>(),
            A::InfixCall(_, xs) => 1 + xs.iter().map(|x| x.size()).sum::<usize>(),
            A::KeywordCall(xs) => xs.iter().map(|(_, x)| x.size() + 1).sum(),
        }
    }

    fn pretty(&self, lvl: usize) -> String {
        fn one_line(xs: &[Ast<'_>], lvl: usize) -> String {
            let xs = xs.iter().map(|x| x.pretty(lvl));
            xs.collect::<Vec<_>>().join(", ")
        }
        fn multi_line(xs: &[Ast<'_>], lvl: usize) -> String {
            let xs = xs.iter().map(|x| "  ".repeat(lvl) + &x.pretty(lvl) + "\n");
            "\n".to_string() + &xs.collect::<String>() + &"  ".repeat(lvl - 1)
        }
        match (&self.1, self.size() >= 10) {
            (A::Var(s) | A::String(s) | A::Binding(s), _) => s.to_string(),
            (A::Block(xs), false) => "{ ".to_string() + &one_line(xs, lvl + 1) + " }",
            (A::Block(xs), true) => "{".to_string() + &multi_line(xs, lvl + 1) + "}",
            (A::PrefixCall(f, xs), false) => f.pretty(lvl) + "(" + &one_line(xs, lvl + 1) + ")",
            (A::PrefixCall(f, xs), true) => f.pretty(lvl) + "(" + &multi_line(xs, lvl + 1) + ")",
            (A::InfixCall(f, args), _) => {
                let args = args.iter().map(|a| match a.1 {
                    A::KeywordCall(_) | A::InfixCall(_, _) => format!("({})", a.pretty(lvl)),
                    _ => a.pretty(lvl),
                });
                args.collect::<Vec<_>>().join(&format!(" {f} "))
            }
            (A::KeywordCall(items), _) => {
                let args = items.iter().map(|(k, arg)| match arg.1 {
                    A::KeywordCall(_) => format!("{k} ({})", arg.pretty(lvl)),
                    _ => format!("{k} {}", arg.pretty(lvl)),
                });
                args.collect::<Vec<_>>().join(" ")
            }
        }
    }
}

impl std::fmt::Display for Prg<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let items = self.0.iter().map(|x| x.pretty(0));
        f.write_str(&items.collect::<Vec<_>>().join("\n\n"))
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Var(usize),
    String(usize),
    Abs(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Fn,
    If,
    Pop,
}

fn desugar<'c>(Prg(block, code): Prg<'c>) -> Result<(Expr, Vec<&'c str>), String> {
    enum E {
        UnboundVar(String),
        EmptyBlock,
    }
    struct Ctx<'c> {
        vars: Vec<&'c str>,
        strs: Vec<&'c str>,
        bindings: Vec<&'c str>,
    }
    fn resolve_var<'c>(c: &Ctx<'c>, v: &str) -> Option<(usize, &'c str)> {
        c.vars.iter().copied().rev().enumerate().find(|x| x.1 == v)
    }
    fn resolve_str<'c>(c: &mut Ctx<'c>, s: &'c str) -> usize {
        match c.strs.iter().copied().rev().enumerate().find(|x| x.1 == s) {
            Some((i, _)) => i,
            None => {
                c.strs.push(s);
                c.strs.len() - 1
            }
        }
    }
    fn desugar<'c>(Ast(pos, ast): Ast<'c>, c: &mut Ctx<'c>) -> Result<Expr, (usize, E)> {
        match ast {
            A::Var(v) => match resolve_var(c, v) {
                Some((v, _)) => Ok(Expr::Var(v)),
                None if v == "=>" => Ok(Expr::Fn),
                None if v == "if" => Ok(Expr::If),
                None if v == "pop" => Ok(Expr::Pop),
                None => Err((pos, E::UnboundVar(v.to_string()))),
            },
            A::String(s) => Ok(Expr::String(resolve_str(c, s))),
            A::Binding(s) => {
                c.bindings.push(&s[1..]);
                Ok(Expr::String(resolve_str(c, &s[1..])))
            }
            A::Block(items) => {
                let mut desugared = vec![];
                for ast in items {
                    let bindings = c.bindings.len();
                    c.vars.extend(c.bindings.drain(..));
                    if bindings == 0 {
                        c.vars.push("")
                    }
                    desugared.push((bindings, desugar(ast, c)?));
                }
                let Some((bindings, mut expr)) = desugared.pop() else {
                    return Err((pos, E::EmptyBlock));
                };
                expr = (0..max(1, bindings)).fold(expr, |x, _| Expr::Abs(Box::new(x)));
                for (bindings, item) in desugared.into_iter().rev() {
                    if bindings == 0 {
                        expr = Expr::App(Box::new(Expr::Abs(Box::new(expr))), Box::new(item));
                    } else {
                        let app = Expr::App(Box::new(item), Box::new(expr));
                        expr = (0..bindings).fold(app, |x, _| Expr::Abs(Box::new(x)));
                    }
                }
                Ok(expr)
            }
            A::PrefixCall(f, args) => app(desugar(*f, c)?, args, c),
            A::InfixCall(f, args) => app(desugar(Ast(pos, A::Var(f)), c)?, args, c),
            A::KeywordCall(items) => {
                let mut f = String::new();
                let mut args = vec![];
                for (keyword, arg) in items {
                    f += keyword;
                    args.push(arg);
                }
                let Some((v, _)) = resolve_var(c, &f) else {
                    return Err((pos, E::UnboundVar(f)));
                };
                app(Expr::Var(v), args, c)
            }
        }
    }
    fn app<'c>(mut f: Expr, args: Vec<Ast<'c>>, c: &mut Ctx<'c>) -> Result<Expr, (usize, E)> {
        let outer_bindings = std::mem::replace(&mut c.bindings, vec![]);
        for arg in args {
            f = Expr::App(Box::new(f), Box::new(desugar(arg, c)?))
        }
        c.bindings.splice(0..0, outer_bindings);
        Ok(f)
    }
    let mut c = Ctx {
        vars: vec![],
        strs: vec![],
        bindings: vec![],
    };
    let nil = resolve_str(&mut c, "Nil");
    match desugar(Ast(0, A::Block(block)), &mut c) {
        Ok(expr) => Ok((
            Expr::App(Box::new(expr), Box::new(Expr::String(nil))),
            c.strs,
        )),
        Err((i, e)) => match e {
            E::UnboundVar(v) => Err(format!("Unbound variable '{v}' at {}", pos_at(i, code))),
            E::EmptyBlock => Err(format!("Empty block at {}", pos_at(i, code))),
        },
    }
}

#[derive(Debug, Clone)]
pub struct Bytecode<'code>(Vec<Op>, usize, Vec<&'code str>, &'code str);

#[derive(Debug, Clone, Copy)]
enum Op {
    PushFn(usize),
    PushString(usize),
    BuiltInFn,
    Call,
    Return,
}

impl<'code> Prg<'code> {
    pub fn compile(self) -> Result<Bytecode<'code>, String> {
        fn compile(expr: Expr, ops: &mut Vec<Op>, fns: &mut Vec<Op>) {
            match expr {
                Expr::Var(_) => todo!(),
                Expr::String(s) => ops.push(Op::PushString(s)),
                Expr::Abs(expr) => {
                    let mut fn_ops = vec![];
                    compile(*expr, &mut fn_ops, fns);
                    fn_ops.push(Op::Return);
                    let f = fns.len();
                    fns.extend(fn_ops);
                    ops.push(Op::PushFn(f));
                }
                Expr::App(f, arg) => {
                    compile(*arg, ops, fns);
                    compile(*f, ops, fns);
                    ops.push(Op::Call);
                }
                Expr::Fn => ops.push(Op::BuiltInFn),
                Expr::If => todo!(),
                Expr::Pop => todo!(),
            }
        }
        let code = self.1;
        let mut main = vec![];
        let mut fns = vec![];
        let (expr, strs) = desugar(self)?;
        compile(expr, &mut main, &mut fns);
        let ip = fns.len();
        fns.extend(main);
        Ok(Bytecode(fns, ip, strs, code))
    }
}

#[derive(Debug)]
pub enum V {
    Fn(usize),
    String(usize),
    Compound(Box<V>, Vec<V>),
}

impl std::fmt::Display for Bytecode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(ops, _, strs, _) = self;
        for (i, str) in strs.iter().enumerate() {
            write!(f, "{i:#6} -> {str}\n")?;
        }
        for (i, op) in ops.iter().enumerate() {
            write!(f, "\n{i:#6}: {op:?}")?;
            if let Op::Return = op {
                f.write_str("\n")?;
            }
        }
        Ok(())
    }
}

impl Bytecode<'_> {
    pub fn run(self) -> Option<String> {
        fn pretty(v: &V, strs: &[&str]) -> String {
            match v {
                V::Fn(f) => f.to_string(),
                V::String(s) => match strs.iter().enumerate().find(|(i, _)| i == s) {
                    Some((_, s)) => s.to_string(),
                    None => panic!("Could not find interned string corresponding to index {s}"),
                },
                V::Compound(f, items) => {
                    let items = items
                        .into_iter()
                        .map(|x| pretty(x, strs))
                        .collect::<Vec<_>>();
                    pretty(f, strs) + "(" + &items.join(", ") + ")"
                }
            }
        }
        let Self(ops, mut ip, strs, _) = self;
        let mut values = vec![];
        let mut calls = vec![];
        println!("\n*** TRACE: ***\n");
        while ip < ops.len() {
            match ops[ip] {
                Op::PushFn(f) => values.push(V::Fn(f)),
                Op::PushString(s) => values.push(V::String(s)),
                Op::BuiltInFn => {
                    let f = values.pop()?;
                    let arg = values.pop()?;
                    match (f, arg) {
                        (V::Fn(f), V::String(_)) => {
                            calls.push(ip + 1);
                            ip = f;
                            continue;
                        }
                        _ => values.push(V::String(0)),
                    }
                }
                Op::Call => match values.pop()? {
                    V::Fn(f) => {
                        calls.push(ip + 1);
                        ip = f;
                        continue;
                    }
                    V::String(s) => {
                        let arg = values.pop()?;
                        values.push(V::Compound(Box::new(V::String(s)), vec![arg]))
                    }
                    V::Compound(f, mut items) => {
                        items.push(values.pop()?);
                        values.push(V::Compound(f, items))
                    }
                },
                Op::Return => ip = calls.pop().unwrap(),
            }
            ip += 1;
            println!("{}:", ip - 1);
            for v in values.iter() {
                println!("  {}", pretty(&v, &strs));
            }
        }
        values.pop().map(|v| pretty(&v, &strs))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_error_unmatched() {
        let code = "{)}";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("')'"));
        assert!(e.contains("line 1, col 2"));
    }

    #[test]
    fn parse_error_infix_mismatch() {
        let code = "a + b - c";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("all infix functions starting at line 1, col 3 to be named '+'"));
        assert!(e.contains("'-' at line 1, col 7"));
    }

    #[test]
    fn parse_error_missing_infix_arg() {
        let code = "a +";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("infix argument after the function '+' at line 1, col 3"));
    }

    #[test]
    fn parse_error_missing_infix_arg_in_call() {
        let code = "f(a +)";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("infix argument after the function '+' at line 1, col 5"));
    }

    #[test]
    fn parse_error_invalid_fn_args() {
        for code in ["f(", "f(a"] {
            let e = parse(code).unwrap_err();
            eprintln!("'{code}': {e}");
            assert!(e.contains("at line 1, col 2"));
            assert!(e.contains("')'"));
        }
    }

    #[test]
    fn parse_error_fn_args_without_separators() {
        let code = "f({a} {b}";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("',' or '\\n'"));
        assert!(e.contains("'{' at line 1, col 7"));
    }

    #[test]
    fn parse_error_fn_args_without_r_paren() {
        let code = "{f(a}";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("'}' at line 1, col 5"));
    }

    #[test]
    fn parse_error_invalid_group() {
        for code in ["(", "(a", "({a} {b})", "({a}, {b})"] {
            let e = parse(code).unwrap_err();
            eprintln!("'{code}': {e}");
            assert!(e.contains("'(' at line 1, col 1"));
            assert!(e.contains("')'"));
        }
    }

    #[test]
    fn parse_error_keyword_as_value() {
        let code = "a + keyword:";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("infix argument"));
        assert!(e.contains("'keyword:' at line 1, col 5"));
    }

    #[test]
    fn parse_error_double_keyword() {
        let code = "foo: bar:";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("argument"));
        assert!(e.contains("'bar:' at line 1, col 6"));
    }

    #[test]
    fn parse_error_separator_as_value() {
        let code = "a + ,";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("infix argument"));
        assert!(e.contains("',' at line 1, col 5"));
    }

    #[test]
    fn parse_error_list_without_closing_bracket() {
        for code in ["x + {a, b", "x + {a, )"] {
            let e = parse(code).unwrap_err();
            eprintln!("'{code}': {e}");
            assert!(e.contains("closed with '}'"));
            assert!(e.contains("at line 1, col 5"));
        }
    }

    #[test]
    fn error_unbound_var() {
        let code = "Foo(f(Bar))";
        let e = parse(code).unwrap().compile().unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("Unbound variable 'f' at line 1, col 5"));
    }

    #[test]
    fn pretty_var() {
        let code = "foo";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_string() {
        let code = "Foo";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_binding() {
        let code = "'foo";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_block() {
        let code = "{ foo, Foo, 'foo }";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_prefix_call() {
        let code = "f(g(), h(Bar), Baz)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_curried_prefix_call() {
        let code = "f(g(x)(y)())";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_infix_call() {
        let code = "f(x) foo g(y)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_grouped_infix_call() {
        let code = "(a * b) + (c * d)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_keyword_call() {
        let code = "if: x == y do: { foo() } else: Bar";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_grouped_keyword_call() {
        let code = "if: x == y do: (foo: Bar baz: Qux) else: Bar";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty() {
        let code = "'foo = Foo

'id = ('x => { x })

f'('x, 'y) = {
  match: Pair(x, y) with: Vec(
    Pair('x, 'x) => { x }
    Pair('x, 'y) => { Mismatch }
    ' => { InvalidPair }
  )
}

if: x == y do: { print(Equal) } else: { print(NotEqual) }

'x = id(id(foo))

if: x == Foo do: Pair(x, x) else: Error";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn eval_compound() {
        let code = "Foo(Bar, Baz)";
        let parsed = parse(code).unwrap();
        let bytecode = parsed.compile().unwrap();
        println!("{bytecode}");
        let v = bytecode.run().unwrap();
        assert_eq!(v.to_string(), "Foo(Bar, Baz)");
    }

    #[test]
    fn eval_fn_app() {
        let code = "('x => { Bar })(Foo)";
        let parsed = parse(code).unwrap();
        let bytecode = parsed.compile().unwrap();
        println!("{bytecode}");
        let v = bytecode.run().unwrap();
        assert_eq!(v.to_string(), "Bar");
    }
}
