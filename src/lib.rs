use std::{
    cmp::max,
    iter::{Peekable, once},
    rc::Rc,
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
    Call(Call<'code>, Vec<Ast<'code>>),
}

#[derive(Debug, Clone)]
enum Call<'code> {
    Infix(&'code str),
    Prefix(Box<Ast<'code>>),
    Keyword(Vec<&'code str>),
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
        let mut keywords = vec![];
        let mut args = vec![];
        while let Some((Tok::Keyword, i, s)) = toks.peek().copied() {
            toks.next();
            keywords.push(s);
            match _infix(toks) {
                Ok(arg) => args.push(arg),
                Err(E(_, tok, _E::Value)) => return Err(E(i, tok, _E::KeywordArg(s))),
                Err(e) => return Err(e),
            }
        }
        Ok(Ast(pos, A::Call(Call::Keyword(keywords), args)))
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
        Ok(Ast(i, A::Call(Call::Infix(f), args)))
    }
    fn _prefix<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, E<'c>> {
        let mut expr = _value(toks)?;
        let pos = expr.0;
        while let Some((Tok::LParen, i, _)) = toks.peek().copied() {
            toks.next();
            let args = _exprs(toks, i, Some(Tok::RParen))?;
            expr = Ast(pos, A::Call(Call::Prefix(Box::new(expr)), args));
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
            A::Call(Call::Prefix(f), xs) => f.size() + xs.iter().map(|x| x.size()).sum::<usize>(),
            A::Call(_, xs) => 1 + xs.iter().map(|x| x.size()).sum::<usize>(),
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
            (A::Call(Call::Prefix(f), xs), false) => {
                f.pretty(lvl) + "(" + &one_line(xs, lvl + 1) + ")"
            }
            (A::Call(Call::Prefix(f), xs), true) => {
                f.pretty(lvl) + "(" + &multi_line(xs, lvl + 1) + ")"
            }
            (A::Call(Call::Infix(f), args), _) => {
                let args = args.iter().map(|a| match a.1 {
                    A::Call(Call::Keyword(_) | Call::Infix(_), _) => format!("({})", a.pretty(lvl)),
                    _ => a.pretty(lvl),
                });
                args.collect::<Vec<_>>().join(&format!(" {f} "))
            }
            (A::Call(Call::Keyword(keywords), items), _) => {
                let args = keywords
                    .iter()
                    .zip(items.iter())
                    .map(|(k, arg)| match arg.1 {
                        A::Call(Call::Keyword(_), _) => format!("{k} ({})", arg.pretty(lvl)),
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
    fn resolve_var(v: &str, ctx: &Ctx) -> Option<usize> {
        ctx.vars.iter().rev().position(|x| *x == v)
    }
    fn resolve_str<'c>(s: &'c str, ctx: &mut Ctx<'c>) -> usize {
        ctx.strs.iter().position(|x| *x == s).unwrap_or_else(|| {
            ctx.strs.push(s);
            ctx.strs.len() - 1
        })
    }
    fn fold<'c>(
        x: Ast<'c>,
        mut xs: IntoIter<Ast<'c>>,
        ctx: &mut Ctx<'c>,
    ) -> Result<Expr, (usize, E)> {
        let x = desugar(x, ctx)?;
        let bindings = ctx.bindings.len();
        match (xs.next(), bindings) {
            (None, _) => Ok(x),
            (Some(next), 0) => {
                ctx.vars.push("");
                let rest = Expr::Abs(Box::new(fold(next, xs, ctx)?));
                ctx.vars.pop();
                Ok(Expr::App(Box::new(rest), Box::new(x)))
            }
            (Some(next), 1..) => {
                ctx.vars.extend(ctx.bindings.drain(..));
                let rest = (0..bindings).fold(fold(next, xs, ctx)?, |x, _| Expr::Abs(Box::new(x)));
                ctx.vars.truncate(ctx.vars.len() - bindings);
                Ok(Expr::App(Box::new(x), Box::new(rest)))
            }
        }
    }
    fn desugar<'c>(Ast(pos, ast): Ast<'c>, ctx: &mut Ctx<'c>) -> Result<Expr, (usize, E)> {
        match ast {
            A::Var(v) => match resolve_var(v, ctx) {
                Some(v) => Ok(Expr::Var(v)),
                None if v == "=" => Ok(Expr::Abs(Box::new(Expr::Abs(Box::new(Expr::Abs(
                    Box::new(Expr::App(Box::new(Expr::Var(0)), Box::new(Expr::Var(1)))),
                )))))),
                None if v == "=>" => Ok(Expr::Abs(Box::new(Expr::Abs(Box::new(Expr::Var(0)))))),
                None if v == "if" => Ok(Expr::If),
                None if v == "pop" => Ok(Expr::Pop),
                None => Err((pos, E::UnboundVar(v.to_string()))),
            },
            A::String(s) => Ok(Expr::String(resolve_str(s, ctx))),
            A::Binding(b) => {
                ctx.bindings.push(&b[1..]);
                Ok(Expr::String(resolve_str(&b[1..], ctx)))
            }
            A::Block(items) => {
                let mut items = items.into_iter();
                let bindings = ctx.bindings.len();
                ctx.vars.extend(ctx.bindings.drain(..));
                if bindings == 0 {
                    ctx.vars.push("");
                }
                let Some(x) = items.next() else {
                    return Err((pos, E::EmptyBlock));
                };
                let mut expr = fold(x, items, ctx)?;
                for _ in 0..max(1, bindings) {
                    ctx.vars.pop();
                    expr = Expr::Abs(Box::new(expr))
                }
                Ok(expr)
            }
            A::Call(call, args) => {
                let mut f = match call {
                    Call::Infix(f) => desugar(Ast(pos, A::Var(f)), ctx)?,
                    Call::Prefix(f) => desugar(*f, ctx)?,
                    Call::Keyword(keywords) => match resolve_var(&keywords.join(""), ctx) {
                        Some(v) => Expr::Var(v),
                        None => return Err((pos, E::UnboundVar(keywords.join("")))),
                    },
                };
                for arg in args {
                    f = Expr::App(Box::new(f), Box::new(desugar(arg, ctx)?));
                }
                Ok(f)
            }
        }
    }
    let mut ctx = Ctx {
        vars: vec![],
        strs: vec![],
        bindings: vec![],
    };
    match desugar(Ast(0, A::Block(block)), &mut ctx) {
        Err((i, E::UnboundVar(v))) => Err(format!("Unbound variable '{v}' at {}", pos_at(i, code))),
        Err((i, E::EmptyBlock)) => Err(format!("Empty block at {}", pos_at(i, code))),
        Ok(Expr::Abs(body)) => Ok((*body, ctx.strs)),
        Ok(_) => panic!("Expected the main block to be desugared to an abstraction!"),
    }
}

#[derive(Debug, Clone)]
pub struct Bytecode<'code>(Vec<&'code str>, Vec<Op>, usize);

#[derive(Debug, Clone, Copy)]
enum Op {
    PushVar(usize),
    PushString(usize),
    PushFn(usize),
    Apply,
    Return,
}

fn compile(expr: Expr, ops: &mut Vec<Op>, fns: &mut Vec<Op>) {
    match expr {
        Expr::Var(v) => ops.push(Op::PushVar(v)),
        Expr::String(s) => ops.push(Op::PushString(s)),
        Expr::Abs(body) => {
            let mut f = vec![];
            compile(*body, &mut f, fns);
            f.push(Op::Return);
            ops.push(Op::PushFn(fns.len()));
            fns.extend(f);
        }
        Expr::App(f, arg) => {
            compile(*arg, ops, fns);
            compile(*f, ops, fns);
            ops.push(Op::Apply);
        }
        Expr::If => todo!(),
        Expr::Pop => todo!(),
    }
}

impl<'code> Prg<'code> {
    pub fn compile(self) -> Result<Bytecode<'code>, String> {
        let (expr, strs) = desugar(self)?;
        let mut ops = vec![];
        let mut fns = vec![];
        compile(expr, &mut ops, &mut fns);
        let start = fns.len();
        fns.extend(ops);
        Ok(Bytecode(strs, fns, start))
    }
}

#[derive(Debug, Clone)]
pub enum V {
    Fn(usize, usize),
    String(usize),
    Record(usize, Vec<Rc<V>>),
    Closure(usize, Rc<Vec<V>>),
}

impl Bytecode<'_> {
    pub fn run(self) -> Result<String, usize> {
        self.run_with_trace(true)
    }

    pub fn run_with_trace(self, trace: bool) -> Result<String, usize> {
        let Self(strs, ops, mut ip) = self;
        let mut vars = vec![];
        let mut values = vec![];
        let mut frames = vec![];
        while ip < ops.len() {
            let op = ip;
            ip += 1;
            match ops[op] {
                Op::PushVar(v) => {
                    let v: &V = &vars[vars.len() - 1 - v];
                    values.push(v.clone());
                }
                Op::PushString(s) => values.push(V::String(s)),
                Op::PushFn(c) => values.push(V::Fn(c, frames.len())),
                Op::Apply => {
                    let (f, mut arg) = (values.pop().ok_or(op)?, values.pop().ok_or(op)?);
                    if let V::Fn(c, f) = arg {
                        if f == frames.len() {
                            arg = V::Closure(c, Rc::new(vars.clone()));
                        }
                    }
                    match (f, arg) {
                        (V::Closure(c, captured), arg) => {
                            // TODO: if the next op is an Op::Return, we might want to do TCO
                            frames.push((captured.len() + 1, ip));
                            vars.extend(captured.iter().cloned());
                            vars.push(arg);
                            ip = c;
                        }
                        (V::Fn(c, _), arg) => {
                            frames.push((1, ip));
                            vars.push(arg);
                            ip = c;
                        }
                        (V::String(s), arg) => values.push(V::Record(s, vec![Rc::new(arg)])),
                        (V::Record(s, mut items), arg) => {
                            items.push(Rc::new(arg));
                            values.push(V::Record(s, items))
                        }
                    }
                }
                Op::Return => {
                    let (args, ret) = frames.pop().ok_or(op)?;
                    match (values.last().cloned(), ops.get(ret)) {
                        (Some(V::Fn(c, f)), Some(Op::Apply)) if f == frames.len() + 1 => {
                            let _f = values.pop();
                            let mut arg = values.pop().ok_or(op)?;
                            if let V::Fn(c, f) = arg {
                                if f == frames.len() {
                                    let captured = Rc::new(vars[..vars.len() - args].to_vec());
                                    arg = V::Closure(c, captured);
                                }
                            }
                            frames.push((args + 1, ret + 1));
                            vars.push(arg);
                            ip = c;
                        }
                        (Some(V::Fn(c, f)), _) if f == frames.len() + 1 => {
                            let _f = values.pop();
                            values.push(V::Closure(c, Rc::new(vars.clone())));
                            vars.truncate(vars.len() - args);
                            ip = ret
                        }
                        _ => {
                            // TODO: can I find a test case that fails if I remove the following truncate()?
                            vars.truncate(vars.len() - args);
                            ip = ret
                        }
                    }
                }
            }
            if trace {
                println!("==> {op}: {:?}", ops[op]);
                for (i, v) in vars.iter().rev().enumerate() {
                    println!("        {i}: {}", pretty(v, &strs));
                }
                for v in &values {
                    println!("  {}", pretty(v, &strs));
                }
                if ip != op + 1 {
                    println!("====> jump to {ip}");
                }
            }
        }
        fn pretty(v: &V, strs: &[&str]) -> String {
            match v {
                V::String(s) => strs[*s].to_string(),
                V::Fn(c, frame) => format!("{c}(frame:{frame})"),
                V::Closure(c, vs) => {
                    let closed = vs.iter().map(|v| pretty(v, strs)).collect::<Vec<_>>();
                    format!("{c} [{}]", closed.join(", "))
                }
                V::Record(s, vs) => {
                    let items = vs.iter().map(|v| pretty(v, strs)).collect::<Vec<_>>();
                    format!("{}({})", pretty(&V::String(*s), strs), items.join(", "))
                }
            }
        }
        Ok(pretty(&values.pop().ok_or(ip)?, &strs))
    }
}

impl std::fmt::Display for Bytecode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, str) in self.0.iter().enumerate() {
            write!(f, "{i:05} -> {str}\n")?;
        }
        for (i, op) in self.1.iter().enumerate() {
            write!(f, "\n{i:05}: {op:?}")?;
            if let Op::Return = op {
                f.write_str("\n")?;
            }
        }
        f.write_str("\n")
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
    fn eval_record() {
        let code = "Foo(Bar, Baz)";
        let parsed = parse(code).unwrap();
        let bytecode = parsed.compile().unwrap();
        println!("{bytecode}");
        let v = bytecode.run().unwrap();
        assert_eq!(v.to_string(), "Foo(Bar, Baz)");
    }

    #[test]
    fn eval_raw_app1() {
        // (\x.x) "Foo"
        let expr = Expr::App(
            Box::new(Expr::Abs(Box::new(Expr::Var(0)))),
            Box::new(Expr::String(0)),
        );
        let mut ops = vec![];
        let mut fns = vec![];
        compile(expr, &mut ops, &mut fns);
        let start = fns.len();
        fns.extend(ops);
        let bytecode = Bytecode(vec!["Foo"], fns, start);
        println!("{bytecode}");
        let v = bytecode.run().unwrap();
        assert_eq!(v.to_string(), "Foo");
    }

    #[test]
    fn eval_raw_app2() {
        // ((\x.\y.y) "Foo") "Bar"
        let expr = Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::Abs(Box::new(Expr::Abs(Box::new(Expr::Var(0)))))),
                Box::new(Expr::String(0)),
            )),
            Box::new(Expr::String(1)),
        );
        let mut ops = vec![];
        let mut fns = vec![];
        compile(expr, &mut ops, &mut fns);
        let start = fns.len();
        fns.extend(ops);
        let bytecode = Bytecode(vec!["Foo", "Bar"], fns, start);
        println!("{bytecode}");
        let v = bytecode.run().unwrap();
        assert_eq!(v.to_string(), "Bar");
    }

    #[test]
    fn eval_raw_app3() {
        // (((\x.\y.\z.Vec(z, x, z)) "Foo") "Bar") "Baz"
        let expr = Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::App(
                    Box::new(Expr::Abs(Box::new(Expr::Abs(Box::new(Expr::Abs(
                        Box::new(Expr::App(
                            Box::new(Expr::App(
                                Box::new(Expr::App(
                                    Box::new(Expr::String(3)),
                                    Box::new(Expr::Var(0)),
                                )),
                                Box::new(Expr::Var(2)),
                            )),
                            Box::new(Expr::Var(0)),
                        )),
                    )))))),
                    Box::new(Expr::String(0)),
                )),
                Box::new(Expr::String(1)),
            )),
            Box::new(Expr::String(2)),
        );
        let mut ops = vec![];
        let mut fns = vec![];
        compile(expr, &mut ops, &mut fns);
        let start = fns.len();
        fns.extend(ops);
        let bytecode = Bytecode(vec!["Foo", "Bar", "Baz", "Vec"], fns, start);
        println!("{bytecode}");
        let v = bytecode.run().unwrap();
        assert_eq!(v.to_string(), "Vec(Baz, Foo, Baz)");
    }

    #[test]
    fn eval_fn_app1() {
        let code = "('x => { Bar })(Foo)";
        let parsed = parse(code).unwrap();
        let bytecode = parsed.compile().unwrap();
        println!("{bytecode}");
        let v = bytecode.run().unwrap();
        assert_eq!(v.to_string(), "Bar");
    }

    #[test]
    fn eval_fn_app2() {
        let code = "('x => { x })(Foo)";
        let parsed = parse(code).unwrap();
        let bytecode = parsed.compile().unwrap();
        println!("{bytecode}");
        let v = bytecode.run().unwrap();
        assert_eq!(v.to_string(), "Foo");
    }

    #[test]
    fn eval_fn_app3() {
        let code = "('x => { 'y => { x } })(Foo, Bar)";
        let parsed = parse(code).unwrap();
        let bytecode = parsed.compile().unwrap();
        println!("{bytecode}");
        let v = bytecode.run().unwrap();
        assert_eq!(v.to_string(), "Foo");
    }

    #[test]
    fn eval_fn_app4() {
        let code = "('x => { 'y => { 'z => { Vec(z, x, z) } } })(Foo, Bar, Baz)";
        let parsed = parse(code).unwrap();
        let bytecode = parsed.compile().unwrap();
        println!("{bytecode}");
        let v = bytecode.run().unwrap();
        assert_eq!(v.to_string(), "Vec(Baz, Foo, Baz)");
    }

    #[test]
    fn eval_block() {
        let code = "'x = Foo, 'y = Bar, Pair(x, y)";
        let parsed = parse(code).unwrap();
        let desugared = desugar(parsed.clone()).unwrap();
        println!("{:#?}", desugared);
        let bytecode = parsed.compile().unwrap();
        println!("{bytecode}");
        let v = bytecode.run().unwrap();
        assert_eq!(v.to_string(), "Pair(Foo, Bar)");
    }
}
