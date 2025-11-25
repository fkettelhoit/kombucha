use std::{cmp::max, iter, mem, usize, vec::IntoIter};

use crate::bytecode::{BindType, Bytecode, Ctx, LIST, NULL, Op, Str};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tok<'code> {
    Ident(&'code str),
    Atom(&'code str),
    String(&'code str),
    Keyword(&'code str),
    Binding(usize, BindType, &'code str),
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Separator,
}

fn scan(code: &str) -> Result<Vec<(Tok<'_>, usize, &str)>, (usize, &str)> {
    let mut toks = vec![];
    let mut i = 0;
    let mut chars = code.char_indices().chain(iter::once((code.len(), ' ')));
    while let Some((j, c)) = chars.next() {
        let tok = match c {
            '(' => Some(Tok::LParen),
            ')' => Some(Tok::RParen),
            '[' => Some(Tok::LBracket),
            ']' => Some(Tok::RBracket),
            '{' => Some(Tok::LBrace),
            '}' => Some(Tok::RBrace),
            ',' | '\n' => Some(Tok::Separator),
            _ => None,
        };
        let is_comment = c == '/' && code.get(j + 1..j + 2) == Some("/");
        let is_str_literal = code[j..].chars().skip_while(|&c| c == '#').next() == Some('"');
        if tok.is_some() || c.is_ascii_whitespace() || is_comment || is_str_literal {
            if let (Some(n), Some(l)) = (code[i..j].chars().next(), code[i..j].chars().last()) {
                let tok = match (n, l) {
                    (':' | '#', _) if i + 1 == j => return Err((i, "an empty binding")),
                    (':' | '#', ':') => return Err((i, "a binding used as a keyword")),
                    (':' | '#', _) => {
                        let colons = code[i..].chars().take_while(|c| *c == n).count();
                        let ty = if n == ':' { BindType::Variable } else { BindType::Macro };
                        Tok::Binding(colons - 1, ty, &code[i + colons..j])
                    }
                    (_, ':') => Tok::Keyword(&code[i..j - 1]),
                    (n, _) if n.is_ascii_uppercase() => Tok::Atom(&code[i..j]),
                    _ => Tok::Ident(&code[i..j]),
                };
                toks.push((tok, i, &code[i..j]));
            }
            i = j + 1;
        }
        if let Some(tok) = tok {
            toks.push((tok, j, &code[j..j + 1]));
        } else if is_comment {
            toks.push((Tok::Separator, j, &code[j..j + 1]));
            let (j, _) = chars.find(|(_, c)| *c == '\n').unwrap_or_default();
            i = j + 1;
        } else if is_str_literal {
            let n = code[j..].chars().take_while(|&ch| ch == '#').count();
            let end = format!("\"{}", "#".repeat(n));
            match chars.by_ref().skip(n).map(|(j, _)| j).find(|j| code[*j..].starts_with(&end)) {
                None => return Err((i - 1, "an unclosed string literal")),
                Some(j) => {
                    toks.push((Tok::String(&code[i + n..j]), i - 1, &code[i - 1..j + n + 1]));
                    i = j + n + 1;
                }
            }
        }
    }
    Ok(toks)
}

#[derive(Debug, Clone)]
pub struct Ast(pub usize, pub A);

#[derive(Debug, Clone)]
pub enum A {
    Var(String),
    Atom(String),
    String(String),
    Binding(usize, BindType, String),
    Block(Vec<Ast>),
    Call(Box<Ast>, Vec<Ast>),
}

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

pub fn parse(code: &str) -> Result<Vec<Ast>, String> {
    struct E<'c>(usize, Option<(Tok<'c>, usize, &'c str)>, _E<'c>);
    enum _E<'c> {
        RParen,
        RBracket,
        RBrace,
        Sep,
        Value,
        InfixFn(&'c str),
        InfixArg(&'c str),
        KeywordArg(&'c str),
    }
    type Toks<'c> = iter::Peekable<IntoIter<(Tok<'c>, usize, &'c str)>>;
    fn _list(pos: usize) -> Box<Ast> {
        Box::new(Ast(pos, A::Atom(LIST.to_string())))
    }
    fn _expr<'c>(toks: &mut Toks<'c>) -> Result<Ast, E<'c>> {
        if let Some((Tok::Keyword(k), i, _)) = toks.peek().copied() {
            toks.next();
            let expr = _expr(toks)?;
            return Ok(Ast(i, A::Call(_list(i), vec![Ast(i, A::String(k.to_string())), expr])));
        }
        let Some((Tok::Ident(_) | Tok::Atom(_), _, _)) = toks.peek().copied() else {
            let expr = _prefix(toks)?;
            return _infix(toks, expr);
        };
        let expr = _prefix(toks)?;
        let mut trailing = vec![];
        while let Some((Tok::LBracket | Tok::LBrace, _, _)) = toks.peek().copied() {
            trailing.push(_value(toks)?);
        }
        let mut keyword_args = vec![];
        while let Some((Tok::Keyword(s), i, tok_s)) = toks.peek().copied() {
            toks.next();
            let kw = Ast(i, A::String(s.to_string()));
            match _prefix(toks) {
                Ok(expr) => {
                    keyword_args.push(Ast(i, A::Call(_list(i), vec![kw, _infix(toks, expr)?])))
                }
                Err(E(_, tok, _E::Value)) => return Err(E(i, tok, _E::KeywordArg(tok_s))),
                Err(e) => return Err(e),
            }
        }
        if let Some(Ast(i, _)) = keyword_args.first() {
            trailing.push(Ast(*i, A::Call(_list(*i), keyword_args)));
        }
        match (expr, trailing.len()) {
            (expr, 0) => _infix(toks, expr),
            (Ast(pos, A::Call(f, args)), _) => {
                Ok(Ast(pos, A::Call(f, args.into_iter().chain(trailing).collect())))
            }
            (Ast(pos, value), _) => Ok(Ast(pos, A::Call(Box::new(Ast(pos, value)), trailing))),
        }
    }
    fn _infix<'c>(toks: &mut Toks<'c>, mut x: Ast) -> Result<Ast, E<'c>> {
        let Some((Tok::Ident(f), i, _)) = toks.peek().copied() else {
            return Ok(x);
        };
        while let Some((Tok::Ident(s), j, _)) = toks.peek().copied() {
            toks.next();
            if f != s {
                return Err(E(i, Some((Tok::Ident(s), j, s)), _E::InfixFn(f)));
            }
            match _prefix(toks) {
                Ok(y) => x = Ast(j, A::Call(Box::new(Ast(j, A::Var(f.to_string()))), vec![x, y])),
                Err(E(_, tok, _E::Value)) => return Err(E(j, tok, _E::InfixArg(s))),
                Err(e) => return Err(e),
            }
        }
        Ok(x)
    }
    fn _prefix<'c>(toks: &mut Toks<'c>) -> Result<Ast, E<'c>> {
        let mut expr = _value(toks)?;
        let pos = expr.0;
        while let Some((Tok::LParen, i, _)) = toks.peek().copied() {
            toks.next();
            let args = _exprs(toks, i, Some(Tok::RParen))?;
            expr = Ast(pos, A::Call(Box::new(expr), args));
        }
        Ok(expr)
    }
    fn _value<'c>(toks: &mut Toks<'c>) -> Result<Ast, E<'c>> {
        match toks.next() {
            None => Err(E(0, None, _E::Value)),
            Some((t, i, tok_s)) => match t {
                Tok::Ident(s) => Ok(Ast(i, A::Var(s.to_string()))),
                Tok::Atom(s) => Ok(Ast(i, A::Atom(s.to_string()))),
                Tok::String(s) => Ok(Ast(i, A::String(s.to_string()))),
                Tok::Binding(lvl, c, s) => Ok(Ast(i, A::Binding(lvl, c, s.to_string()))),
                Tok::LParen => match _expr(toks) {
                    Err(E(i, Some((Tok::RParen, _, _)), _E::Value)) => {
                        Ok(Ast(i, A::Atom(NULL.to_string())))
                    }
                    Err(E(i, tok, _E::Value)) => Err(E(i, tok, _E::RParen)),
                    Err(e) => Err(e),
                    Ok(expr) => match toks.next() {
                        Some((Tok::RParen, _, _)) => Ok(expr),
                        tok => Err(E(i, tok, _E::RParen)),
                    },
                },
                Tok::LBracket => match _exprs(toks, i, Some(Tok::RBracket))? {
                    items if items.is_empty() => Ok(*_list(i)),
                    items => Ok(Ast(i, A::Call(_list(i), items))),
                },
                Tok::LBrace => Ok(Ast(i, A::Block(_exprs(toks, i, Some(Tok::RBrace))?))),
                Tok::RParen | Tok::RBracket | Tok::RBrace | Tok::Separator | Tok::Keyword(_) => {
                    Err(E(i, Some((t, i, tok_s)), _E::Value))
                }
            },
        }
    }
    fn _exprs<'c>(toks: &mut Toks<'c>, i: usize, t: Option<Tok>) -> Result<Vec<Ast>, E<'c>> {
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
                Some((t, j, s)) if needs_sep => return Err(E(i + 1, Some((t, j, s)), _E::Sep)),
                _ => match _expr(toks) {
                    Ok(expr) => {
                        exprs.push(expr);
                        needs_sep = true;
                    }
                    Err(E(_, tok, _E::Value)) => match t {
                        Some(Tok::RParen) => return Err(E(i, tok, _E::RParen)),
                        Some(Tok::RBracket) => return Err(E(i, tok, _E::RBracket)),
                        Some(Tok::RBrace) => return Err(E(i, tok, _E::RBrace)),
                        _ => return Err(E(i, tok, _E::Value)),
                    },
                    Err(e) => return Err(e),
                },
            }
        }
    }
    let toks = scan(code).map_err(|(i, msg)| format!("Found {msg} at {}", pos_at(i, code)))?;
    let mut toks = toks.into_iter().peekable();
    match _exprs(&mut toks, 0, None) {
        Ok(exprs) => Ok(exprs),
        Err(E(i, actual, expected)) => {
            let p = pos_at(i, code);
            let expected = match expected {
                _E::RParen => format!("the '(' at {p} to be closed with ')'"),
                _E::RBracket => format!("the '[' at {p} to be closed with ']'"),
                _E::RBrace => format!("the '{{' at {p} to be closed with '}}'"),
                _E::Sep => format!("a ',' or '\\n' to separate the expressions starting at {p}"),
                _E::Value => "an expression".to_string(),
                _E::InfixFn(f) => format!("all infix functions starting at {p} to be named '{f}'"),
                _E::InfixArg(f) => format!("an infix argument after the function '{f}' at {p}"),
                _E::KeywordArg(k) => format!("an argument after the keyword '{k}' at {p}"),
            };
            let instead = match actual {
                None => "but the code just ended".to_string(),
                Some((_, j, s)) => format!("but found '{s}' at {}", pos_at(j, code)),
            };
            Err(format!("Expected {expected}, {instead}"))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Var(usize),
    String(usize),
    Effect(usize),
    Abs(Box<Expr>),
    Rec(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Type(Box<Expr>),
    Unpack([Box<Expr>; 3]),
    Handle([Box<Expr>; 2]),
    Compare([Box<Expr>; 4]),
}

pub fn abs(body: Expr) -> Expr {
    Expr::Abs(Box::new(body))
}

pub fn app(f: Expr, arg: Expr) -> Expr {
    Expr::App(Box::new(f), Box::new(arg))
}

pub fn desugar<'c>(block: Vec<Ast>, code: &'c str, ctx: &mut Ctx) -> Result<Expr, String> {
    fn resolve_var(v: &str, ctx: &Ctx) -> Option<usize> {
        ctx.vars.iter().rev().position(|(_, x)| *x == v)
    }
    fn resolve_str<'c>(s: String, ctx: &mut Ctx) -> usize {
        ctx.strs.iter().position(|x| *x == s).unwrap_or_else(|| {
            ctx.strs.push(s);
            ctx.strs.len() - 1
        })
    }
    fn is_macro(Ast(_, ast): &Ast, ctx: &Ctx) -> bool {
        if let A::Var(v) = ast {
            if let Some((BindType::Macro, _)) = ctx.vars.iter().rev().find(|(_, x)| x == v) {
                return true;
            }
        }
        false
    }
    fn has_bindings(Ast(_, ast): &Ast, ctx: &Ctx) -> bool {
        match ast {
            A::Binding(_, _, _) => true,
            A::Var(_) | A::Atom(_) | A::String(_) | A::Block(_) => false,
            A::Call(f, _) if is_macro(f, ctx) => false,
            A::Call(f, _) if has_bindings(f, ctx) => true,
            A::Call(_, args) => args.iter().any(|arg| has_bindings(arg, ctx)),
        }
    }
    fn desug_macro(ast: Ast, ctx: &mut Ctx) -> Result<Expr, (usize, String)> {
        fn desug_all(xs: Vec<Ast>, ctx: &mut Ctx) -> Result<Vec<Expr>, (usize, String)> {
            xs.into_iter().map(|x| desug_macro(x, ctx)).collect()
        }
        match ast.1 {
            A::Call(f, args) if has_bindings(&ast, ctx) => {
                let f = desug_macro(*f, ctx)?;
                let args = desug_all(args, ctx)?;
                let empty = Expr::String(Str::List as usize);
                let list = args.into_iter().fold(empty, |l, x| app(l, x));
                Ok(app(app(Expr::String(Str::Compound as usize), f), list))
            }
            A::Var(_) | A::Atom(_) | A::String(_) | A::Call(_, _) => {
                Ok(app(Expr::String(Str::Value as usize), desug_val(ast, ctx)?))
            }
            A::Binding(_, _, _) => {
                Ok(app(Expr::String(Str::Binding as usize), desug_val(ast, ctx)?))
            }
            A::Block(_) => desug_val(ast, ctx),
        }
    }
    fn desug_val<'c>(Ast(pos, ast): Ast, ctx: &mut Ctx) -> Result<Expr, (usize, String)> {
        match ast {
            A::Var(v) if v.ends_with("!") => {
                Ok(Expr::Effect(resolve_str(v[..v.len() - 1].to_string(), ctx)))
            }
            A::Var(v) => match resolve_var(&v, ctx) {
                Some(v) => Ok(Expr::Var(v)),
                None => match v.as_str() {
                    "=" => Ok(abs(abs(abs(app(Expr::Var(0), Expr::Var(1)))))),
                    "=>" => Ok(abs(abs(Expr::Var(0)))),
                    "~>" => Ok(abs(abs(Expr::Rec(Box::new(Expr::Var(0)))))),
                    "type" => Ok(abs(Expr::Type(Box::new(Expr::Var(0))))),
                    "__compare" => Ok(abs(abs(abs(abs(Expr::Compare(
                        [3, 2, 1, 0].map(|v| Expr::Var(v).into()),
                    )))))),
                    "__unpack" => {
                        Ok(abs(abs(abs(Expr::Unpack([2, 1, 0].map(|v| Expr::Var(v).into()))))))
                    }
                    "__handle" => Ok(abs(abs(Expr::Handle([1, 0].map(|v| Expr::Var(v).into()))))),
                    _ => Err((pos, v.to_string())),
                },
            },
            A::Atom(s) => Ok(Expr::String(resolve_str(s.to_string(), ctx))),
            A::String(s) => Ok(Expr::String(resolve_str(format!("\"{s}\""), ctx))),
            A::Binding(lvl, c, b) => {
                ctx.bindings.push((lvl, c, b.to_string()));
                Ok(Expr::String(resolve_str(format!("\"{b}\""), ctx)))
            }
            A::Block(mut items) => {
                let mut desugared = vec![];
                if items.is_empty() {
                    items.push(Ast(pos, A::Atom(NULL.to_string())));
                }
                for ast in items {
                    let bindings = ctx.bindings.len();
                    let drained = ctx.drain_bindings();
                    ctx.vars.extend(drained);
                    if bindings == 0 {
                        ctx.vars.push((BindType::Variable, String::new()))
                    }
                    desugared.push((bindings, desug_val(ast, ctx)?));
                }
                let (mut bindings, mut expr) = desugared.pop().unwrap();
                expr = (0..max(1, bindings)).fold(expr, |x, _| abs(x));
                for (prev_bindings, x) in desugared.into_iter().rev() {
                    let (f, arg) = if bindings == 0 { (expr, x) } else { (x, expr) };
                    expr = (0..max(1, prev_bindings)).fold(app(f, arg), |x, _| abs(x));
                    ctx.vars.truncate(ctx.vars.len() - max(1, bindings));
                    bindings = prev_bindings;
                }
                ctx.vars.truncate(ctx.vars.len() - max(1, bindings));
                ctx.clear_bindings();
                Ok(expr)
            }
            A::Call(f, args) => {
                let bindings = mem::replace(&mut ctx.bindings, vec![]);
                let is_macro = is_macro(&f, ctx);
                let mut f = desug_val(*f, ctx)?;
                if args.is_empty() {
                    f = app(f, Expr::String(Str::Null as usize));
                }
                for x in args {
                    f = app(f, if is_macro { desug_macro(x, ctx)? } else { desug_val(x, ctx)? })
                }
                ctx.bindings.splice(0..0, bindings);
                Ok(f)
            }
        }
    }
    match desug_val(Ast(0, A::Block(block)), ctx) {
        Err((i, v)) => Err(format!("Unbound variable '{v}' at {}", pos_at(i, code))),
        Ok(Expr::Abs(body)) => Ok(body.simplify(&mut vec![])),
        Ok(_) => unreachable!("Expected the main block to be desugared to an abstraction!"),
    }
}

impl Expr {
    fn is_pure(&self) -> bool {
        match self {
            Expr::Var(_) | Expr::String(_) => true,
            Expr::Effect(_) => false,
            Expr::Abs(expr) | Expr::Rec(expr) | Expr::Type(expr) => expr.is_pure(),
            Expr::App(f, arg) => f.is_pure() && arg.is_pure(),
            Expr::Unpack([v, t, f]) => v.is_pure() && t.is_pure() && f.is_pure(),
            Expr::Handle([v, h]) => v.is_pure() && h.is_pure(),
            Expr::Compare([a, b, t, f]) => a.is_pure() && b.is_pure() && t.is_pure() && f.is_pure(),
        }
    }

    fn shift(self, min: usize, by: isize) -> Self {
        match self {
            Expr::Var(v) if v >= min => Expr::Var((v as isize + by) as usize),
            Expr::Var(v) => Expr::Var(v),
            val @ (Expr::String(_) | Expr::Effect(_)) => val,
            Expr::Abs(body) => Expr::Abs(body.shift(min + 1, by).into()),
            Expr::Rec(body) => Expr::Rec(body.shift(min, by).into()),
            Expr::App(f, arg) => Expr::App(f.shift(min, by).into(), arg.shift(min, by).into()),
            Expr::Type(expr) => Expr::Type(expr.shift(min, by).into()),
            Expr::Unpack([v, t, f]) => Expr::Unpack([
                v.shift(min, by).into(),
                t.shift(min, by).into(),
                f.shift(min, by).into(),
            ]),
            Expr::Handle([v, h]) => {
                Expr::Handle([v.shift(min, by).into(), h.shift(min, by).into()])
            }
            Expr::Compare([a, b, t, f]) => Expr::Compare([
                a.shift(min, by).into(),
                b.shift(min, by).into(),
                t.shift(min, by).into(),
                f.shift(min, by).into(),
            ]),
        }
    }

    fn simplify(self, env: &mut Vec<Option<Expr>>) -> Self {
        match self {
            Expr::Var(v) if v < env.len() => env[env.len() - v - 1]
                .clone()
                .map(|expr| expr.shift(0, v as isize))
                .unwrap_or(Expr::Var(v)),
            val @ (Expr::Var(_) | Expr::String(_) | Expr::Effect(_)) => val,
            Expr::Abs(body) => {
                env.push(None);
                let expr = Expr::Abs(body.simplify(env).into());
                env.pop();
                expr
            }
            Expr::Rec(body) => Expr::Rec(body.simplify(env).into()),
            Expr::App(f, arg) => {
                let arg = arg.simplify(env);
                let f = f.simplify(env);
                if let Expr::Abs(body) = f {
                    if arg.is_pure() {
                        let arg = arg.shift(0, 1);
                        env.push(Some(arg));
                        let expr = body.simplify(env).shift(0, -1);
                        env.pop();
                        expr
                    } else {
                        Expr::App(Expr::Abs(body).into(), arg.into())
                    }
                } else {
                    Expr::App(f.into(), arg.into())
                }
            }
            Expr::Type(expr) => Expr::Type(expr.simplify(env).into()),
            Expr::Unpack([v, t, f]) => {
                let v = v.simplify(env);
                let t = t.simplify(env);
                let f = f.simplify(env);
                Expr::Unpack([v.into(), t.into(), f.into()])
            }
            Expr::Handle([val, handler]) => {
                let val = val.simplify(env);
                let handler = handler.simplify(env);
                Expr::Handle([val.into(), handler.into()])
            }
            Expr::Compare([a, b, t, f]) => {
                let a = a.simplify(env);
                let b = b.simplify(env);
                let t = t.simplify(env);
                let f = f.simplify(env);
                if a.is_pure() && b.is_pure() && t.is_pure() && f.is_pure() {
                    match (a, b, t, f) {
                        (a, b, Expr::Abs(t), _) if a == b => *t,
                        (Expr::String(_), Expr::String(_), _, Expr::Abs(f)) => *f,
                        (a, b, t, f) => Expr::Compare([a.into(), b.into(), t.into(), f.into()]),
                    }
                } else {
                    Expr::Compare([a.into(), b.into(), t.into(), f.into()])
                }
            }
        }
    }
}

fn emit(exprs: &[&Expr], ops: &mut Vec<Op>, fns: &mut Vec<Op>) {
    for expr in exprs.into_iter() {
        match expr {
            Expr::Var(v) => ops.push(Op::LoadVar(*v)),
            Expr::String(s) => ops.push(Op::LoadString(*s)),
            Expr::Effect(e) => ops.push(Op::LoadEffect(*e)),
            Expr::Abs(body) => {
                let mut f = vec![];
                emit(&[body], &mut f, fns);
                f.push(Op::Return);
                let fvars = f.iter().fold(0, |captured, op| match *op {
                    Op::LoadVar(v) if v > captured => v,
                    Op::LoadFn { fvars, .. } if fvars > captured => fvars - 1,
                    _ => captured,
                });
                let code = fns.len();
                ops.push(Op::LoadFn { code, fvars });
                fns.extend(f);
            }
            Expr::App(f, arg) => {
                emit(&[arg, f], ops, fns);
                ops.push(Op::Apply);
            }
            Expr::Rec(body) => {
                emit(&[body], ops, fns);
                ops.push(Op::Fix);
            }
            Expr::Type(body) => {
                emit(&[body], ops, fns);
                ops.push(Op::Type);
            }
            Expr::Compare([a, b, if_t, if_f]) => {
                emit(&[a, b, if_t, if_f], ops, fns);
                ops.extend([Op::Compare, Op::Apply]);
            }
            Expr::Unpack([val, if_t, if_f]) => {
                emit(&[val, if_t, if_f], ops, fns);
                ops.extend([Op::Unpack, Op::Apply, Op::Apply]);
            }
            Expr::Handle([val, handler]) => {
                emit(&[val, handler], ops, fns);
                ops.extend([Op::Try, Op::Apply, Op::Unwind, Op::Apply, Op::Apply, Op::Apply]);
            }
        }
    }
}

pub fn codegen(expr: Expr, ctx: Ctx) -> Bytecode {
    let mut main = vec![];
    let mut bytecode = vec![];
    emit(&[&expr], &mut main, &mut bytecode);
    main.push(Op::Return);
    let start = bytecode.len();
    bytecode.extend(main);
    Bytecode::new(ctx, bytecode, start)
}

pub fn compile(code: &str) -> Result<Bytecode, String> {
    let mut ctx = Ctx::default();
    let parsed = parse(include_str!("_prelude.kb"))?.into_iter().chain(parse(code)?);
    let expr = desugar(parsed.collect::<Vec<_>>(), &code, &mut ctx)?;
    Ok(codegen(expr, ctx))
}

impl Bytecode {
    pub fn load(&mut self, code: &str) -> Result<usize, String> {
        let parsed = parse(&code)?;
        let expr = desugar(parsed, &code, &mut self.ctx)?;
        let mut main = vec![];
        emit(&[&expr], &mut main, &mut self.ops);
        main.push(Op::Return);
        let start = self.ops.len();
        self.ops.extend(main);
        Ok(start)
    }
}
