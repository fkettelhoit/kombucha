use std::{
    cmp::{max, min},
    iter::{Peekable, once},
    mem, usize,
    vec::IntoIter,
};

use crate::bytecode::{BINDING, Bytecode, COMPOUND, LIST, NIL, Op, Reflect, VALUE};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tok {
    Symbol,
    String,
    Keyword,
    Binding,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Separator,
}

fn scan(code: &str) -> Vec<(Tok, usize, &str)> {
    let mut toks = vec![];
    let mut i = 0;
    let mut chars = code.char_indices().chain(once((code.len(), ' ')));
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
                    (n, _) if n.is_ascii_uppercase() => Tok::String,
                    (':', _) => Tok::Binding,
                    (_, ':') => Tok::Keyword,
                    _ => Tok::Symbol,
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
            let j = chars.by_ref().skip(n).map(|(j, _)| j).find(|j| code[*j..].starts_with(&end));
            let end = j.map(|j| j + 1).unwrap_or(code.len());
            toks.push((Tok::String, i - 1, &code[i + n - 1..end]));
            i = end + n;
        }
    }
    toks
}

#[derive(Debug, Clone)]
pub struct Ast<'code>(pub usize, pub A<'code>);

#[derive(Debug, Clone)]
pub enum A<'code> {
    Var(&'code str),
    String(&'code str),
    Binding(&'code str),
    Block(Vec<Ast<'code>>),
    List(Vec<Ast<'code>>),
    Call(Call<'code>, Vec<Ast<'code>>),
}

#[derive(Debug, Clone)]
pub enum Call<'code> {
    Infix(&'code str),
    Prefix(Box<Ast<'code>>),
    Keyword(Vec<&'code str>),
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

pub fn parse(code: &str) -> Result<Vec<Ast<'_>>, String> {
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
            keywords.push(&s[..s.len() - 1]);
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
                Tok::LBracket => Ok(Ast(i, A::List(_exprs(toks, i, Some(Tok::RBracket))?))),
                Tok::LBrace => Ok(Ast(i, A::Block(_exprs(toks, i, Some(Tok::RBrace))?))),
                Tok::RParen | Tok::RBracket | Tok::RBrace | Tok::Separator | Tok::Keyword => {
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
                Some((_, j, s)) if needs_sep => return Err(E(i + 1, Some((j, s)), _E::Sep)),
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
        Ok(exprs) => Ok(exprs),
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

#[derive(Debug, Clone)]
pub enum Expr {
    Var(usize),
    String(usize),
    Effect(usize),
    Abs(Box<Expr>),
    Rec(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Unpack(Box<Expr>, Box<Expr>, Box<Expr>),
    Handle(Box<Expr>, Box<Expr>, Box<Expr>),
    Cmp(Box<Expr>, Box<Expr>, Box<Expr>, Box<Expr>),
}

pub fn abs(body: Expr) -> Expr {
    Expr::Abs(Box::new(body))
}

pub fn app(f: Expr, arg: Expr) -> Expr {
    Expr::App(Box::new(f), Box::new(arg))
}

pub struct Ctx {
    pub bindings: Vec<String>,
    pub vars: Vec<String>,
    pub strs: Vec<String>,
}

impl Ctx {
    pub fn new() -> Self {
        let mut ctx = Ctx { bindings: vec![], vars: vec![], strs: vec![String::new(); 5] };
        ctx.strs[Reflect::Nil as usize] = NIL.to_string();
        ctx.strs[Reflect::Value as usize] = VALUE.to_string();
        ctx.strs[Reflect::Binding as usize] = BINDING.to_string();
        ctx.strs[Reflect::Compound as usize] = COMPOUND.to_string();
        ctx.strs[Reflect::List as usize] = LIST.to_string();
        ctx
    }
}

pub fn desugar<'c>(block: Vec<Ast<'c>>, code: &'c str, ctx: &mut Ctx) -> Result<Expr, String> {
    fn resolve_var(v: &str, ctx: &Ctx) -> Option<usize> {
        ctx.vars.iter().rev().position(|x| *x == v)
    }
    fn resolve_str<'c>(s: String, ctx: &mut Ctx) -> usize {
        ctx.strs.iter().position(|x| *x == s).unwrap_or_else(|| {
            ctx.strs.push(s);
            ctx.strs.len() - 1
        })
    }
    fn contains_bindings(Ast(_, ast): &Ast<'_>) -> bool {
        match ast {
            A::Binding(_) => true,
            A::Var(_) | A::String(_) | A::Block(_) => false,
            A::List(items) => {
                let is_macro = items.iter().any(|Ast(_, arg)| matches!(arg, A::Block(_)));
                !is_macro && items.iter().any(|arg| contains_bindings(arg))
            }
            A::Call(call, args) => match call {
                Call::Prefix(f) if contains_bindings(f) => true,
                _ => {
                    let is_macro = args.iter().any(|Ast(_, arg)| matches!(arg, A::Block(_)));
                    !is_macro && args.iter().any(|arg| contains_bindings(arg))
                }
            },
        }
    }
    fn desug_reflect(ast: Ast<'_>, ctx: &mut Ctx) -> Result<Expr, (usize, String)> {
        let has_bindings = contains_bindings(&ast);
        match ast.1 {
            A::List(items) if has_bindings => {
                let mut desugared_items = vec![];
                for item in items {
                    desugared_items.push(desug_reflect(item, ctx)?);
                }
                let mut f = Expr::String(Reflect::Nil as usize);
                for item in desugared_items {
                    f = app(f, item)
                }
                Ok(app(Expr::String(Reflect::List as usize), f))
            }
            A::Call(call, args) if has_bindings => {
                let mut desugared_args = vec![];
                for arg in args {
                    desugared_args.push(desug_reflect(arg, ctx)?);
                }
                let f = match call {
                    Call::Infix(f) => desug_reflect(Ast(ast.0, A::Var(f)), ctx)?,
                    Call::Prefix(f) => desug_reflect(*f, ctx)?,
                    Call::Keyword(keywords) => match resolve_var(&keywords.join("-"), ctx) {
                        Some(v) => app(Expr::String(Reflect::Value as usize), Expr::Var(v)),
                        None => return Err((ast.0, keywords.join("-"))),
                    },
                };
                let mut list = Expr::String(Reflect::Nil as usize);
                for item in desugared_args.into_iter().rev() {
                    list = app(list, item)
                }
                Ok(app(app(Expr::String(Reflect::Compound as usize), f), list))
            }
            A::Var(_) | A::String(_) | A::List(_) | A::Call(_, _) => {
                Ok(app(Expr::String(Reflect::Value as usize), desug_val(ast, ctx)?))
            }
            A::Binding(_) => Ok(app(Expr::String(Reflect::Binding as usize), desug_val(ast, ctx)?)),
            A::Block(_) => desug_val(ast, ctx),
        }
    }
    fn desug_val<'c>(Ast(pos, ast): Ast<'c>, ctx: &mut Ctx) -> Result<Expr, (usize, String)> {
        match ast {
            A::Var(v) if v.ends_with("!") => {
                Ok(Expr::Effect(resolve_str(v[..v.len() - 1].to_string(), ctx)))
            }
            A::Var(v) => match resolve_var(v, ctx) {
                Some(v) => Ok(Expr::Var(v)),
                None if v == "=" => Ok(abs(abs(abs(app(Expr::Var(0), Expr::Var(1)))))),
                None if v == "=>" => Ok(abs(abs(Expr::Var(0)))),
                None if v == "~>" => Ok(abs(abs(Expr::Rec(Box::new(Expr::Var(0)))))),
                None if v == "__compare" => Ok(abs(abs(abs(abs(Expr::Cmp(
                    Box::new(Expr::Var(3)),
                    Box::new(Expr::Var(2)),
                    Box::new(Expr::Var(1)),
                    Box::new(Expr::Var(0)),
                )))))),
                None if v == "__unpack" => Ok(abs(abs(abs(Expr::Unpack(
                    Box::new(Expr::Var(2)),
                    Box::new(Expr::Var(1)),
                    Box::new(Expr::Var(0)),
                ))))),
                None if v == "__handle" => Ok(abs(abs(abs(Expr::Handle(
                    Box::new(Expr::Var(2)),
                    Box::new(Expr::Var(1)),
                    Box::new(Expr::Var(0)),
                ))))),
                None => Err((pos, v.to_string())),
            },
            A::String(s) => Ok(Expr::String(resolve_str(s.to_string(), ctx))),
            A::Binding(b) => {
                ctx.bindings.push(b[1..].to_string());
                Ok(Expr::String(resolve_str(format!("\"{}\"", &b[1..]), ctx)))
            }
            A::Block(mut items) => {
                let mut desugared = vec![];
                if items.is_empty() {
                    items.push(Ast(pos, A::String(NIL)));
                }
                for ast in items {
                    let bindings = ctx.bindings.len();
                    ctx.vars.extend(ctx.bindings.drain(..));
                    if bindings == 0 {
                        ctx.vars.push(String::new())
                    }
                    desugared.push((bindings, desug_val(ast, ctx)?));
                }
                let (mut bindings, mut expr) = desugared.pop().unwrap();
                expr = (0..max(1, bindings)).fold(expr, |x, _| Expr::Abs(Box::new(x)));
                for (prev_bindings, x) in desugared.into_iter().rev() {
                    let (f, arg) = if bindings == 0 { (expr, x) } else { (x, expr) };
                    let app = Expr::App(Box::new(f), Box::new(arg));
                    expr = (0..max(1, prev_bindings)).fold(app, |x, _| Expr::Abs(Box::new(x)));
                    ctx.vars.truncate(ctx.vars.len() - max(1, bindings));
                    bindings = prev_bindings;
                }
                ctx.vars.truncate(ctx.vars.len() - max(1, bindings));
                ctx.bindings.clear();
                Ok(expr)
            }
            A::List(items) => {
                let mut list = Expr::String(Reflect::Nil as usize);
                let mut desugared = vec![];
                for item in items {
                    desugared.push(desug_val(item, ctx)?);
                }
                for item in desugared.into_iter().rev() {
                    list = app(list, item)
                }
                Ok(list)
            }
            A::Call(call, args) => {
                let bindings = mem::replace(&mut ctx.bindings, vec![]);
                let mut f = match call {
                    Call::Infix(f) => desug_val(Ast(pos, A::Var(f)), ctx)?,
                    Call::Prefix(f) => desug_val(*f, ctx)?,
                    Call::Keyword(keywords) => match resolve_var(&keywords.join("-"), ctx) {
                        Some(v) => Expr::Var(v),
                        None => return Err((pos, keywords.join("-"))),
                    },
                };
                if args.is_empty() {
                    f = Expr::App(Box::new(f), Box::new(Expr::String(Reflect::Nil as usize)))
                }
                let is_builtin = matches!(f, Expr::Abs(_));
                let is_macro = args.iter().any(|Ast(_, arg)| matches!(arg, A::Block(_)));
                for arg in args {
                    if is_macro && !is_builtin {
                        f = Expr::App(Box::new(f), Box::new(desug_reflect(arg, ctx)?));
                    } else {
                        f = Expr::App(Box::new(f), Box::new(desug_val(arg, ctx)?));
                    }
                }
                ctx.bindings.splice(0..0, bindings);
                Ok(f)
            }
        }
    }
    match desug_val(Ast(0, A::Block(block)), ctx) {
        Err((i, v)) => Err(format!("Unbound variable '{v}' at {}", pos_at(i, code))),
        Ok(Expr::Abs(body)) => Ok(*body),
        Ok(_) => unreachable!("Expected the main block to be desugared to an abstraction!"),
    }
}

pub fn compile_expr(expr: Expr, strings: Vec<String>) -> Bytecode {
    fn params(expr: &Expr) -> usize {
        match expr {
            Expr::String(_) => usize::MAX,
            Expr::Var(_) | Expr::Effect(_) => 1,
            Expr::Abs(body) => params(body).saturating_add(1),
            Expr::Rec(f) => params(f).saturating_sub(1),
            Expr::App(_, arg) if params(arg) == 0 => 0,
            Expr::App(f, _) => params(f).saturating_sub(1),
            Expr::Unpack(val, _, _) if params(val) == 0 => 0,
            Expr::Handle(_, eff, _) if params(eff) == 0 => 0,
            Expr::Handle(val, _, handler) => min(params(val), params(handler).saturating_sub(2)),
            Expr::Cmp(a, b, _, _) if params(a) == 0 || params(b) == 0 => 0,
            Expr::Unpack(_, if_t, if_f) | Expr::Cmp(_, _, if_t, if_f) => {
                min(params(if_t).saturating_sub(2), params(if_f).saturating_sub(1))
            }
        }
    }
    fn compile(args: isize, expr: Expr, ops: &mut Vec<Op>, fns: &mut Vec<Op>) {
        match expr {
            Expr::Var(v) => ops.push(Op::LoadVar(v)),
            Expr::String(s) => ops.push(Op::LoadString(s)),
            Expr::Effect(e) => ops.push(Op::LoadEffect(e)),
            Expr::Abs(body) => {
                let mut f = vec![];
                compile(args, *body, &mut f, fns);
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
                if params(&f) > 0 {
                    compile(0, *arg, ops, fns);
                    compile(args + 1, *f, ops, fns);
                    ops.push(Op::ApplyArgToFn);
                } else {
                    compile(args + 1, *f, ops, fns);
                    compile(0, *arg, ops, fns);
                    ops.push(Op::ApplyFnToArg);
                }
            }
            Expr::Rec(body) => {
                compile(args + 1, *body, ops, fns);
                ops.push(Op::LoadFn { code: 0, fvars: 0 }); // the built-in fixed-point combinator
                ops.push(Op::ApplyArgToFn);
            }
            Expr::Cmp(a, b, if_t, if_f) => {
                compile(0, *a, ops, fns);
                compile(0, *b, ops, fns);
                compile(args + 1, *if_t, ops, fns);
                compile(args + 1, *if_f, ops, fns);
                ops.push(Op::Cmp);
                ops.push(Op::ApplyArgToFn);
            }
            Expr::Unpack(val, if_t, if_f) => {
                compile(0, *val, ops, fns);
                compile(args + 2, *if_t, ops, fns);
                compile(args + 1, *if_f, ops, fns);
                ops.push(Op::Unpack);
                ops.push(Op::ApplyArgToFn);
                ops.push(Op::ApplyArgToFn);
            }
            Expr::Handle(val, eff, handler) => {
                compile(args + 1, *val, ops, fns);
                compile(0, *eff, ops, fns);
                compile(0, *handler, ops, fns);
                ops.push(Op::Try);
                ops.push(Op::ApplyArgToFn);
                ops.push(Op::Unwind);
                ops.push(Op::ApplyArgToFn);
                ops.push(Op::ApplyArgToFn);
            }
        }
    }

    let mut main = vec![];

    // fix = f => x => f(fix(f))(x)
    let mut bytecode = vec![
        // 0: f => ...
        Op::LoadFn { code: 2, fvars: 1 },
        Op::Return,
        // 2: ... x => f(fix(f))(x)
        Op::LoadVar(0),                   // x
        Op::LoadVar(1),                   // x, f
        Op::LoadFn { code: 0, fvars: 0 }, // x, f, fix
        Op::ApplyArgToFn,                 // x, fix(f)
        Op::LoadVar(1),                   // x, fix(f), f
        Op::ApplyArgToFn,                 // x, f(fix(f))
        Op::ApplyArgToFn,                 // f(fix(f))(x)
        Op::Return,
    ];
    compile(0, expr, &mut main, &mut bytecode);
    main.push(Op::Return);
    let start = bytecode.len();
    bytecode.extend(main);
    Bytecode::new(strings, bytecode, start)
}

pub fn compile(code: &str) -> Result<Bytecode, String> {
    let code = include_str!("_prelude.vo").to_string() + "\n" + code;
    let parsed = parse(&code)?;
    let mut ctx = Ctx::new();
    let expr = desugar(parsed, &code, &mut ctx)?;
    Ok(compile_expr(expr, ctx.strs))
}
