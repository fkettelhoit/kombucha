use std::{
    cmp::{max, min},
    iter::{Peekable, once},
    mem, usize,
    vec::IntoIter,
};

use crate::bytecode::{Bytecode, Ctx, Op, Reflect};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tok<'code> {
    Ident(&'code str),
    Atom(&'code str),
    String(&'code str),
    Keyword(&'code str),
    Binding(&'code str),
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Separator,
}

fn scan(code: &str) -> Result<Vec<(Tok, usize, &str)>, usize> {
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
                    (':', _) => Tok::Binding(&code[i + 1..j]),
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
                None => return Err(i - 1),
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
    Binding(String),
    Block(Vec<Ast>),
    List(Vec<Ast>),
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
    struct E<'c>(usize, Option<(usize, &'c str)>, _E<'c>);
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
    type Toks<'c> = Peekable<IntoIter<(Tok<'c>, usize, &'c str)>>;
    fn _expr<'c>(toks: &mut Toks<'c>) -> Result<Ast, E<'c>> {
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
                Ok(expr) => keyword_args.push(Ast(i, A::List(vec![kw, _infix(toks, expr)?]))),
                Err(E(_, tok, _E::Value)) => return Err(E(i, tok, _E::KeywordArg(tok_s))),
                Err(e) => return Err(e),
            }
        }
        if let Some(Ast(i, _)) = keyword_args.first() {
            trailing.push(Ast(*i, A::List(keyword_args)));
        }
        match (expr, trailing.len()) {
            (expr, 0) => _infix(toks, expr),
            (Ast(pos, A::Call(f, args)), _) => {
                Ok(Ast(pos, A::Call(f, args.into_iter().chain(trailing).collect())))
            }
            (Ast(pos, value), _) => Ok(Ast(pos, A::Call(Box::new(Ast(pos, value)), trailing))),
        }
    }
    fn _infix<'c>(toks: &mut Toks<'c>, expr: Ast) -> Result<Ast, E<'c>> {
        let Some((Tok::Ident(f), i, _)) = toks.peek().copied() else {
            return Ok(expr);
        };
        let mut args = vec![expr];
        while let Some((Tok::Ident(s), j, _)) = toks.peek().copied() {
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
        Ok(Ast(i, A::Call(Box::new(Ast(i, A::Var(f.to_string()))), args)))
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
                Tok::Binding(s) => Ok(Ast(i, A::Binding(s.to_string()))),
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
                Tok::RParen | Tok::RBracket | Tok::RBrace | Tok::Separator | Tok::Keyword(_) => {
                    Err(E(i, Some((i, tok_s)), _E::Value))
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
                Some((_, j, s)) if needs_sep => return Err(E(i + 1, Some((j, s)), _E::Sep)),
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
    let toks = scan(code).map_err(|i| format!("Unclosed token starting at {}", pos_at(i, code)))?;
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
    Unpack([Box<Expr>; 3]),
    Handle([Box<Expr>; 3]),
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
        ctx.vars.iter().rev().position(|x| *x == v)
    }
    fn resolve_str<'c>(s: String, ctx: &mut Ctx) -> usize {
        ctx.strs.iter().position(|x| *x == s).unwrap_or_else(|| {
            ctx.strs.push(s);
            ctx.strs.len() - 1
        })
    }
    fn contains_bindings(Ast(_, ast): &Ast) -> bool {
        match ast {
            A::Binding(_) => true,
            A::Var(_) | A::Atom(_) | A::String(_) | A::Block(_) => false,
            A::Call(f, _) if contains_bindings(f) => true,
            A::List(items) | A::Call(_, items) => {
                let is_macro = items.iter().any(|Ast(_, arg)| matches!(arg, A::Block(_)));
                !is_macro && items.iter().any(|arg| contains_bindings(arg))
            }
        }
    }
    fn desug_reflect(ast: Ast, ctx: &mut Ctx) -> Result<Expr, (usize, String)> {
        fn desug_all(xs: Vec<Ast>, ctx: &mut Ctx) -> Result<Vec<Expr>, (usize, String)> {
            xs.into_iter().map(|x| desug_reflect(x, ctx)).collect()
        }
        match ast.1 {
            A::List(items) if contains_bindings(&ast) => {
                let items = desug_all(items.into_iter().rev().collect(), ctx)?;
                let nil = Expr::String(Reflect::Nil as usize);
                let list = items.into_iter().fold(nil, |l, x| app(l, x));
                Ok(app(Expr::String(Reflect::List as usize), list))
            }
            A::Call(f, args) if contains_bindings(&ast) => {
                let args = desug_all(args, ctx)?;
                let nil = Expr::String(Reflect::Nil as usize);
                let list = args.into_iter().rev().fold(nil, |l, x| app(l, x));
                let f = desug_reflect(*f, ctx)?;
                Ok(app(app(Expr::String(Reflect::Compound as usize), f), list))
            }
            A::Var(_) | A::Atom(_) | A::String(_) | A::List(_) | A::Call(_, _) => {
                Ok(app(Expr::String(Reflect::Value as usize), desug_val(ast, ctx)?))
            }
            A::Binding(_) => Ok(app(Expr::String(Reflect::Binding as usize), desug_val(ast, ctx)?)),
            A::Block(_) => desug_val(ast, ctx),
        }
    }
    fn desug_val<'c>(Ast(pos, ast): Ast, ctx: &mut Ctx) -> Result<Expr, (usize, String)> {
        match ast {
            A::Var(v) if v.ends_with("!") => {
                Ok(Expr::Effect(resolve_str(v[..v.len() - 1].to_string(), ctx)))
            }
            A::Var(v) => match (resolve_var(&v, ctx), v.as_str()) {
                (Some(v), _) => Ok(Expr::Var(v)),
                (_, "=") => Ok(abs(abs(abs(app(Expr::Var(0), Expr::Var(1)))))),
                (_, "=>") => Ok(abs(abs(Expr::Var(0)))),
                (_, "~>") => Ok(abs(abs(Expr::Rec(Box::new(Expr::Var(0)))))),
                (_, "__compare") => {
                    Ok(abs(abs(abs(abs(Expr::Compare([3, 2, 1, 0].map(|v| Expr::Var(v).into())))))))
                }
                (_, "__unpack") => {
                    Ok(abs(abs(abs(Expr::Unpack([2, 1, 0].map(|v| Expr::Var(v).into()))))))
                }
                (_, "__handle") => {
                    Ok(abs(abs(abs(Expr::Handle([2, 1, 0].map(|v| Expr::Var(v).into()))))))
                }
                _ => Err((pos, v.to_string())),
            },
            A::Atom(s) => Ok(Expr::String(resolve_str(s.to_string(), ctx))),
            A::String(s) => Ok(Expr::String(resolve_str(format!("\"{s}\""), ctx))),
            A::Binding(b) => {
                ctx.bindings.push(b.to_string());
                Ok(Expr::String(resolve_str(format!("\"{b}\""), ctx)))
            }
            A::Block(mut items) => {
                let mut desugared = vec![];
                if items.is_empty() {
                    items.push(Ast(pos, A::List(vec![])));
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
                expr = (0..max(1, bindings)).fold(expr, |x, _| abs(x));
                for (prev_bindings, x) in desugared.into_iter().rev() {
                    let (f, arg) = if bindings == 0 { (expr, x) } else { (x, expr) };
                    expr = (0..max(1, prev_bindings)).fold(app(f, arg), |x, _| abs(x));
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
            A::Call(f, args) => {
                let bindings = mem::replace(&mut ctx.bindings, vec![]);
                let mut f = desug_val(*f, ctx)?;
                if args.is_empty() {
                    f = app(f, Expr::String(Reflect::Nil as usize));
                }
                let is_builtin = matches!(f, Expr::Abs(_));
                let is_macro = args.iter().any(|Ast(_, arg)| matches!(arg, A::Block(_)));
                for arg in args {
                    if is_macro && !is_builtin {
                        f = app(f, desug_reflect(arg, ctx)?);
                    } else {
                        f = app(f, desug_val(arg, ctx)?);
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

fn params(expr: &Expr) -> usize {
    match expr {
        Expr::String(_) => usize::MAX,
        Expr::Var(_) | Expr::Effect(_) => 1,
        Expr::Abs(body) => params(body).saturating_add(1),
        Expr::Rec(f) => params(f).saturating_sub(1),
        Expr::App(_, arg) if params(arg) == 0 => 0,
        Expr::App(f, _) => params(f).saturating_sub(1),
        Expr::Unpack([val, _, _]) if params(val) == 0 => 0,
        Expr::Handle([_, eff, _]) if params(eff) == 0 => 0,
        Expr::Handle([val, _, handler]) => min(params(val), params(handler).saturating_sub(2)),
        Expr::Compare([a, b, _, _]) if params(a) == 0 || params(b) == 0 => 0,
        Expr::Unpack([_, if_t, if_f]) | Expr::Compare([_, _, if_t, if_f]) => {
            min(params(if_t).saturating_sub(2), params(if_f).saturating_sub(1))
        }
    }
}

fn emit(args: isize, expr: Expr, ops: &mut Vec<Op>, fns: &mut Vec<Op>) {
    match expr {
        Expr::Var(v) => ops.push(Op::LoadVar(v)),
        Expr::String(s) => ops.push(Op::LoadString(s)),
        Expr::Effect(e) => ops.push(Op::LoadEffect(e)),
        Expr::Abs(body) => {
            let mut f = vec![];
            emit(args, *body, &mut f, fns);
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
                emit(0, *arg, ops, fns);
                emit(args + 1, *f, ops, fns);
                ops.push(Op::ApplyArgToFn);
            } else {
                emit(args + 1, *f, ops, fns);
                emit(0, *arg, ops, fns);
                ops.push(Op::ApplyFnToArg);
            }
        }
        Expr::Rec(body) => {
            emit(args + 1, *body, ops, fns);
            ops.push(Op::LoadFn { code: 0, fvars: 0 }); // the built-in fixed-point combinator
            ops.push(Op::ApplyArgToFn);
        }
        Expr::Compare([a, b, if_t, if_f]) => {
            emit(0, *a, ops, fns);
            emit(0, *b, ops, fns);
            emit(args + 1, *if_t, ops, fns);
            emit(args + 1, *if_f, ops, fns);
            ops.push(Op::Cmp);
            ops.push(Op::ApplyArgToFn);
        }
        Expr::Unpack([val, if_t, if_f]) => {
            emit(0, *val, ops, fns);
            emit(args + 2, *if_t, ops, fns);
            emit(args + 1, *if_f, ops, fns);
            ops.push(Op::Unpack);
            ops.push(Op::ApplyArgToFn);
            ops.push(Op::ApplyArgToFn);
        }
        Expr::Handle([val, eff, handler]) => {
            emit(args + 1, *val, ops, fns);
            emit(0, *eff, ops, fns);
            emit(0, *handler, ops, fns);
            ops.push(Op::Try);
            ops.push(Op::ApplyArgToFn);
            ops.push(Op::Unwind);
            ops.push(Op::ApplyArgToFn);
            ops.push(Op::ApplyArgToFn);
        }
    }
}

pub fn codegen(expr: Expr, ctx: Ctx) -> Bytecode {
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
    emit(0, expr, &mut main, &mut bytecode);
    main.push(Op::Return);
    let start = bytecode.len();
    bytecode.extend(main);
    Bytecode::new(ctx, bytecode, start)
}

pub fn compile(code: &str) -> Result<Bytecode, String> {
    let code = include_str!("_prelude.vo").to_string() + "\n" + code;
    let parsed = parse(&code)?;
    let mut ctx = Ctx::default();
    let expr = desugar(parsed, &code, &mut ctx)?;
    Ok(codegen(expr, ctx))
}

impl Bytecode {
    pub fn load(&mut self, code: &str) -> Result<usize, String> {
        let parsed = parse(&code)?;
        let expr = desugar(parsed, &code, &mut self.ctx)?;
        let mut main = vec![];
        emit(0, expr, &mut main, &mut self.ops);
        main.push(Op::Return);
        let start = self.ops.len();
        self.ops.extend(main);
        Ok(start)
    }
}
