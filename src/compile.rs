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

fn scan(code: &str) -> Result<Vec<(Tok, usize, &str)>, (usize, &str)> {
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

#[derive(Debug, Clone)]
pub enum Expr {
    Var(usize),
    String(usize),
    Effect(usize),
    Abs(usize, Box<Expr>),
    Rec(Box<Expr>),
    App(Box<Expr>, Vec<Expr>),
    Seq(Box<Expr>, Box<Expr>),
    Type(Box<Expr>),
    Unpack([Box<Expr>; 3]),
    Handle([Box<Expr>; 2]),
    Compare([Box<Expr>; 4]),
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
                let l = Expr::App(Expr::String(Str::List as usize).into(), desug_all(args, ctx)?);
                Ok(Expr::App(Expr::String(Str::Compound as usize).into(), vec![f, l]))
            }
            A::Var(_) | A::Atom(_) | A::String(_) | A::Call(_, _) => {
                Ok(Expr::App(Expr::String(Str::Value as usize).into(), vec![desug_val(ast, ctx)?]))
            }
            A::Binding(_, _, _) => Ok(Expr::App(
                Expr::String(Str::Binding as usize).into(),
                vec![desug_val(ast, ctx)?],
            )),
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
                    "=" => {
                        Ok(Expr::Abs(3, Expr::App(Expr::Var(0).into(), vec![Expr::Var(1)]).into()))
                    }
                    "=>" => Ok(Expr::Abs(2, Expr::Var(0).into())),
                    "~>" => Ok(Expr::Abs(2, Expr::Rec(Expr::Var(0).into()).into())),
                    "type" => Ok(Expr::Abs(1, Expr::Type(Box::new(Expr::Var(0))).into())),
                    "__compare" => Ok(Expr::Abs(
                        4,
                        Expr::Compare([3, 2, 1, 0].map(|v| Expr::Var(v).into())).into(),
                    )),
                    "__unpack" => Ok(Expr::Abs(
                        3,
                        Expr::Unpack([2, 1, 0].map(|v| Expr::Var(v).into())).into(),
                    )),
                    "__handle" => {
                        Ok(Expr::Abs(2, Expr::Handle([1, 0].map(|v| Expr::Var(v).into())).into()))
                    }
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
                    desugared.push((bindings, desug_val(ast, ctx)?));
                }
                let (mut bindings, mut expr) = desugared.pop().unwrap();
                expr = Expr::Abs(bindings, expr.into());
                for (prev_bindings, x) in desugared.into_iter().rev() {
                    if bindings == 0 {
                        expr = Expr::Seq(x.into(), expr.into())
                    } else {
                        match x {
                            Expr::App(f, mut args) => {
                                args.push(expr);
                                expr = Expr::App(f, args);
                            }
                            _ => expr = Expr::App(x.into(), vec![expr]),
                        }
                    }
                    expr = Expr::Abs(prev_bindings, expr.into());
                    ctx.vars.truncate(ctx.vars.len() - bindings);
                    bindings = prev_bindings;
                }
                ctx.vars.truncate(ctx.vars.len() - bindings);
                ctx.clear_bindings();
                Ok(expr)
            }
            A::Call(f, args) => {
                let bindings = mem::replace(&mut ctx.bindings, vec![]);
                let is_macro = is_macro(&f, ctx);
                let f = desug_val(*f, ctx)?;
                let mut desug_args = vec![];
                for x in args {
                    desug_args.push(if is_macro {
                        desug_macro(x, ctx)?
                    } else {
                        desug_val(x, ctx)?
                    });
                }
                ctx.bindings.splice(0..0, bindings);
                Ok(Expr::App(f.into(), desug_args))
            }
        }
    }
    match desug_val(Ast(0, A::Block(block)), ctx) {
        Err((i, v)) => Err(format!("Unbound variable '{v}' at {}", pos_at(i, code))),
        Ok(Expr::Abs(0, body)) => Ok(*body),
        Ok(_) => unreachable!("Expected the main block to be desugared to a zero arg abstraction!"),
    }
}

fn emit(exprs: &[&Expr], ops: &mut Vec<Op>, fns: &mut Vec<Op>) {
    for expr in exprs.into_iter() {
        match expr {
            Expr::Var(v) => ops.push(Op::LoadVar(*v)),
            Expr::String(s) => ops.push(Op::LoadString(*s)),
            Expr::Effect(e) => ops.push(Op::LoadEffect(*e)),
            Expr::Abs(params, body) => {
                let mut f = vec![];
                emit(&[body], &mut f, fns);
                f.push(Op::Return);
                let fvars = f.iter().fold(0, |captured, op| match *op {
                    Op::LoadVar(v) if v >= captured + params => v - params,
                    Op::LoadFn { fvars, .. } if fvars > captured => fvars - params,
                    _ => captured,
                });
                let code = fns.len();
                ops.push(Op::LoadFn { code, params: *params, fvars });
                fns.extend(f);
            }
            Expr::App(f, args) => {
                emit(&args.iter().collect::<Vec<_>>(), ops, fns);
                emit(&[f], ops, fns);
                ops.push(Op::App(args.len()));
            }
            Expr::Rec(body) => {
                emit(&[body], ops, fns);
                ops.extend([Op::Fix, Op::App(1)]);
            }
            Expr::Seq(a, b) => emit(&[a, b], ops, fns),
            Expr::Type(body) => {
                emit(&[body], ops, fns);
                ops.push(Op::Type);
            }
            Expr::Compare([a, b, if_t, if_f]) => {
                emit(&[a, b, if_t, if_f], ops, fns);
                ops.extend([Op::Compare, Op::App(0)]);
            }
            Expr::Unpack([val, if_t, if_f]) => {
                let mut fn_t = vec![];
                emit(&[if_t], &mut fn_t, fns);
                fn_t.extend([Op::App(2), Op::Return]);
                let code_t = fns.len();
                fns.extend(fn_t);

                let mut fn_f = vec![];
                emit(&[if_f], &mut fn_f, fns);
                fn_f.extend([Op::App(0), Op::Return]);
                let code_f = fns.len();
                fns.extend(fn_f);

                emit(&[val], ops, fns);
                ops.push(Op::Unpack { if_true: code_t, if_false: code_f });
            }
            Expr::Handle([val, handler]) => {
                emit(&[val, handler], ops, fns);
                ops.extend([Op::Try, Op::App(0), Op::Unwind, Op::App(3)]);
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
