use std::{
    cmp::max,
    iter::{Peekable, once},
    mem,
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
struct Prg<'code>(Vec<Ast<'code>>, &'code str);

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

fn parse(code: &str) -> Result<Prg<'_>, String> {
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

#[derive(Debug, Clone)]
enum Expr {
    Var(usize),
    String(usize),
    Effect(usize),
    Abs(Box<Expr>),
    Rec(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Pop(Box<Expr>, Box<Expr>, Box<Expr>),
    Try(Box<Expr>, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>, Box<Expr>),
}

fn abs(body: Expr) -> Expr {
    Expr::Abs(Box::new(body))
}

fn app(f: Expr, arg: Expr) -> Expr {
    Expr::App(Box::new(f), Box::new(arg))
}

fn desugar<'c>(Prg(block, code): Prg<'c>) -> Result<(Expr, Vec<&'c str>), String> {
    enum E {
        UnboundVar(String),
        EmptyBlock,
    }
    #[derive(Default)]
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
    fn desugar<'c>(Ast(pos, ast): Ast<'c>, ctx: &mut Ctx<'c>) -> Result<Expr, (usize, E)> {
        match ast {
            A::Var(v) if v.ends_with("!") => Ok(Expr::Effect(resolve_str(&v[..v.len() - 1], ctx))),
            A::Var(v) => match resolve_var(v, ctx) {
                Some(v) => Ok(Expr::Var(v)),
                None if v == "=" => Ok(abs(abs(abs(app(Expr::Var(0), Expr::Var(1)))))),
                None if v == "=>" => Ok(abs(abs(Expr::Var(0)))),
                None if v == "~>" => Ok(abs(abs(Expr::Rec(Box::new(Expr::Var(0)))))),
                None if v == "pop" => Ok(abs(abs(abs(Expr::Pop(
                    Box::new(Expr::Var(2)),
                    Box::new(Expr::Var(1)),
                    Box::new(Expr::Var(0)),
                ))))),
                None if v == "try" => Ok(abs(abs(abs(Expr::Try(
                    Box::new(Expr::Var(2)),
                    Box::new(Expr::Var(1)),
                    Box::new(Expr::Var(0)),
                ))))),
                None if v == "if" => Ok(abs(abs(abs(abs(Expr::If(
                    Box::new(Expr::Var(3)),
                    Box::new(Expr::Var(2)),
                    Box::new(Expr::Var(1)),
                    Box::new(Expr::Var(0)),
                )))))),
                None => Err((pos, E::UnboundVar(v.to_string()))),
            },
            A::String(s) => Ok(Expr::String(resolve_str(s, ctx))),
            A::Binding(b) => {
                ctx.bindings.push(&b[1..]);
                Ok(Expr::String(resolve_str(&b[1..], ctx)))
            }
            A::Block(items) => {
                let mut desugared = vec![];
                for ast in items {
                    let bindings = ctx.bindings.len();
                    ctx.vars.extend(ctx.bindings.drain(..));
                    if bindings == 0 {
                        ctx.vars.push("")
                    }
                    desugared.push((bindings, desugar(ast, ctx)?));
                }
                let (mut bindings, mut expr) = desugared.pop().ok_or((pos, E::EmptyBlock))?;
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
            A::Call(call, args) => {
                let bindings = mem::replace(&mut ctx.bindings, vec![]);
                let mut f = match call {
                    Call::Infix(f) => desugar(Ast(pos, A::Var(f)), ctx)?,
                    Call::Prefix(f) => desugar(*f, ctx)?,
                    Call::Keyword(keywords) => match resolve_var(&keywords.join(""), ctx) {
                        Some(v) => Expr::Var(v),
                        None => return Err((pos, E::UnboundVar(keywords.join("")))),
                    },
                };
                if args.is_empty() {
                    f = Expr::App(Box::new(f), Box::new(Expr::String(0)))
                }
                for arg in args {
                    f = Expr::App(Box::new(f), Box::new(desugar(arg, ctx)?));
                }

                ctx.bindings.splice(0..0, bindings);
                Ok(f)
            }
        }
    }
    let mut ctx = Ctx::default();
    ctx.strs.push("Nil");
    match desugar(Ast(0, A::Block(block)), &mut ctx) {
        Err((i, E::UnboundVar(v))) => Err(format!("Unbound variable '{v}' at {}", pos_at(i, code))),
        Err((i, E::EmptyBlock)) => Err(format!("Empty block at {}", pos_at(i, code))),
        Ok(Expr::Abs(body)) => Ok((*body, ctx.strs)),
        Ok(_) => unreachable!("Expected the main block to be desugared to an abstraction!"),
    }
}

#[derive(Debug, Clone, Copy)]
enum Op {
    PushVar(usize),
    PushString(usize),
    PushEffect(usize),
    PushFn(usize, usize),
    Rec,
    Apply,
    Return,
    Unwind,
    Pop,
    Try,
    If,
}

fn compile_expr(expr: Expr, ops: &mut Vec<Op>, fns: &mut Vec<Op>) -> usize {
    match expr {
        Expr::Var(v) => {
            ops.push(Op::PushVar(v));
            v
        }
        Expr::String(s) => {
            ops.push(Op::PushString(s));
            0
        }
        Expr::Effect(e) => {
            ops.push(Op::PushEffect(e));
            0
        }
        Expr::Abs(body) => {
            let mut f = vec![];
            let captured = compile_expr(*body, &mut f, fns);
            f.push(Op::Return);
            ops.push(Op::PushFn(fns.len(), captured));
            fns.extend(f);
            if captured == 0 { 0 } else { captured - 1 }
        }
        Expr::Rec(body) => {
            let f = compile_expr(*body, ops, fns);
            ops.push(Op::Rec);
            f
        }
        Expr::App(f, arg) => {
            let a = compile_expr(*arg, ops, fns);
            let b = compile_expr(*f, ops, fns);
            ops.push(Op::Apply);
            max(a, b)
        }
        Expr::Pop(v, t, f) => {
            let f = compile_expr(*f, ops, fns);
            let t = compile_expr(*t, ops, fns);
            let v = compile_expr(*v, ops, fns);
            ops.push(Op::Pop);
            ops.push(Op::Apply);
            ops.push(Op::Apply);
            v.max(t).max(f)
        }
        Expr::Try(v, e, h) => {
            let h = compile_expr(*h, ops, fns);
            let e = compile_expr(*e, ops, fns);
            let v = compile_expr(*v, ops, fns);
            ops.push(Op::Try);
            ops.push(Op::Apply);
            ops.push(Op::Unwind);
            v.max(e).max(h)
        }
        Expr::If(a, b, t, f) => {
            let f = compile_expr(*f, ops, fns);
            let t = compile_expr(*t, ops, fns);
            let b = compile_expr(*b, ops, fns);
            let a = compile_expr(*a, ops, fns);
            ops.push(Op::If);
            ops.push(Op::Apply);
            a.max(b).max(t).max(f)
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Vm<'code> {
    strings: Vec<&'code str>,
    bytecode: Vec<Op>,
    ip: usize,
    vars: Vec<V>,
    temps: Vec<V>,
    frames: Vec<(usize, usize)>,
    handlers: Vec<Handler>,
}

#[derive(Debug, Clone, Default)]
pub struct Coroutine {
    vars: Vec<V>,
    temps: Vec<V>,
    frames: Vec<(usize, usize)>,
    handlers: Vec<Handler>,
}

#[derive(Debug, Clone)]
struct Handler {
    effect: usize,
    code: usize,
    state: (usize, usize, usize, usize),
    captured: Rc<Vec<V>>,
    ret: usize,
}

impl<'code> Vm<'code> {
    fn new(strings: Vec<&'code str>, bytecode: Vec<Op>, start: usize) -> Self {
        Vm {
            strings,
            bytecode,
            ip: start,
            ..Default::default()
        }
    }
}

fn compile_prg<'code>(expr: Expr, strings: Vec<&'code str>) -> Vm<'code> {
    let mut main = vec![];
    let mut bytecode = vec![];
    compile_expr(expr, &mut main, &mut bytecode);
    let start = bytecode.len();
    bytecode.extend(main);
    Vm::new(strings, bytecode, start)
}

pub fn compile<'code>(code: &'code str) -> Result<Vm<'code>, String> {
    let parsed = parse(code)?;
    let (expr, strings) = desugar(parsed)?;
    Ok(compile_prg(expr, strings.clone()))
}

#[derive(Debug, Clone)]
pub enum V {
    Fn(usize, usize, usize),
    String(usize),
    Effect(usize),
    Record(usize, Vec<Rc<V>>),
    Closure(usize, Rc<Vec<V>>),
    Recursive(usize, Rc<Vec<V>>),
    Resumable(usize, usize, Coroutine),
}

fn pretty(v: &V, strs: &[&str]) -> String {
    match v {
        V::String(s) => strs[*s].to_string(),
        V::Effect(eff) => format!("{}!", strs[*eff]),
        V::Fn(c, v, frame) => format!("{c}(captured:{v},frame:{frame})"),
        V::Closure(c, vs) => {
            let closed = vs.iter().map(|v| pretty(v, strs)).collect::<Vec<_>>();
            format!("{c} [{}]", closed.join(", "))
        }
        V::Record(s, vs) => {
            let items = vs.iter().map(|v| pretty(v, strs)).collect::<Vec<_>>();
            format!("{}({})", pretty(&V::String(*s), strs), items.join(", "))
        }
        V::Recursive(c, vs) => {
            let closed = vs.iter().map(|v| pretty(v, strs)).collect::<Vec<_>>();
            format!("~>{c} [{}]", closed.join(", "))
        }
        V::Resumable(c, v, coroutine) => {
            format!("resumable {c}, ({v}), {coroutine:?}")
        }
    }
}

#[derive(Debug)]
pub enum VmState<'c> {
    Done(V, Vec<&'c str>),
    Resumable(Resumable<'c>, V),
}

#[derive(Debug)]
pub struct Resumable<'c>(Vm<'c>, usize);

impl<'c> Resumable<'c> {
    pub fn strings(&self) -> &[&str] {
        &self.0.strings
    }

    pub fn get_effect_name(&self) -> Option<&str> {
        let Resumable(vm, eff) = self;
        vm.strings.get(*eff).copied()
    }

    pub fn intern_string(&mut self, s: &'c str) -> V {
        let Resumable(vm, _) = self;
        let s = vm.strings.iter().position(|x| *x == s).unwrap_or_else(|| {
            vm.strings.push(s);
            vm.strings.len() - 1
        });
        V::String(s)
    }

    pub fn run(self, arg: V) -> Result<VmState<'c>, usize> {
        let Resumable(mut vm, _) = self;
        vm.temps.push(arg);
        vm.run()
    }
}

impl<'c> Vm<'c> {
    pub fn run(mut self) -> Result<VmState<'c>, usize> {
        let vars = &mut self.vars;
        let temps = &mut self.temps;
        let frames = &mut self.frames;
        let handlers = &mut self.handlers;
        while let Some(op) = self.bytecode.get(self.ip).copied() {
            let i = self.ip;
            if cfg!(test) {
                println!("==> {i}: {op:?}");
            }
            self.ip += 1;
            match op {
                Op::PushVar(v) => {
                    let v: &V = &vars[vars.len() - 1 - v];
                    temps.push(v.clone());
                }
                Op::PushString(s) => temps.push(V::String(s)),
                Op::PushEffect(eff) => temps.push(V::Effect(eff)),
                Op::PushFn(code, captured) => temps.push(V::Fn(code, captured, frames.len())),
                Op::Rec => match temps.pop().ok_or(i)? {
                    V::Fn(c, v, _) => {
                        // TODO: does the frame not matter here? is it always safe to create a closure?
                        let captured = vars[vars.len() - v..].to_vec();
                        temps.push(V::Recursive(c, Rc::new(captured)));
                    }
                    V::Closure(c, captured) => {
                        temps.push(V::Recursive(c, captured));
                    }
                    v => temps.push(v),
                },
                Op::Apply => {
                    let (f, mut arg) = (temps.pop().ok_or(i)?, temps.pop().ok_or(i)?);
                    if let V::Fn(c, v, _) = arg {
                        arg = V::Closure(c, Rc::new(vars[vars.len() - v..].to_vec()));
                    }
                    match (f, arg) {
                        (V::Effect(eff), arg) => {
                            let handler = handlers.iter().rev().find(|h| h.effect == eff);
                            match handler.cloned() {
                                Some(handler) => {
                                    let (v, t, f, h) = handler.state;
                                    let res_vars = vars.drain(v..).collect::<Vec<_>>();
                                    let res_temps = temps.drain(t..).collect::<Vec<_>>();
                                    let res_frames = frames.drain(f..).collect::<Vec<_>>();
                                    let res_handlers = handlers.drain(h..).collect::<Vec<_>>();
                                    frames.push((vars.len(), handler.ret));
                                    vars.extend(handler.captured.iter().cloned());
                                    let coroutine = Coroutine {
                                        vars: res_vars,
                                        temps: res_temps,
                                        frames: res_frames,
                                        handlers: res_handlers,
                                    };
                                    vars.push(V::Resumable(self.ip, v, coroutine));
                                    temps.push(arg);
                                    self.ip = handler.code;
                                }
                                None => return Ok(VmState::Resumable(Resumable(self, eff), arg)),
                            }
                        }
                        (V::Resumable(c, v, coroutine), arg) => {
                            let offset = vars.len() as i64 - v as i64;
                            frames.push((vars.len(), self.ip));
                            vars.extend(coroutine.vars);
                            temps.extend(coroutine.temps);
                            frames.extend(
                                coroutine
                                    .frames
                                    .into_iter()
                                    .map(|(v, ret)| ((v as i64 + offset) as usize, ret)),
                            );
                            handlers.extend(coroutine.handlers);
                            temps.push(arg);
                            self.ip = c;
                        }
                        (V::Recursive(c, captured), arg) => {
                            temps.push(arg);
                            frames.push((vars.len(), self.ip - 1));
                            vars.extend(captured.iter().cloned());
                            vars.push(V::Recursive(c, captured));
                            self.ip = c;
                        }
                        (V::Closure(c, captured), arg) => {
                            // TODO: if the next op is an Op::Return, we might want to do TCO
                            frames.push((vars.len(), self.ip));
                            vars.extend(captured.iter().cloned());
                            vars.push(arg);
                            self.ip = c;
                        }
                        (V::Fn(c, _, _), arg) => {
                            frames.push((vars.len(), self.ip));
                            vars.push(arg);
                            self.ip = c;
                        }
                        (V::String(s), arg) => temps.push(V::Record(s, vec![Rc::new(arg)])),
                        (V::Record(s, mut items), arg) => {
                            items.push(Rc::new(arg));
                            temps.push(V::Record(s, items))
                        }
                    }
                }
                Op::Return => {
                    let (frame, ret) = frames.pop().ok_or(i)?;
                    match (temps.last().cloned(), self.bytecode.get(ret)) {
                        (Some(V::Fn(c, _, f)), Some(Op::Apply)) if f == frames.len() + 1 => {
                            let _f = temps.pop();
                            let mut arg = temps.pop().ok_or(i)?;
                            if let V::Fn(c, v, _) = arg {
                                let captured = Rc::new(vars[frame - v..frame].to_vec());
                                arg = V::Closure(c, captured);
                            }
                            frames.push((frame, ret + 1));
                            vars.push(arg);
                            self.ip = c;
                        }
                        (Some(V::Fn(c, v, f)), _) if f == frames.len() + 1 => {
                            let _f = temps.pop();
                            temps.push(V::Closure(c, Rc::new(vars[vars.len() - v..].to_vec())));
                            vars.truncate(frame);
                            self.ip = ret
                        }
                        _ => {
                            vars.truncate(frame);
                            self.ip = ret
                        }
                    }
                }
                Op::Pop => match temps.pop().ok_or(i)? {
                    V::Recursive(c, captured) => {
                        frames.push((vars.len(), self.ip - 1));
                        vars.extend(captured.iter().cloned());
                        vars.push(V::Recursive(c, captured));
                        self.ip = c;
                    }
                    v => match (v, temps.pop().ok_or(i)?, temps.pop().ok_or(i)?) {
                        (V::Record(f, mut xs), t, _) => {
                            let x = xs.pop().ok_or(i)?;
                            temps.push(x.as_ref().clone());
                            temps.push(if xs.is_empty() {
                                V::String(f)
                            } else {
                                V::Record(f, xs)
                            });
                            temps.push(t);
                        }
                        (_, _, f) => {
                            temps.push(V::String(0));
                            temps.push(f);
                            self.ip += 1;
                        }
                    },
                },
                Op::Try => {
                    let v = temps.pop().ok_or(i)?;
                    let eff = temps.pop().ok_or(i)?;
                    let mut handler = temps.pop().ok_or(i)?;
                    if let V::Fn(c, v, _) = handler {
                        handler = V::Closure(c, Rc::new(vars[vars.len() - v..].to_vec()));
                    }
                    match (eff, handler) {
                        (V::Effect(effect), V::Closure(code, captured)) => {
                            let state = (vars.len(), temps.len(), frames.len(), handlers.len());
                            let ret = self.ip;
                            handlers.push(Handler {
                                effect,
                                code,
                                state,
                                captured,
                                ret,
                            });
                            temps.push(V::String(0));
                            temps.push(v);
                        }
                        _ => temps.push(v),
                    }
                }
                Op::Unwind => {
                    handlers.pop();
                }
                Op::If => {
                    let (a, b, t, f) = (
                        temps.pop().ok_or(i)?,
                        temps.pop().ok_or(i)?,
                        temps.pop().ok_or(i)?,
                        temps.pop().ok_or(i)?,
                    );
                    let branch = match (a, b, t, f) {
                        (V::String(a), V::String(b), t, _) if a == b => t,
                        (_, _, _, f) => f,
                    };
                    temps.push(V::String(0));
                    temps.push(branch);
                }
            }
            if cfg!(test) {
                for (i, v) in vars.iter().rev().enumerate() {
                    println!("        {i}: {}", pretty(v, &self.strings));
                }
                for v in temps.iter() {
                    println!("  {}", pretty(v, &self.strings));
                }
            }
        }
        Ok(VmState::Done(
            self.temps.pop().ok_or(self.ip)?,
            self.strings,
        ))
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, io::Write, path::PathBuf};

    use super::*;

    #[test]
    fn eval_raw_app1() {
        // (\x.x) "Foo"
        let expr = Expr::App(
            Box::new(Expr::Abs(Box::new(Expr::Var(0)))),
            Box::new(Expr::String(0)),
        );
        let strings = vec!["Foo"];
        assert_eq!(eval_expr(expr, strings).unwrap(), "Foo");
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
        let strings = vec!["Foo", "Bar"];
        assert_eq!(eval_expr(expr, strings).unwrap(), "Bar");
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
        let strings = vec!["Foo", "Bar", "Baz", "Vec"];
        assert_eq!(eval_expr(expr, strings).unwrap(), "Vec(Baz, Foo, Baz)");
    }

    #[test]
    fn eval_raw_app4() {
        // (\x.(x(\x."Foo") "Bar")) "Baz"
        let expr = Expr::App(
            Box::new(Expr::Abs(Box::new(Expr::App(
                Box::new(Expr::Var(0)),
                Box::new(Expr::App(
                    Box::new(Expr::Abs(Box::new(Expr::String(0)))),
                    Box::new(Expr::String(1)),
                )),
            )))),
            Box::new(Expr::String(2)),
        );
        let strings = vec!["Foo", "Bar", "Baz"];
        assert_eq!(eval_expr(expr, strings).unwrap(), "Baz(Foo)");
    }

    #[test]
    fn eval_raw_if() {
        // if(Foo, Foo, { True }, { False })
        let t = abs(Expr::String(1));
        let f = abs(Expr::String(2));
        let if_fn = abs(abs(abs(abs(Expr::If(
            Box::new(Expr::Var(3)),
            Box::new(Expr::Var(2)),
            Box::new(Expr::Var(1)),
            Box::new(Expr::Var(0)),
        )))));
        let expr = app(app(app(app(if_fn, Expr::String(0)), Expr::String(0)), t), f);
        let strings = vec!["Foo", "True", "False"];
        assert_eq!(eval_expr(expr, strings).unwrap(), "True");
    }

    #[test]
    fn eval_raw_pop() {
        // pop(Foo(Bar), 'f => { 'x => { x } }, { Error })
        let foo_bar = app(Expr::String(0), Expr::String(1));
        let t = abs(abs(Expr::Var(0)));
        let f = abs(Expr::String(2));
        let pop_fn = abs(abs(abs(Expr::Pop(
            Box::new(Expr::Var(2)),
            Box::new(Expr::Var(1)),
            Box::new(Expr::Var(0)),
        ))));
        let expr = app(app(app(pop_fn, foo_bar), t), f);
        let strings = vec!["Foo", "Bar", "Error"];
        assert_eq!(eval_expr(expr, strings).unwrap(), "Bar");
    }

    #[test]
    fn eval_raw_pop_rec() {
        // pop('r ~> { Cons(Foo, r) }, 'f => { 'x => { f } }, { Error })
        let foo_app_rec = Expr::Rec(Box::new(abs(app(
            app(Expr::String(0), Expr::String(1)),
            Expr::Var(0),
        ))));
        let t = abs(abs(Expr::Var(1)));
        let f = abs(Expr::String(2));
        let pop_fn = abs(abs(abs(Expr::Pop(
            Box::new(Expr::Var(2)),
            Box::new(Expr::Var(1)),
            Box::new(Expr::Var(0)),
        ))));
        let expr = app(app(app(pop_fn, foo_app_rec), t), f);
        let strings = vec!["Cons", "Foo", "Error"];
        assert_eq!(eval_expr(expr, strings).unwrap(), "Cons(Foo)");
    }

    fn pretty_prg<'c>(prg: &Prg<'c>) -> String {
        fn pretty<'c>(ast: &Ast<'c>, lvl: usize, buf: &mut String) {
            let indent = "  ";
            match &ast.1 {
                A::Var(s) | A::String(s) | A::Binding(s) => buf.push_str(s),
                A::Block(items) => {
                    buf.push('{');
                    for item in items {
                        buf.push('\n');
                        buf.push_str(&indent.repeat(lvl + 1));
                        pretty(&item, lvl + 1, buf);
                    }
                    buf.push('}')
                }
                A::Call(call, args) => {
                    buf.push('(');
                    match call {
                        Call::Prefix(f) => pretty(&f, lvl, buf),
                        Call::Infix(f) => buf.push_str(f),
                        Call::Keyword(f) => buf.push_str(&f.join("")),
                    }
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
        for item in prg.0.iter() {
            pretty(item, 0, &mut buf);
            buf.push('\n');
        }
        buf
    }

    fn pretty_expr<'c>(expr: &Expr, strs: &[&'c str]) -> String {
        fn pretty<'c>(expr: &Expr, strs: &[&'c str], lvl: usize, buf: &mut String) {
            let indent = "  ";
            match expr {
                Expr::Var(v) => buf.push_str(&v.to_string()),
                Expr::String(s) => buf.push_str(&format!("\"{}\"", strs[*s])),
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
                Expr::Pop(v, t, f) => {
                    buf.push_str("( pop");
                    for expr in [v, t, f] {
                        buf.push('\n');
                        buf.push_str(&indent.repeat(lvl + 1));
                        pretty(expr, strs, lvl + 1, buf);
                    }
                    buf.push_str(" )");
                }
                Expr::Try(v, e, h) => {
                    buf.push_str("( try");
                    for expr in [v, e, h] {
                        buf.push('\n');
                        buf.push_str(&indent.repeat(lvl + 1));
                        pretty(expr, strs, lvl + 1, buf);
                    }
                    buf.push_str(" )");
                }
                Expr::If(a, b, t, f) => {
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

    fn pretty_bytecode(vm: &Vm) -> String {
        let mut buf = String::new();
        for (i, op) in vm.bytecode.iter().enumerate() {
            match op {
                Op::Return => buf.push_str(&format!("{i:05}:   Return\n")),
                Op::PushString(s) => match vm.strings.get(*s) {
                    Some(s) => buf.push_str(&format!("{i:05}: PushString(\"{s}\")\n")),
                    None => buf.push_str(&format!("{i:05}: {op:05?}\n")),
                },
                Op::PushEffect(s) => match vm.strings.get(*s) {
                    Some(s) => buf.push_str(&format!("{i:05}: PushEffect(\"{s}\")\n")),
                    None => buf.push_str(&format!("{i:05}: {op:05?}\n")),
                },
                _ => buf.push_str(&format!("{i:05}: {op:05?}\n")),
            }
        }
        buf
    }

    fn eval_expr(expr: Expr, strings: Vec<&str>) -> Result<String, String> {
        let bytecode = compile_prg(expr, strings.clone());
        println!("{}", pretty_bytecode(&bytecode));
        match bytecode.run() {
            Ok(VmState::Done(v, strs)) => Ok(pretty(&v, &strs)),
            Ok(VmState::Resumable(res, arg)) => {
                Err(format!("Effect {:?} with {arg:?}", res.get_effect_name()))
            }
            Err(e) => Err(format!("Error at op {e}")),
        }
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
    const OVERWRITE_TESTS: bool = false;

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

    fn test_without_run<'c>(code: &'c str) -> (Vec<String>, Vec<Vm<'c>>) {
        let mut results = vec![];
        let mut compiled = vec![];
        match parse(code) {
            Err(e) => results.push(e),
            Ok(parsed) => {
                results.push(pretty_prg(&parsed));
                match desugar(parsed.clone()) {
                    Err(e) => results.push(e),
                    Ok((expr, strs)) => {
                        results.push(pretty_expr(&expr, &strs));
                        let vm = compile_prg(expr, strs.clone());
                        results.push(pretty_bytecode(&vm));
                        compiled.push(vm);
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
        if !OVERWRITE_TESTS {
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
        if OVERWRITE_TESTS {
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
                    Ok(VmState::Done(v, strs)) => actual.push(pretty(&v, &strs)),
                    Ok(VmState::Resumable(res, arg)) => {
                        actual.push(format!("Effect {:?} with {arg:?}", res.get_effect_name()))
                    }
                    Err(e) => actual.push(format!("Error at op {e}")),
                }
            }
            results.push((code, expected, actual));
        }
        report(path, results)
    }

    #[test]
    fn test_parse() -> Result<(), String> {
        test(PathBuf::from("tests/parse.txt"))
    }

    #[test]
    fn test_err_parse() -> Result<(), String> {
        test(PathBuf::from("tests/err_parse.txt"))
    }

    #[test]
    fn test_err_desugar() -> Result<(), String> {
        test(PathBuf::from("tests/err_desugar.txt"))
    }

    #[test]
    fn test_run_apply() -> Result<(), String> {
        test(PathBuf::from("tests/run_apply.txt"))
    }

    #[test]
    fn test_run_fn() -> Result<(), String> {
        test(PathBuf::from("tests/run_fn.txt"))
    }

    #[test]
    fn test_run_block() -> Result<(), String> {
        test(PathBuf::from("tests/run_block.txt"))
    }

    #[test]
    fn test_run_if() -> Result<(), String> {
        test(PathBuf::from("tests/run_if.txt"))
    }

    #[test]
    fn test_run_pop() -> Result<(), String> {
        test(PathBuf::from("tests/run_pop.txt"))
    }

    #[test]
    fn test_run_rec() -> Result<(), String> {
        test(PathBuf::from("tests/run_rec.txt"))
    }

    fn test_with_print_effect(path: PathBuf) -> Result<(), String> {
        let tests = parse_tests(path.clone())?;
        let mut results = vec![];
        for (code, expected) in tests {
            let (mut actual, compiled) = test_without_run(&code);
            for vm in compiled {
                let mut printed = vec![];
                let mut result = vm.run();
                let nil = "Nil";
                loop {
                    match result {
                        Ok(VmState::Resumable(mut res, arg)) => {
                            let arg = pretty(&arg, res.strings());
                            match res.get_effect_name() {
                                Some(eff) if eff == "print" => {
                                    printed.push(format!("\"{arg}\"\n"));
                                    let nil = res.intern_string(nil);
                                    result = res.run(nil);
                                }
                                name => break actual.push(format!("Effect {name:?} with {arg}")),
                            }
                        }
                        Ok(VmState::Done(v, strs)) => {
                            break actual.push(printed.join("") + &pretty(&v, &strs));
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
    fn test_run_with_effects() -> Result<(), String> {
        test_with_print_effect(PathBuf::from("tests/run_with_effects.txt"))
    }

    #[test]
    fn test_run_with_rec_effects() -> Result<(), String> {
        test_with_print_effect(PathBuf::from("tests/run_with_rec_effects.txt"))
    }
}
