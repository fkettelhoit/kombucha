use std::{
    array,
    borrow::Cow,
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
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Separator,
}

fn scan(code: &str) -> Vec<(Tok, usize, &str)> {
    let mut toks = vec![];
    let mut i = 0;
    for (j, c) in code.char_indices().chain(once((code.len(), ' '))) {
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
    List(Vec<Ast<'code>>),
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

enum Reflect {
    Nil = 0,
    Value = 1,
    Binding = 2,
    Compound = 3,
    Template = 4,
    Tuple = 5,
}

const NIL: &str = "";
const VALUE: &str = "Value";
const BINDING: &str = "Binding";
const COMPOUND: &str = "Compound";
const TEMPLATE: &str = "Template";
const TUPLE: &str = "Tuple";

fn desugar(Prg(block, code): Prg<'_>) -> Result<(Expr, Vec<String>), String> {
    struct Ctx<'c> {
        bindings: Vec<&'c str>,
        vars: Vec<&'c str>,
        strs: Vec<String>,
    }
    let mut ctx = Ctx {
        bindings: vec![],
        vars: vec![],
        strs: vec![String::new(); 6],
    };
    ctx.strs[Reflect::Nil as usize] = NIL.to_string();
    ctx.strs[Reflect::Value as usize] = VALUE.to_string();
    ctx.strs[Reflect::Binding as usize] = BINDING.to_string();
    ctx.strs[Reflect::Compound as usize] = COMPOUND.to_string();
    ctx.strs[Reflect::Template as usize] = TEMPLATE.to_string();
    ctx.strs[Reflect::Tuple as usize] = TUPLE.to_string();

    fn resolve_var(v: &str, ctx: &Ctx) -> Option<usize> {
        ctx.vars.iter().rev().position(|x| *x == v)
    }
    fn resolve_str<'c>(s: String, ctx: &mut Ctx<'c>) -> usize {
        ctx.strs.iter().position(|x| *x == s).unwrap_or_else(|| {
            ctx.strs.push(s);
            ctx.strs.len() - 1
        })
    }
    fn contains_bindings<'c>(Ast(_, ast): &Ast<'c>) -> bool {
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
    fn desug_reflect<'c>(ast: Ast<'c>, ctx: &mut Ctx<'c>) -> Result<Expr, (usize, String)> {
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
                Ok(app(Expr::String(Reflect::Tuple as usize), f))
            }
            A::Call(call, args) if has_bindings => {
                let mut desugared_args = vec![];
                for arg in args {
                    desugared_args.push(desug_reflect(arg, ctx)?);
                }
                let mut reflector = Reflect::Compound;
                let mut f = match call {
                    Call::Infix(f) => desug_val(Ast(ast.0, A::Var(f)), ctx)?,
                    Call::Prefix(f) => {
                        if let Ast(_, A::Binding(_)) = *f {
                            reflector = Reflect::Template
                        }
                        desug_val(*f, ctx)?
                    }
                    Call::Keyword(keywords) => match resolve_var(&keywords.join("-"), ctx) {
                        Some(v) => Expr::Var(v),
                        None => return Err((ast.0, keywords.join("-"))),
                    },
                };
                if desugared_args.is_empty() {
                    let nil = app(
                        Expr::String(Reflect::Value as usize),
                        Expr::String(Reflect::Nil as usize),
                    );
                    f = app(f, nil)
                }
                for arg in desugared_args {
                    f = app(f, arg)
                }
                Ok(app(Expr::String(reflector as usize), f))
            }
            A::Var(_) | A::String(_) | A::List(_) | A::Call(_, _) => Ok(app(
                Expr::String(Reflect::Value as usize),
                desug_val(ast, ctx)?,
            )),
            A::Binding(_) => Ok(app(
                Expr::String(Reflect::Binding as usize),
                desug_val(ast, ctx)?,
            )),
            A::Block(_) => desug_val(ast, ctx),
        }
    }
    fn desug_val<'c>(Ast(pos, ast): Ast<'c>, ctx: &mut Ctx<'c>) -> Result<Expr, (usize, String)> {
        match ast {
            A::Var(v) if v.ends_with("!") => {
                Ok(Expr::Effect(resolve_str(v[..v.len() - 1].to_string(), ctx)))
            }
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
                None => Err((pos, v.to_string())),
            },
            A::String(s) => Ok(Expr::String(resolve_str(s.to_string(), ctx))),
            A::Binding(b) => {
                ctx.bindings.push(&b[1..]);
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
                        ctx.vars.push("")
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
                let mut tuple = Expr::String(Reflect::Nil as usize);
                let mut desugared = vec![];
                for item in items {
                    desugared.push(desug_val(item, ctx)?);
                }
                for item in desugared.into_iter().rev() {
                    tuple = app(tuple, item)
                }
                Ok(tuple)
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
    match desug_val(Ast(0, A::Block(block)), &mut ctx) {
        Err((i, v)) => Err(format!("Unbound variable '{v}' at {}", pos_at(i, code))),
        Ok(Expr::Abs(body)) => Ok((*body, ctx.strs)),
        Ok(_) => unreachable!("Expected the main block to be desugared to an abstraction!"),
    }
}

#[derive(Debug, Clone, Default)]
pub struct Bytecode {
    strings: Vec<Cow<'static, str>>,
    ops: Vec<Op>,
    start: usize,
}

impl Bytecode {
    fn new(strings: Vec<impl Into<Cow<'static, str>>>, bytecode: Vec<Op>, start: usize) -> Self {
        Bytecode {
            strings: strings.into_iter().map(|s| s.into()).collect(),
            ops: bytecode,
            start,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Op {
    LoadVar(usize),
    LoadString(usize),
    LoadEffect(usize),
    LoadFn(usize, usize),
    ApplyFnToArg,
    ApplyArgToFn,
    Return,
    Cmp,
    Unpack,
    Try,
    Unwind,
}

fn compile_expr(expr: Expr, ops: &mut Vec<Op>, fns: &mut Vec<Op>) {
    match expr {
        Expr::Var(v) => ops.push(Op::LoadVar(v)),
        Expr::String(s) => ops.push(Op::LoadString(s)),
        Expr::Effect(e) => ops.push(Op::LoadEffect(e)),
        Expr::Abs(body) => {
            let mut f = vec![];
            compile_expr(*body, &mut f, fns);
            f.push(Op::Return);
            let captured = f.iter().fold(0, |captured, op| match *op {
                Op::LoadVar(v) if v > captured => v,
                Op::LoadFn(_, c) if c > captured => c - 1,
                _ => captured,
            });
            ops.push(Op::LoadFn(fns.len(), captured));
            fns.extend(f);
        }
        Expr::App(f, arg) => {
            compile_expr(*f, ops, fns);
            compile_expr(*arg, ops, fns);
            ops.push(Op::ApplyFnToArg);
        }
        Expr::Rec(body) => {
            compile_expr(*body, ops, fns);
            ops.push(Op::LoadFn(0, 0));
            ops.push(Op::ApplyArgToFn);
        }
        Expr::If(a, b, if_t, if_f) => {
            compile_expr(*a, ops, fns);
            compile_expr(*b, ops, fns);
            compile_expr(*if_t, ops, fns);
            compile_expr(*if_f, ops, fns);
            ops.push(Op::Cmp);
            ops.push(Op::ApplyFnToArg);
        }
        Expr::Pop(val, if_t, if_f) => {
            compile_expr(*val, ops, fns);
            compile_expr(*if_t, ops, fns);
            compile_expr(*if_f, ops, fns);
            ops.push(Op::Unpack);
            ops.push(Op::ApplyArgToFn);
            ops.push(Op::ApplyArgToFn);
        }
        Expr::Try(val, eff, handler) => {
            compile_expr(*val, ops, fns);
            compile_expr(*eff, ops, fns);
            compile_expr(*handler, ops, fns);
            ops.push(Op::Try);
            ops.push(Op::ApplyArgToFn);
            ops.push(Op::Unwind);
            ops.push(Op::ApplyArgToFn);
            ops.push(Op::ApplyArgToFn);
        }
    }
}

fn compile_prg(expr: Expr, strings: Vec<String>) -> Bytecode {
    let mut main = vec![];

    // fix = f => x => f(fix(f))(x)
    let mut bytecode = vec![
        // 0: f => ...
        Op::LoadFn(2, 1),
        Op::Return,
        // 2: ... x => f(fix(f))(x)
        Op::LoadVar(1),   // f
        Op::LoadFn(0, 0), // f, fix
        Op::LoadVar(1),   // f, fix, f
        Op::ApplyFnToArg, // f, fix(f)
        Op::ApplyFnToArg, // f(fix(f))
        Op::LoadVar(0),   // f(fix(f)), x
        Op::ApplyFnToArg, // f(fix(f))(x)
        Op::Return,
    ];
    compile_expr(expr, &mut main, &mut bytecode);
    let start = bytecode.len();
    bytecode.extend(main);
    Bytecode::new(strings, bytecode, start)
}

pub fn compile(code: &str) -> Result<Bytecode, String> {
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
    Resumable(usize, Box<Vm>),
}

pub fn pretty(v: &V, strs: &Vec<Cow<'static, str>>) -> String {
    match v {
        V::String(s) if strs[*s] == NIL => "[]".to_string(),
        V::String(s) => strs[*s].to_string(),
        V::Effect(eff) => format!("{}!", strs[*eff]),
        V::Fn(c, v, frame) => format!("{c}(captured:{v},frame:{frame})"),
        V::Closure(c, vs) => {
            let closed = vs.iter().map(|v| pretty(v, strs)).collect::<Vec<_>>();
            format!("{c} [{}]", closed.join(", "))
        }
        V::Record(s, vs) if strs[*s] == NIL => {
            let items = vs.iter().rev().map(|v| pretty(v, strs)).collect::<Vec<_>>();
            format!("[{}]", items.join(", "))
        }
        V::Record(s, vs) => {
            if vs.len() == 1 {
                match *vs[0] {
                    V::String(v) if strs[v] == NIL => {
                        return format!("{}()", pretty(&V::String(*s), strs));
                    }
                    _ => {}
                }
            }
            let items = vs.iter().map(|v| pretty(v, strs)).collect::<Vec<_>>();
            format!("{}({})", strs[*s].to_string(), items.join(", "))
        }
        V::Recursive(c, vs) => {
            let closed = vs.iter().map(|v| pretty(v, strs)).collect::<Vec<_>>();
            format!("~>{c} [{}]", closed.join(", "))
        }
        V::Resumable(_, _) => {
            format!("resumable")
        }
    }
}

#[derive(Debug, Clone)]
pub struct Stack<T: Clone, const SIZE: usize> {
    stack: [Option<T>; SIZE],
    sp: usize,
}

impl<T: Clone, const SIZE: usize> Stack<T, SIZE> {
    fn new() -> Self {
        Self {
            stack: array::from_fn(|_| None),
            sp: 0,
        }
    }
    fn get(&self, i: usize) -> Option<&T> {
        if i >= self.sp {
            panic!("Stack underflow in get")
        }
        self.stack[self.sp - 1 - i].as_ref()
    }
    fn get_n(&self, i: usize) -> Vec<T> {
        if i > self.sp {
            panic!("Stack underflow in get_n")
        }
        self.stack[self.sp - i..self.sp]
            .iter()
            .map(|v| v.clone().unwrap())
            .collect()
    }
    fn pop(&mut self) -> Option<T> {
        if self.sp == 0 {
            return None;
        }
        self.sp -= 1;
        self.stack[self.sp].take()
    }
    fn pop_after(&mut self, sp: usize) -> Stack<T, SIZE> {
        if sp > self.sp {
            panic!("sp > self.sp");
        }
        let mut popped = Stack::new();
        for i in 0..self.sp - sp {
            popped.stack[i] = self.stack[sp + i].take();
        }
        popped.sp = self.sp - sp;
        self.sp = sp;
        popped
    }
    fn push(&mut self, v: T) {
        if self.sp < SIZE {
            self.stack[self.sp] = Some(v);
            self.sp += 1;
        } else {
            panic!("Stack overflow!");
        }
    }
    fn extend(&mut self, mut other: Stack<T, SIZE>) {
        for i in 0..other.sp {
            self.stack[self.sp + i] = other.stack[i].take()
        }
        self.sp += other.sp
    }
}

const VAR_STACK_SIZE: usize = 512;
const TEMP_STACK_SIZE: usize = 256;
const CALL_STACK_SIZE: usize = 256;

#[derive(Debug, Clone)]
pub struct Vm {
    ip: usize,
    vars: Stack<V, VAR_STACK_SIZE>,
    temps: Stack<V, TEMP_STACK_SIZE>,
    frames: Stack<(usize, usize), CALL_STACK_SIZE>,
    handlers: Vec<Handler>,
}

#[derive(Debug, Clone)]
struct Handler {
    effect: usize,
    handler: V,
    state: (usize, usize, usize, usize),
    ret: usize,
}

#[derive(Debug)]
pub enum VmState {
    Done(V, Vec<Cow<'static, str>>),
    Resumable(Resumable, V),
}

#[derive(Debug)]
pub struct Resumable(Bytecode, Vm, usize);

impl Resumable {
    pub fn strings(&self) -> &Vec<Cow<'static, str>> {
        &self.0.strings
    }

    pub fn get_effect_name(&self) -> Option<&str> {
        let Resumable(code, _, eff) = self;
        code.strings.get(*eff).map(|s| s.as_ref())
    }

    pub fn intern_string(&mut self, s: impl Into<Cow<'static, str>>) -> V {
        let s = s.into();
        let Resumable(bc, _, _) = self;
        let s = bc.strings.iter().position(|x| *x == s).unwrap_or_else(|| {
            bc.strings.push(s);
            bc.strings.len() - 1
        });
        V::String(s)
    }

    pub fn run(self, arg: V) -> Result<VmState, usize> {
        let Resumable(bc, mut vm, _) = self;
        vm.temps.push(arg);
        vm.run(bc)
    }
}

impl Bytecode {
    pub fn nil() -> V {
        V::String(Reflect::Nil as usize)
    }

    pub fn run(self) -> Result<VmState, usize> {
        let vm = Vm {
            ip: self.start,
            vars: Stack::new(),
            temps: Stack::new(),
            frames: Stack::new(),
            handlers: vec![],
        };
        vm.run(self)
    }
}

impl Vm {
    fn run(self, code: Bytecode) -> Result<VmState, usize> {
        let Vm {
            mut ip,
            mut vars,
            mut temps,
            mut frames,
            mut handlers,
        } = self;
        while let Some(op) = code.ops.get(ip).copied() {
            let i = ip;
            // if cfg!(test) {
            //     println!("==> {i}: {op:?}");
            // }
            ip += 1;
            match op {
                Op::LoadVar(v) => temps.push(vars.get(v).ok_or(i)?.clone()),
                Op::LoadString(s) => temps.push(V::String(s)),
                Op::LoadEffect(eff) => temps.push(V::Effect(eff)),
                Op::LoadFn(code, captured) => temps.push(V::Fn(code, captured, frames.sp)),
                Op::ApplyFnToArg | Op::ApplyArgToFn => {
                    let (mut arg, f) = match (op, temps.pop().ok_or(i)?, temps.pop().ok_or(i)?) {
                        (Op::ApplyFnToArg, b, a) => (b, a),
                        (_, b, a) => (a, b),
                    };
                    if let V::Fn(c, v, _) = arg {
                        arg = V::Closure(c, Rc::new(vars.get_n(v)));
                    }
                    match (f, arg) {
                        (V::Effect(eff), arg) => {
                            let handler = handlers.iter().rev().find(|h| h.effect == eff);
                            match handler.cloned() {
                                Some(handler) => {
                                    let (v, t, f, h) = handler.state;
                                    let res_vars = vars.pop_after(v);
                                    let res_temps = temps.pop_after(t);
                                    let res_frames = frames.pop_after(f);
                                    let res_handlers = handlers.drain(h..).collect::<Vec<_>>();
                                    handlers.pop();
                                    let vm = Vm {
                                        ip,
                                        vars: res_vars,
                                        temps: res_temps,
                                        frames: res_frames,
                                        handlers: res_handlers,
                                    };
                                    temps.push(arg);
                                    temps.push(V::Resumable(v, Box::new(vm)));
                                    temps.push(handler.handler);
                                    ip = handler.ret;
                                }
                                None => {
                                    let vm = Vm {
                                        ip,
                                        vars,
                                        temps,
                                        frames,
                                        handlers,
                                    };
                                    return Ok(VmState::Resumable(Resumable(code, vm, eff), arg));
                                }
                            }
                        }
                        (V::Resumable(_v, vm), arg) => {
                            frames.push((vm.vars.sp + 1, ip));
                            vars.extend(vm.vars);
                            temps.extend(vm.temps);
                            frames.extend(vm.frames);
                            handlers.extend(vm.handlers);
                            temps.push(arg);
                            ip = vm.ip;
                        }
                        (V::Recursive(c, captured), arg) => {
                            temps.push(arg);
                            frames.push((captured.len() + 1, ip - 1));
                            for v in captured.iter() {
                                vars.push(v.clone());
                            }
                            vars.push(V::Recursive(c, captured));
                            ip = c;
                        }
                        (V::Closure(c, captured), arg) => {
                            // TODO: if the next op is an Op::Return, we might want to do TCO
                            frames.push((captured.len() + 1, ip));
                            for v in captured.iter() {
                                vars.push(v.clone());
                            }
                            vars.push(arg);
                            ip = c;
                        }
                        (V::Fn(c, _, _), arg) => {
                            frames.push((1, ip));
                            vars.push(arg);
                            ip = c;
                        }
                        (V::String(s), arg) => temps.push(V::Record(s, vec![Rc::new(arg)])),
                        (V::Record(s, mut items), arg) => {
                            items.push(Rc::new(arg));
                            temps.push(V::Record(s, items))
                        }
                    }
                }
                Op::Return => {
                    let (captured, ret) = frames.pop().ok_or(i)?;
                    match temps.pop().expect("temps underflow in RETURN") {
                        V::Fn(c, v, f) if f == frames.sp + 1 => {
                            temps.push(V::Closure(c, Rc::new(vars.get_n(v))));
                            vars.pop_after(vars.sp - captured);
                            ip = ret
                        }
                        v => {
                            temps.push(v);
                            vars.pop_after(vars.sp - captured);
                            ip = ret
                        }
                    }
                }
                Op::Unpack => match (
                    temps.pop().ok_or(i)?,
                    temps.pop().ok_or(i)?,
                    temps.pop().ok_or(i)?,
                ) {
                    (_, t, V::Record(f, mut xs)) => {
                        let x = xs.pop().ok_or(i)?;
                        temps.push(x.as_ref().clone());
                        temps.push(if xs.is_empty() {
                            V::String(f)
                        } else {
                            V::Record(f, xs)
                        });
                        temps.push(t);
                    }
                    (f, _, _) => {
                        temps.push(V::String(Reflect::Nil as usize));
                        temps.push(f);
                        ip += 1;
                    }
                },
                Op::Try => {
                    let mut handler = temps.pop().ok_or(i)?;
                    let eff = temps.pop().ok_or(i)?;
                    let v = temps.pop().ok_or(i)?;
                    if let V::Fn(c, v, _) = handler {
                        handler = V::Closure(c, Rc::new(vars.get_n(v)));
                    }
                    match eff {
                        V::Effect(effect) => {
                            let state = (vars.sp, temps.sp, frames.sp, handlers.len() + 1);
                            let ret = ip + 2; // skip apply + unwind
                            handlers.push(Handler {
                                effect,
                                handler,
                                state,
                                ret,
                            });
                            temps.push(V::String(Reflect::Nil as usize));
                            temps.push(v);
                        }
                        _ => {
                            temps.push(v);
                            ip += 4; // skip apply, unwind, apply, apply
                        }
                    }
                }
                Op::Unwind => {
                    handlers.pop();
                    ip += 2;
                }
                Op::Cmp => {
                    let (f, t, b, a) = (
                        temps.pop().ok_or(i)?,
                        temps.pop().ok_or(i)?,
                        temps.pop().ok_or(i)?,
                        temps.pop().ok_or(i)?,
                    );
                    let branch = match (a, b, t, f) {
                        (V::String(a), V::String(b), t, _) if a == b => t,
                        (_, _, _, f) => f,
                    };
                    temps.push(branch);
                    temps.push(V::String(Reflect::Nil as usize));
                }
            }
            // if cfg!(test) {
            //     for (i, v) in vars.iter().rev().enumerate() {
            //         println!("        {i}: {}", pretty(v, &self.strings));
            //     }
            //     for v in temps.iter() {
            //         println!("  {}", pretty(v, &self.strings));
            //     }
            // }
        }
        Ok(VmState::Done(temps.pop().ok_or(ip)?, code.strings))
    }
}

#[cfg(test)]
mod tests {
    use std::{env, fs, io::Write, path::PathBuf};

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

    fn pretty_prg<'c>(prg: &Prg<'c>) -> String {
        fn pretty<'c>(ast: &Ast<'c>, lvl: usize, buf: &mut String) {
            let indent = "  ";
            match &ast.1 {
                A::Var(s) | A::String(s) | A::Binding(s) => buf.push_str(s),
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
                    buf.push_str("( ");
                    for (i, item) in items.iter().enumerate() {
                        if i != 0 {
                            buf.push('\n');
                            buf.push_str(&indent.repeat(lvl + 1));
                        }
                        pretty(&item, lvl + 1, buf);
                    }
                    buf.push_str(" )")
                }
                A::Call(call, args) => {
                    buf.push('(');
                    match call {
                        Call::Prefix(f) => pretty(&f, lvl, buf),
                        Call::Infix(f) => buf.push_str(f),
                        Call::Keyword(f) => buf.push_str(&f.join("-")),
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

    fn pretty_bytecode(vm: &Bytecode) -> String {
        let mut buf = String::new();
        for (i, op) in vm.ops.iter().enumerate() {
            match op {
                Op::Return => buf.push_str(&format!("{i:05}:   Return\n")),
                Op::LoadString(s) => match vm.strings.get(*s) {
                    Some(s) => buf.push_str(&format!("{i:05}: PushString(\"{s}\")\n")),
                    None => buf.push_str(&format!("{i:05}: {op:05?}\n")),
                },
                Op::LoadEffect(s) => match vm.strings.get(*s) {
                    Some(s) => buf.push_str(&format!("{i:05}: PushEffect(\"{s}\")\n")),
                    None => buf.push_str(&format!("{i:05}: {op:05?}\n")),
                },
                _ => buf.push_str(&format!("{i:05}: {op:05?}\n")),
            }
        }
        buf
    }

    fn eval_expr(expr: Expr, strings: Vec<&str>) -> Result<String, String> {
        let bytecode = compile_prg(expr, strings.into_iter().map(|s| s.to_string()).collect());
        println!("{}", pretty_bytecode(&bytecode));
        match bytecode.run() {
            Ok(VmState::Done(v, strs)) => Ok(pretty(&v, &strs)),
            Ok(VmState::Resumable(res, arg)) => Err(format!(
                "{}!({})",
                res.get_effect_name().unwrap(),
                pretty(&arg, res.strings())
            )),
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
                    Ok(VmState::Done(v, strs)) => actual.push(pretty(&v, &strs)),
                    Ok(VmState::Resumable(res, arg)) => actual.push(format!(
                        "{}!({})",
                        res.get_effect_name().unwrap(),
                        pretty(&arg, res.strings())
                    )),
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
    fn test_run_macro() -> Result<(), String> {
        test(PathBuf::from("tests/run_macro.txt"))
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
                loop {
                    match result {
                        Ok(VmState::Resumable(vm, arg)) => {
                            let arg = pretty(&arg, vm.strings());
                            match vm.get_effect_name().unwrap() {
                                eff if eff == "print" => {
                                    printed.push(format!("\"{arg}\"\n"));
                                    result = vm.run(Bytecode::nil());
                                }
                                name => break actual.push(format!("{name}!({arg})")),
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

    #[test]
    fn test_run_with_std_fns() -> Result<(), String> {
        test(PathBuf::from("tests/run_with_std_fns.txt"))
    }
}
