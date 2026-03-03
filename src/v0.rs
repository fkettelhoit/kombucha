// --- Tokens ---

#[derive(Debug, Clone, Copy)]
pub struct Pos(usize);

impl Pos {
    fn line_in(&self, code: &str) -> String {
        let (mut line, mut col) = (1, 1);
        for c in code.chars().take(self.0) {
            if c == '\n' {
                line += 1;
                col = 0;
            }
            col += 1;
        }
        format!("line {line}, col {col}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tok<'code> {
    Sep(char),
    Var(&'code str),
    Key(&'code str),
    Str(&'code str),
    Int(i64),
}

impl std::fmt::Display for Tok<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tok::Sep('\n') => write!(f, "'\\n'"),
            Tok::Sep(c) => write!(f, "'{c}'"),
            Tok::Var(s) | Tok::Key(s) | Tok::Str(s) => write!(f, "'{s}'"),
            Tok::Int(n) => write!(f, "'{n}'"),
        }
    }
}

fn scan(code: &str) -> Vec<(Pos, Tok<'_>)> {
    let mut toks = vec![];
    let mut i = 0;
    let mut chars = code.char_indices().chain(std::iter::once((code.len(), ' ')));
    fn push_ident<'a>(toks: &mut Vec<(Pos, Tok<'a>)>, code: &'a str, i: usize, j: usize) {
        let s = &code[i..j];
        if s.is_empty() {
            return;
        }
        if let Ok(n) = s.parse::<i64>() {
            toks.push((Pos(i), Tok::Int(n)));
        } else if s.as_bytes()[0].is_ascii_uppercase() {
            toks.push((Pos(i), Tok::Str(s)));
        } else {
            toks.push((Pos(i), Tok::Var(s)));
        }
    }
    while let Some((j, c)) = chars.next() {
        match c {
            ':' if i < j => {
                toks.push((Pos(i), Tok::Key(&code[i..j])));
                i = j + 1;
            }
            '(' | ')' | '[' | ']' | '{' | '}' | '.' | ':' | ';' | ',' | '\n' => {
                push_ident(&mut toks, code, i, j);
                toks.push((Pos(j), Tok::Sep(c)));
                i = j + 1;
            }
            '/' if code.get(j + 1..j + 2) == Some("/") => {
                push_ident(&mut toks, code, i, j);
                i = chars.by_ref().find(|(_, c)| *c == '\n').map_or(code.len(), |(j, _)| j + 1);
                toks.push((Pos(i), Tok::Sep('\n')));
            }
            '"' => {
                push_ident(&mut toks, code, i, j);
                let start = j + 1;
                let mut close = code.len();
                loop {
                    match chars.next() {
                        Some((_, '\\')) => {
                            chars.next();
                        }
                        Some((k, '"')) => {
                            close = k;
                            break;
                        }
                        Some(_) => {}
                        None => break,
                    }
                }
                toks.push((Pos(j), Tok::Str(&code[start..close])));
                i = close + 1;
            }
            c if c.is_ascii_whitespace() => {
                push_ident(&mut toks, code, i, j);
                i = j + 1;
            }
            _ => {}
        }
    }
    toks
}

// --- AST ---

#[derive(Debug, Clone)]
pub enum Ast<'code> {
    Var(Pos, &'code str),
    Str(Pos, &'code str),
    Int(Pos, i64),
    List(Pos, Vec<Ast<'code>>),
    Tuple(Pos, Vec<Ast<'code>>),
    Block(Pos, Vec<Ast<'code>>),
    Prefix(Pos, Box<Ast<'code>>, Vec<Ast<'code>>),
    Infix(Pos, &'code str, [Box<Ast<'code>>; 2], Option<Box<Ast<'code>>>),
}

impl std::fmt::Display for Ast<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ast::Var(_, s) => write!(f, "{s}"),
            Ast::Str(_, s) => write!(f, "{s}"),
            Ast::Int(_, n) => write!(f, "{n}"),
            Ast::List(_, elems) => {
                write!(f, "[")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, "]")
            }
            Ast::Tuple(_, elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Ast::Block(_, elems) if elems.is_empty() => write!(f, "{{}}"),
            Ast::Block(_, elems) => {
                write!(f, "{{ ")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, " }}")
            }
            Ast::Prefix(_, func, args) => {
                write!(f, "{func}(")?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{a}")?;
                }
                write!(f, ")")
            }
            Ast::Infix(_, op, [x, y], None) => write!(f, "({x} {op} {y})"),
            Ast::Infix(_, op, [x, y], Some(t)) => write!(f, "({x} {op} {y} {t})"),
        }
    }
}

fn is_bare_string(s: &str) -> bool {
    s.chars().next().is_some_and(|c| c.is_ascii_uppercase()) && !s.contains(char::is_whitespace)
}

fn is_kw_list(ast: &Ast) -> bool {
    matches!(ast, Ast::List(_, items) if !items.is_empty() && items.iter().all(|i| matches!(i,
        Ast::Tuple(_, elems) if matches!(elems.first(), Some(Ast::Str(_, s)) if
            s.chars().next().is_some_and(|c| c.is_ascii_lowercase())))))
}

const MAX_ONE_LINE_COMPLEXITY: usize = 12;

impl Ast<'_> {
    fn pos(&self) -> Pos {
        match self {
            Ast::Var(p, _)
            | Ast::Str(p, _)
            | Ast::Int(p, _)
            | Ast::List(p, _)
            | Ast::Tuple(p, _)
            | Ast::Block(p, _)
            | Ast::Prefix(p, _, _)
            | Ast::Infix(p, _, _, _) => *p,
        }
    }

    fn size(&self) -> usize {
        match self {
            Ast::Str(_, s) if s.contains(char::is_whitespace) => 4,
            Ast::Var(_, _) | Ast::Str(_, _) | Ast::Int(_, _) => 1,
            Ast::List(_, xs) | Ast::Tuple(_, xs) | Ast::Block(_, xs) => {
                xs.iter().map(|x| x.size()).sum::<usize>() + 1
            }
            Ast::Prefix(_, f, xs) => f.size() + xs.iter().map(|x| x.size()).sum::<usize>(),
            Ast::Infix(_, _, [a, b], trailing) => {
                a.size() + 1 + b.size() + trailing.as_ref().map_or(0, |t| t.size())
            }
        }
    }

    pub fn pretty(&self) -> String {
        match self {
            Ast::Block(_, defs) => {
                defs.iter().map(|def| def.pretty_lvl(0)).collect::<Vec<_>>().join("\n\n")
            }
            ast => ast.pretty_lvl(0),
        }
    }

    fn pretty_lvl(&self, lvl: usize) -> String {
        fn one_line(xs: &[Ast], lvl: usize) -> String {
            xs.iter().map(|x| x.pretty_lvl(lvl)).collect::<Vec<_>>().join(", ")
        }
        fn multi_line(xs: &[Ast], lvl: usize) -> String {
            let indent = "  ";
            let inner = xs
                .iter()
                .map(|x| indent.repeat(lvl) + &x.pretty_lvl(lvl) + "\n")
                .collect::<String>();
            "\n".to_string() + &inner + &indent.repeat(lvl - 1)
        }
        fn wrap(open: char, close: char, xs: &[Ast], lvl: usize, size: usize) -> String {
            if size < MAX_ONE_LINE_COMPLEXITY {
                format!("{open}{}{close}", one_line(xs, lvl + 1))
            } else {
                format!("{open}{}{close}", multi_line(xs, lvl + 1))
            }
        }
        match self {
            Ast::Var(_, s) => s.to_string(),
            Ast::Str(_, s) if is_bare_string(s) => s.to_string(),
            Ast::Str(_, s) => format!("\"{s}\""),
            Ast::Int(_, n) => n.to_string(),
            Ast::List(_, xs) => wrap('[', ']', xs, lvl, self.size()),
            Ast::Tuple(_, xs) => {
                if let [Ast::Str(_, k), v] = xs.as_slice() {
                    if k.chars().next().is_some_and(|c| c.is_ascii_lowercase()) {
                        return format!("{k}: {}", v.pretty_lvl(lvl));
                    }
                }
                wrap('(', ')', xs, lvl, self.size())
            }
            Ast::Block(_, xs) => format!("{{{}}}", multi_line(xs, lvl + 1)),
            Ast::Prefix(_, f, xs) => {
                let (args, kw) = if xs.last().map_or(false, is_kw_list) {
                    (&xs[..xs.len() - 1], Some(xs.last().unwrap()))
                } else {
                    (xs.as_slice(), None)
                };
                let split =
                    args.partition_point(|x| !matches!(x, Ast::Block(_, _) | Ast::List(_, _)));
                let call_args = &args[..split];
                let trailing_args = &args[split..];

                let size: usize = f.size() + call_args.iter().map(|x| x.size()).sum::<usize>();
                let args_str = if size < MAX_ONE_LINE_COMPLEXITY {
                    one_line(call_args, lvl + 1)
                } else {
                    multi_line(call_args, lvl + 1)
                };
                let has_trailing_args = !trailing_args.is_empty() || kw.is_some();
                let mut result = if call_args.is_empty() && has_trailing_args {
                    f.pretty_lvl(lvl)
                } else if has_trailing_args {
                    format!("{} ({args_str})", f.pretty_lvl(lvl))
                } else {
                    format!("{}({args_str})", f.pretty_lvl(lvl))
                };
                for t in trailing_args {
                    result.push_str(&format!(" {}", t.pretty_lvl(lvl)));
                }
                if let Some(Ast::List(_, kws)) = kw {
                    for kw in kws {
                        result.push_str(&format!(" {}", kw.pretty_lvl(lvl)));
                    }
                }
                result
            }
            Ast::Infix(_, op, [a, b], trailing) => {
                let fmt_left = |x: &Ast| match x {
                    Ast::Infix(_, inner_op, _, _) if *inner_op != *op => {
                        format!("({})", x.pretty_lvl(lvl))
                    }
                    _ => x.pretty_lvl(lvl),
                };
                let has_blocks = |x: &Ast| {
                    matches!(x,
                    Ast::Prefix(_, _, args) if args.iter().any(|a|
                        matches!(a, Ast::Block(_, _)) || is_kw_list(a)))
                };
                let fmt_right = |x: &Ast| match x {
                    Ast::Infix(_, _, _, _) => format!("({})", x.pretty_lvl(lvl)),
                    Ast::Tuple(_, elems) if elems.len() != 1 => x.pretty_lvl(lvl),
                    _ if trailing.is_some() => format!("({})", x.pretty_lvl(lvl)),
                    _ if has_blocks(x) => format!("({})", x.pretty_lvl(lvl)),
                    Ast::Prefix(_, f, xs) if xs.iter().any(|a| matches!(a, Ast::List(_, _))) => {
                        let size = f.size() + xs.iter().map(|x| x.size()).sum::<usize>();
                        let args_str = if size < MAX_ONE_LINE_COMPLEXITY {
                            one_line(xs, lvl + 1)
                        } else {
                            multi_line(xs, lvl + 1)
                        };
                        format!("{}({args_str})", f.pretty_lvl(lvl))
                    }
                    _ => x.pretty_lvl(lvl),
                };
                let base = format!("{} {} {}", fmt_left(a), op, fmt_right(b));
                match trailing {
                    Some(t) => format!("{base} {}", t.pretty_lvl(lvl)),
                    None => base,
                }
            }
        }
    }
}

// --- Parser ---

struct Parser<'code> {
    end_pos: Pos,
    toks: std::iter::Peekable<std::vec::IntoIter<(Pos, Tok<'code>)>>,
}

impl<'c> Parser<'c> {
    fn expr(&mut self, expected: &str) -> Result<Ast<'c>, (Pos, String)> {
        if let Some((i, Tok::Key(k))) = self.toks.peek().copied() {
            self.toks.next();
            return Ok(Ast::Tuple(i, vec![Ast::Str(i, k), self.infix("value after key")?]));
        }
        let (i, expr, mut args) = match self.infix(expected)? {
            Ast::Infix(i, f, [x, y], None) => match self.toks.peek().map(|(_, t)| t) {
                Some(Tok::Sep('[' | '{')) => {
                    let trailing = self.value("trailing block or list")?;
                    return Ok(Ast::Infix(i, f, [x, y], Some(trailing.into())));
                }
                _ => return Ok(Ast::Infix(i, f, [x, y], None)),
            },
            Ast::Prefix(i, f, args) => (i, f, Some(args)),
            Ast::Str(i, s) => (i, Box::new(Ast::Str(i, s)), None),
            Ast::Var(i, v) => (i, Box::new(Ast::Var(i, v)), None),
            expr => return Ok(expr),
        };
        while let Some((_, Tok::Sep('[' | '{'))) = self.toks.peek() {
            args.get_or_insert_with(Vec::new).push(self.value("trailing [...] or {...}")?);
        }
        let mut kw_args = vec![];
        while let Some((i, Tok::Key(k))) = self.toks.peek().copied() {
            self.toks.next();
            kw_args.push(Ast::Tuple(i, vec![Ast::Str(i, k), self.infix("keyword argument")?]));
        }
        if let Some(Ast::Tuple(i, _)) = kw_args.first() {
            args.get_or_insert_with(Vec::new).push(Ast::List(*i, kw_args));
        }
        match args {
            None => Ok(*expr),
            Some(args) => Ok(Ast::Prefix(i, expr, args)),
        }
    }

    fn infix(&mut self, expected: &str) -> Result<Ast<'c>, (Pos, String)> {
        let mut x = self.prefix(expected)?;
        let Some((i, Tok::Var(f))) = self.toks.peek().copied() else {
            return Ok(x);
        };
        x = match x {
            Ast::Tuple(_, mut elems) if elems.len() == 1 => elems.pop().unwrap(),
            x => x,
        };
        while let Some((j, Tok::Var(g))) = self.toks.next_if(|(_, t)| matches!(t, Tok::Var(_))) {
            if f != g {
                return Err((j, format!("Expected infix '{f}', found '{g}'")));
            }
            let y = match self.prefix("infix argument")? {
                Ast::Tuple(_, mut elems) if elems.len() == 1 => elems.pop().unwrap(),
                y => y,
            };
            x = Ast::Infix(i, f, [x.into(), y.into()], None);
        }
        Ok(x)
    }

    fn prefix(&mut self, expected: &str) -> Result<Ast<'c>, (Pos, String)> {
        let mut expr = self.value(expected)?;
        while let Some((_, _)) = self.toks.next_if(|(_, t)| *t == Tok::Sep('(')) {
            let i = expr.pos();
            let args = self.exprs("function arguments", Some(Tok::Sep(')')))?;
            expr = Ast::Prefix(i, Box::new(expr), args);
        }
        Ok(expr)
    }

    fn value(&mut self, expected: &str) -> Result<Ast<'c>, (Pos, String)> {
        match self.toks.next() {
            Some((i, Tok::Sep('('))) => {
                Ok(Ast::Tuple(i, self.exprs("tuple elements after '('", Some(Tok::Sep(')')))?))
            }
            Some((i, Tok::Sep('['))) => {
                Ok(Ast::List(i, self.exprs("list elements", Some(Tok::Sep(']')))?))
            }
            Some((i, Tok::Sep('{'))) => {
                Ok(Ast::Block(i, self.exprs("block body", Some(Tok::Sep('}')))?))
            }
            Some((i, Tok::Var(s))) => Ok(Ast::Var(i, s)),
            Some((i, Tok::Str(s))) => Ok(Ast::Str(i, s)),
            Some((i, Tok::Int(n))) => Ok(Ast::Int(i, n)),
            Some((i, t)) => Err((i, format!("Expected {expected}, found {t}"))),
            None => Err((self.end_pos, format!("Expected {expected}"))),
        }
    }

    fn exprs(&mut self, exp: &str, until: Option<Tok<'c>>) -> Result<Vec<Ast<'c>>, (Pos, String)> {
        let mut exprs = vec![];
        let mut last_sep = Some((self.end_pos, Tok::Sep(',')));
        loop {
            match (last_sep, self.toks.peek(), until) {
                (_, None, None) => return Ok(exprs),
                (_, Some((_, t)), Some(until)) if *t == until => {
                    self.toks.next();
                    return Ok(exprs);
                }
                (_, Some((_, Tok::Sep(',' | '\n'))), _) => last_sep = self.toks.next(),
                (_, None, Some(until)) => {
                    return Err((self.end_pos, format!("Expected {exp} to end with {until}")));
                }
                (None, Some((i, t)), None) => {
                    return Err((*i, format!("Expected ',' or '\\n', found {t}")));
                }
                (None, Some((i, t)), Some(until)) => {
                    return Err((*i, format!("Expected ',' or '\\n' or {until}, found {t}")));
                }
                (Some(_), Some(_), _) => {
                    exprs.push(self.expr(exp)?);
                    last_sep = None;
                }
            }
        }
    }
}

pub fn parse(code: &str) -> Result<Vec<Ast<'_>>, String> {
    Parser { end_pos: Pos(code.len()), toks: scan(code).into_iter().peekable() }
        .exprs("an expression", None)
        .map_err(|(pos, msg)| format!("{msg} at {}", pos.line_in(code)))
}

pub fn format(code: &str) -> Result<String, String> {
    let exprs = parse(code)?;
    Ok(Ast::Block(Pos(0), exprs).pretty())
}

// --- Bytecode ---

use std::rc::Rc;

type StrId = usize;
type EffId = usize;

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    Tag,
    Fields,
    Head,
    Tail,
    IsEmpty,
    Cons,
    Len,
    Panic,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Var,
    IntLit(i64),
    BoolLit(bool),
    UnitLit,
    Tag(StrId, Vec<PatField>),
}

#[derive(Debug, Clone)]
pub enum PatField {
    Bind,
    Wildcard,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
enum Op {
    PushInt(i64),
    PushBool(bool),
    PushUnit,
    PushStr(StrId),
    LoadVar(usize),
    StoreEnv,
    PopEnv(u16),
    MakeTagged(StrId, u16),
    MakeList(u16),
    MakeClosure(u8, usize),
    MakeRecClosure(u8, usize),
    Call(u8),
    Return,
    Jump(usize),
    JumpIfFalse(usize),
    Pop,
    BinOp(BinOp),
    UnaryOp(UnaryOp),
    Builtin(Builtin, u8),
    Effect(EffId, u8),
    SetupTry(usize),
    PushHandler(EffId),
    CleanupTry,
    TestMatch(Pattern, usize),
    MatchFail,
    Halt,
}

// --- Compiler (AST → bytecode) ---

struct Compiler {
    code: Vec<Op>,
    strings: Vec<String>,
    effects: Vec<String>,
}

impl Compiler {
    fn new() -> Self {
        Compiler { code: vec![], strings: vec![], effects: vec![] }
    }

    fn emit(&mut self, op: Op) -> usize {
        let addr = self.code.len();
        self.code.push(op);
        addr
    }

    fn intern_str(&mut self, s: &str) -> StrId {
        let processed = Self::process_escapes(s);
        if let Some(i) = self.strings.iter().position(|x| x == &processed) {
            i
        } else {
            let i = self.strings.len();
            self.strings.push(processed);
            i
        }
    }

    fn process_escapes(s: &str) -> String {
        if !s.contains('\\') {
            return s.to_string();
        }
        let mut result = String::with_capacity(s.len());
        let mut chars = s.chars();
        while let Some(c) = chars.next() {
            if c == '\\' {
                match chars.next() {
                    Some('n') => result.push('\n'),
                    Some('"') => result.push('"'),
                    Some(other) => {
                        result.push('\\');
                        result.push(other);
                    }
                    None => result.push('\\'),
                }
            } else {
                result.push(c);
            }
        }
        result
    }

    fn intern_effect(&mut self, name: &str) -> EffId {
        if let Some(i) = self.effects.iter().position(|x| x == name) {
            i
        } else {
            let i = self.effects.len();
            self.effects.push(name.to_string());
            i
        }
    }

    fn compile_program(&mut self, exprs: &[Ast<'_>]) -> Result<(), (Pos, String)> {
        self.compile_block(exprs, &mut vec![])
    }

    fn compile_block(
        &mut self,
        exprs: &[Ast<'_>],
        scope: &mut Vec<String>,
    ) -> Result<(), (Pos, String)> {
        if exprs.is_empty() {
            self.emit(Op::PushUnit);
            return Ok(());
        }
        let scope_base = scope.len();
        for (i, expr) in exprs.iter().enumerate() {
            let is_last = i == exprs.len() - 1;

            if let Some((name, params_ast, body_ast)) = self.try_fn_def(expr) {
                let name_str = name.to_string();
                let params = Self::extract_params(params_ast)?;
                self.compile_closure(Some(&name_str), &params, body_ast, scope)?;
                scope.push(name_str);
                self.emit(Op::StoreEnv);
                if is_last {
                    self.emit(Op::LoadVar(0));
                }
            } else if let Some((name, value_ast)) = Self::try_assign(expr) {
                self.compile_expr(value_ast, scope)?;
                scope.push(name.to_string());
                self.emit(Op::StoreEnv);
                if is_last {
                    self.emit(Op::LoadVar(0));
                }
            } else {
                self.compile_expr(expr, scope)?;
                if !is_last {
                    self.emit(Op::Pop);
                }
            }
        }
        let n_bindings = scope.len() - scope_base;
        if n_bindings > 0 {
            self.emit(Op::PopEnv(n_bindings as u16));
        }
        scope.truncate(scope_base);
        Ok(())
    }

    fn compile_closure(
        &mut self,
        rec_name: Option<&str>,
        params: &[String],
        body: &Ast<'_>,
        scope: &mut Vec<String>,
    ) -> Result<(), (Pos, String)> {
        let arity = params.len() as u8;
        let is_rec = rec_name.is_some();
        let closure_addr = if is_rec {
            self.emit(Op::MakeRecClosure(arity, 0))
        } else {
            self.emit(Op::MakeClosure(arity, 0))
        };
        let jump_addr = self.emit(Op::Jump(0));
        let fn_addr = self.code.len();

        let mut fn_scope = scope.clone();
        if let Some(name) = rec_name {
            fn_scope.push(name.to_string());
        }
        for p in params {
            fn_scope.push(p.clone());
        }
        self.compile_thunk(body, &mut fn_scope)?;
        self.emit(Op::Return);

        let after_fn = self.code.len();
        match &mut self.code[closure_addr] {
            Op::MakeClosure(_, addr) | Op::MakeRecClosure(_, addr) => *addr = fn_addr,
            _ => unreachable!(),
        }
        self.code[jump_addr] = Op::Jump(after_fn);
        Ok(())
    }

    fn try_fn_def<'a, 'co>(
        &self,
        expr: &'a Ast<'co>,
    ) -> Option<(&'co str, &'a Ast<'co>, &'a Ast<'co>)> {
        if let Ast::Infix(_, "=", [lhs, rhs], Some(trailing)) = expr {
            if let Ast::Var(_, name) = lhs.as_ref() {
                if matches!(trailing.as_ref(), Ast::Block(..)) {
                    return Some((name, rhs.as_ref(), trailing.as_ref()));
                }
            }
        }
        None
    }

    fn try_assign<'a, 'co>(expr: &'a Ast<'co>) -> Option<(&'co str, &'a Ast<'co>)> {
        if let Ast::Infix(_, "=", [lhs, rhs], None) = expr {
            if let Ast::Var(_, name) = lhs.as_ref() {
                return Some((name, rhs.as_ref()));
            }
        }
        None
    }

    fn extract_params(ast: &Ast<'_>) -> Result<Vec<String>, (Pos, String)> {
        match ast {
            Ast::List(_, elems) | Ast::Tuple(_, elems) => {
                let mut params = vec![];
                for e in elems {
                    if let Ast::Var(_, name) = e {
                        params.push(name.to_string());
                    } else {
                        return Err((e.pos(), format!("Expected parameter name, got {e}")));
                    }
                }
                Ok(params)
            }
            Ast::Var(_, name) => Ok(vec![name.to_string()]),
            _ => Err((ast.pos(), format!("Expected parameter list, got {ast}"))),
        }
    }

    fn compile_expr(
        &mut self,
        expr: &Ast<'_>,
        scope: &mut Vec<String>,
    ) -> Result<(), (Pos, String)> {
        match expr {
            Ast::Int(_, n) => {
                self.emit(Op::PushInt(*n));
            }
            Ast::Str(_, s) => match *s {
                "True" => {
                    self.emit(Op::PushBool(true));
                }
                "False" => {
                    self.emit(Op::PushBool(false));
                }
                "Unit" => {
                    self.emit(Op::PushUnit);
                }
                _ => {
                    let id = self.intern_str(s);
                    self.emit(Op::MakeTagged(id, 0));
                }
            },
            Ast::Var(pos, name) => {
                if name.ends_with('!') {
                    return Err((
                        *pos,
                        format!("Effect '{name}' must be called, not used as a variable"),
                    ));
                }
                if let Some(idx) = scope.iter().rev().position(|n| n == name) {
                    self.emit(Op::LoadVar(idx));
                } else {
                    return Err((*pos, format!("Unbound variable: {name}")));
                }
            }
            Ast::List(_, elems) => {
                for e in elems {
                    self.compile_expr(e, scope)?;
                }
                self.emit(Op::MakeList(elems.len() as u16));
            }
            Ast::Tuple(_, elems) => {
                if elems.len() == 1 {
                    self.compile_expr(&elems[0], scope)?;
                } else {
                    for e in elems {
                        self.compile_expr(e, scope)?;
                    }
                    self.emit(Op::MakeList(elems.len() as u16));
                }
            }
            Ast::Block(_, _) => {
                self.compile_closure(None, &[], expr, scope)?;
            }
            Ast::Prefix(_, func, args) => {
                self.compile_call(func, args, scope)?;
            }
            Ast::Infix(pos, op, [lhs, rhs], trailing) => {
                self.compile_infix(*pos, op, lhs, rhs, trailing.as_deref(), scope)?;
            }
        }
        Ok(())
    }

    fn compile_call(
        &mut self,
        func: &Ast<'_>,
        args: &[Ast<'_>],
        scope: &mut Vec<String>,
    ) -> Result<(), (Pos, String)> {
        if let Ast::Var(_, name) = func {
            if name.ends_with('!') {
                let effect_name = &name[..name.len() - 1];
                let eff_id = self.intern_effect(effect_name);
                for a in args {
                    self.compile_expr(a, scope)?;
                }
                self.emit(Op::Effect(eff_id, args.len() as u8));
                return Ok(());
            }

            match *name {
                "if" => return self.compile_if(func.pos(), args, scope),
                "match" => return self.compile_match(func.pos(), args, scope),
                "try" => return self.compile_try(func.pos(), args, scope),
                "not" => {
                    if args.len() != 1 {
                        return Err((func.pos(), "not() expects 1 argument".into()));
                    }
                    self.compile_expr(&args[0], scope)?;
                    self.emit(Op::UnaryOp(UnaryOp::Not));
                    return Ok(());
                }
                "tag" | "fields" | "head" | "tail" | "is_empty" | "len" | "panic" => {
                    if args.len() != 1 {
                        return Err((func.pos(), format!("{name}() expects 1 argument")));
                    }
                    let builtin = match *name {
                        "tag" => Builtin::Tag,
                        "fields" => Builtin::Fields,
                        "head" => Builtin::Head,
                        "tail" => Builtin::Tail,
                        "is_empty" => Builtin::IsEmpty,
                        "len" => Builtin::Len,
                        "panic" => Builtin::Panic,
                        _ => unreachable!(),
                    };
                    self.compile_expr(&args[0], scope)?;
                    self.emit(Op::Builtin(builtin, 1));
                    return Ok(());
                }
                "cons" => {
                    if args.len() != 2 {
                        return Err((func.pos(), "cons() expects 2 arguments".into()));
                    }
                    self.compile_expr(&args[0], scope)?;
                    self.compile_expr(&args[1], scope)?;
                    self.emit(Op::Builtin(Builtin::Cons, 2));
                    return Ok(());
                }
                _ => {}
            }
        }

        if let Ast::Str(_, tag_name) = func {
            let tag_id = self.intern_str(tag_name);
            for a in args {
                self.compile_expr(a, scope)?;
            }
            self.emit(Op::MakeTagged(tag_id, args.len() as u16));
            return Ok(());
        }

        self.compile_expr(func, scope)?;
        for a in args {
            self.compile_expr(a, scope)?;
        }
        self.emit(Op::Call(args.len() as u8));
        Ok(())
    }

    fn compile_if(
        &mut self,
        pos: Pos,
        args: &[Ast<'_>],
        scope: &mut Vec<String>,
    ) -> Result<(), (Pos, String)> {
        // if (cond) { then } else: { else }
        let [cond, then_branch, Ast::List(_, kw_args)] = args else {
            return Err((pos, "if expects: if (cond) { then } else: { else }".into()));
        };
        let else_branch = kw_args
            .iter()
            .find_map(|kw| {
                if let Ast::Tuple(_, elems) = kw {
                    if let [Ast::Str(_, "else"), body] = elems.as_slice() {
                        return Some(body);
                    }
                }
                None
            })
            .ok_or_else(|| (pos, "if expects else: branch".to_string()))?;
        self.compile_expr(cond, scope)?;
        let jf = self.emit(Op::JumpIfFalse(0));
        self.compile_thunk(then_branch, scope)?;
        let jend = self.emit(Op::Jump(0));
        let else_addr = self.code.len();
        self.compile_thunk(else_branch, scope)?;
        let end_addr = self.code.len();
        self.code[jf] = Op::JumpIfFalse(else_addr);
        self.code[jend] = Op::Jump(end_addr);
        Ok(())
    }

    fn compile_thunk(
        &mut self,
        expr: &Ast<'_>,
        scope: &mut Vec<String>,
    ) -> Result<(), (Pos, String)> {
        match expr {
            Ast::Block(_, exprs) => self.compile_block(exprs, scope),
            other => self.compile_expr(other, scope),
        }
    }

    fn compile_match(
        &mut self,
        pos: Pos,
        args: &[Ast<'_>],
        scope: &mut Vec<String>,
    ) -> Result<(), (Pos, String)> {
        let (scrutinee_ast, arms_ast) = if args.len() == 2 {
            (&args[0], &args[1])
        } else {
            return Err((pos, "match() expects 2 arguments: value, [arms]".into()));
        };

        self.compile_expr(scrutinee_ast, scope)?;

        let arms_list = match arms_ast {
            Ast::List(_, arms) => arms,
            _ => {
                return Err((
                    arms_ast.pos(),
                    "match() second argument must be a list of arms".into(),
                ));
            }
        };

        // First pass: emit TestMatch instructions, collect arm info
        let mut arm_info: Vec<(usize, Vec<String>)> = vec![];
        for arm in arms_list {
            let (pat_ast, _) = Self::parse_match_arm(arm)?;
            let (pattern, names) = self.compile_pattern(pat_ast)?;
            let test_addr = self.emit(Op::TestMatch(pattern, 0));
            arm_info.push((test_addr, names));
        }
        self.emit(Op::MatchFail);

        // Second pass: emit arm bodies
        let mut end_jumps = vec![];
        for (i, arm) in arms_list.iter().enumerate() {
            let (_, body_ast) = Self::parse_match_arm(arm)?;
            let body_addr = self.code.len();
            let (test_addr, ref names) = arm_info[i];
            match &mut self.code[test_addr] {
                Op::TestMatch(_, addr) => *addr = body_addr,
                _ => unreachable!(),
            }
            for name in names {
                scope.push(name.clone());
            }
            self.compile_thunk(body_ast, scope)?;
            for _ in 0..names.len() {
                scope.pop();
            }
            if !names.is_empty() {
                self.emit(Op::PopEnv(names.len() as u16));
            }
            end_jumps.push(self.emit(Op::Jump(0)));
        }

        let end_addr = self.code.len();
        for j in end_jumps {
            self.code[j] = Op::Jump(end_addr);
        }
        Ok(())
    }

    fn parse_match_arm<'a, 'co>(
        arm: &'a Ast<'co>,
    ) -> Result<(&'a Ast<'co>, &'a Ast<'co>), (Pos, String)> {
        match arm {
            Ast::Infix(_, "->" | "=>", [p, b], None) => Ok((p.as_ref(), b.as_ref())),
            _ => Err((arm.pos(), format!("Expected pattern -> body in match arm, got {arm}"))),
        }
    }

    fn compile_pattern(&mut self, pat: &Ast<'_>) -> Result<(Pattern, Vec<String>), (Pos, String)> {
        match pat {
            Ast::Var(_, "_") => Ok((Pattern::Wildcard, vec![])),
            Ast::Var(_, name) => Ok((Pattern::Var, vec![name.to_string()])),
            Ast::Int(_, n) => Ok((Pattern::IntLit(*n), vec![])),
            Ast::Str(_, "True") => Ok((Pattern::BoolLit(true), vec![])),
            Ast::Str(_, "False") => Ok((Pattern::BoolLit(false), vec![])),
            Ast::Str(_, "Unit") => Ok((Pattern::UnitLit, vec![])),
            Ast::Str(_, tag_name) => {
                let tag_id = self.intern_str(tag_name);
                Ok((Pattern::Tag(tag_id, vec![]), vec![]))
            }
            Ast::Prefix(_, func, args) => {
                if let Ast::Str(_, tag_name) = func.as_ref() {
                    let tag_id = self.intern_str(tag_name);
                    let mut fields = vec![];
                    let mut names = vec![];
                    for arg in args {
                        match arg {
                            Ast::Var(_, "_") => fields.push(PatField::Wildcard),
                            Ast::Var(_, name) => {
                                fields.push(PatField::Bind);
                                names.push(name.to_string());
                            }
                            _ => {
                                return Err((
                                    arg.pos(),
                                    format!("Only variable names allowed in patterns, got {arg}"),
                                ));
                            }
                        }
                    }
                    Ok((Pattern::Tag(tag_id, fields), names))
                } else {
                    Err((func.pos(), format!("Expected tag name in pattern, got {func}")))
                }
            }
            _ => Err((pat.pos(), format!("Unsupported pattern: {pat}"))),
        }
    }

    fn compile_try(
        &mut self,
        pos: Pos,
        args: &[Ast<'_>],
        scope: &mut Vec<String>,
    ) -> Result<(), (Pos, String)> {
        if args.len() != 2 {
            return Err((pos, "try expects: try { body } catch: [handlers]".into()));
        }
        let handlers_ast = match &args[1] {
            Ast::List(_, kw_args) => {
                let catch = kw_args.iter().find_map(|kw| {
                    if let Ast::Tuple(_, elems) = kw {
                        if let [Ast::Str(_, "catch"), Ast::List(_, handlers)] = elems.as_slice() {
                            return Some(handlers);
                        }
                    }
                    None
                });
                match catch {
                    Some(handlers) => handlers,
                    None => return Err((pos, "try expects catch: [handlers]".into())),
                }
            }
            _ => return Err((pos, "try expects: try { body } catch: [handlers]".into())),
        };

        let setup_addr = self.emit(Op::SetupTry(0));

        for h in handlers_ast {
            let (eff_name, handler_ast) = self.parse_handler(h)?;
            let eff_id = self.intern_effect(&eff_name);
            self.compile_expr(handler_ast, scope)?;
            self.emit(Op::PushHandler(eff_id));
        }

        self.compile_thunk(&args[0], scope)?;
        self.emit(Op::CleanupTry);

        let after_addr = self.code.len();
        self.code[setup_addr] = Op::SetupTry(after_addr);
        Ok(())
    }

    fn parse_handler<'a, 'co>(
        &self,
        ast: &'a Ast<'co>,
    ) -> Result<(String, &'a Ast<'co>), (Pos, String)> {
        // Keyword arg syntax: Tuple(Str("read!"), handler_expr)
        if let Ast::Tuple(_, elems) = ast {
            if let [Ast::Str(_, name), handler] = elems.as_slice() {
                if name.ends_with('!') {
                    return Ok((name[..name.len() - 1].to_string(), handler));
                }
            }
        }
        Err((ast.pos(), format!("Expected effect!: handler, got {ast}")))
    }

    fn compile_infix(
        &mut self,
        pos: Pos,
        op: &str,
        lhs: &Ast<'_>,
        rhs: &Ast<'_>,
        trailing: Option<&Ast<'_>>,
        scope: &mut Vec<String>,
    ) -> Result<(), (Pos, String)> {
        match op {
            "=" => Err((pos, "Assignments (=) must be inside a { ... } block".into())),
            "=>" => {
                let params = Self::extract_params(lhs)?;
                let body = trailing.unwrap_or(rhs);
                self.compile_closure(None, &params, body, scope)?;
                Ok(())
            }
            "->" => Err((pos, "-> can only be used inside match arms".into())),
            "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                self.compile_expr(lhs, scope)?;
                self.compile_expr(rhs, scope)?;
                let binop = match op {
                    "+" => BinOp::Add,
                    "-" => BinOp::Sub,
                    "*" => BinOp::Mul,
                    "/" => BinOp::Div,
                    "%" => BinOp::Mod,
                    "==" => BinOp::Eq,
                    "!=" => BinOp::Ne,
                    "<" => BinOp::Lt,
                    ">" => BinOp::Gt,
                    "<=" => BinOp::Le,
                    ">=" => BinOp::Ge,
                    _ => unreachable!(),
                };
                self.emit(Op::BinOp(binop));
                Ok(())
            }
            _ => Err((pos, format!("Unknown infix operator: {op}"))),
        }
    }
}

pub fn compile(code: &str) -> Result<Program, String> {
    let ast = parse(code)?;
    let mut compiler = Compiler::new();
    compiler
        .compile_program(&ast)
        .map_err(|(pos, msg)| format!("{msg} at {}", pos.line_in(code)))?;
    compiler.emit(Op::Halt);
    Ok(Program {
        code: Rc::from(compiler.code),
        strings: compiler.strings,
        effects: compiler.effects,
    })
}

// --- Values + VM ---

#[derive(Debug, Clone)]
pub struct Program {
    code: Rc<[Op]>,
    pub strings: Vec<String>,
    pub effects: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ContData {
    ip: usize,
    env: Vec<Value>,
    stack: Vec<Value>,
    frames: Vec<Frame>,
}

#[derive(Debug)]
pub enum ListNode {
    Nil,
    Cons(Value, Rc<ListNode>),
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Str(StrId),
    Tagged(StrId, Vec<Value>),
    List(Rc<ListNode>),
    Closure(u8, usize, Rc<Vec<Value>>),
    RecClosure(u8, usize, Rc<Vec<Value>>),
    Continuation(Rc<ContData>),
    Unit,
}

impl Value {
    pub fn nil() -> Self {
        Value::List(Rc::new(ListNode::Nil))
    }

    pub fn list_from_vec(items: Vec<Value>) -> Self {
        let mut list = Rc::new(ListNode::Nil);
        for val in items.into_iter().rev() {
            list = Rc::new(ListNode::Cons(val, list));
        }
        Value::List(list)
    }

    pub fn display(&self, prog: &Program) -> String {
        self.display_with(&prog.strings)
    }

    fn display_with(&self, strings: &[String]) -> String {
        match self {
            Value::Int(n) => n.to_string(),
            Value::Bool(true) => "True".to_string(),
            Value::Bool(false) => "False".to_string(),
            Value::Unit => "Unit".to_string(),
            Value::Str(id) => {
                if *id < strings.len() {
                    format!("\"{}\"", strings[*id])
                } else {
                    format!("<str:{id}>")
                }
            }
            Value::Tagged(tag, fields) => {
                let name = if *tag < strings.len() { &strings[*tag] } else { "<unknown>" };
                if fields.is_empty() {
                    name.to_string()
                } else {
                    let args: Vec<_> = fields.iter().map(|f| f.display_with(strings)).collect();
                    format!("{name}({})", args.join(", "))
                }
            }
            Value::List(node) => {
                let mut items = vec![];
                let mut cur = node.as_ref();
                while let ListNode::Cons(head, tail) = cur {
                    items.push(head.display_with(strings));
                    cur = tail.as_ref();
                }
                format!("[{}]", items.join(", "))
            }
            Value::Closure(arity, _, _) | Value::RecClosure(arity, _, _) => {
                format!("<fn/{arity}>")
            }
            Value::Continuation(..) => "<continuation>".to_string(),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Unit, Value::Unit) => true,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Tagged(t1, f1), Value::Tagged(t2, f2)) => t1 == t2 && f1 == f2,
            (Value::List(a), Value::List(b)) => {
                let mut ca = a.as_ref();
                let mut cb = b.as_ref();
                loop {
                    match (ca, cb) {
                        (ListNode::Nil, ListNode::Nil) => return true,
                        (ListNode::Cons(ha, ta), ListNode::Cons(hb, tb)) => {
                            if ha != hb {
                                return false;
                            }
                            ca = ta.as_ref();
                            cb = tb.as_ref();
                        }
                        _ => return false,
                    }
                }
            }
            _ => false,
        }
    }
}

// --- Stack-based VM ---

#[derive(Debug, Clone)]
pub struct VM {
    code: Rc<[Op]>,
    pub strings: Vec<String>,
    pub effects: Vec<String>,
    stack: Vec<Value>,
    env: Vec<Value>,
    frames: Vec<Frame>,
    handlers: Vec<Vec<HandlerEntry>>,
    try_markers: Vec<TryMarker>,
    ip: usize,
}

#[derive(Debug, Clone)]
struct Frame {
    return_ip: usize,
    saved_env: Vec<Value>,
}

#[derive(Debug, Clone)]
struct HandlerEntry {
    closure: Value,
    try_depth: usize,
}

#[derive(Debug, Clone)]
struct TryMarker {
    after_addr: usize,
    saved_stack_len: usize,
    saved_env_len: usize,
    saved_frames_len: usize,
    saved_handler_lens: Vec<usize>,
    is_resume_delimiter: bool,
}

#[derive(Debug)]
pub enum VMResult {
    Done(Value, Vec<String>),
    Effect { name: String, args: Vec<Value>, continuation: VM },
}

enum RunResult {
    Done(Value, Vec<String>),
    Effect(String, Vec<Value>, VM),
    Error(String),
}

impl RunResult {
    fn into_vm_result(self) -> Result<VMResult, String> {
        match self {
            RunResult::Done(val, strings) => Ok(VMResult::Done(val, strings)),
            RunResult::Effect(name, args, cont) => {
                Ok(VMResult::Effect { name, args, continuation: cont })
            }
            RunResult::Error(e) => Err(e),
        }
    }
}

fn unwrap_rc<T: Clone>(rc: Rc<T>) -> T {
    Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone())
}

impl VM {
    fn from_program(program: &Program) -> Self {
        let num_effects = program.effects.len().max(1);
        VM {
            code: program.code.clone(),
            strings: program.strings.clone(),
            effects: program.effects.clone(),
            stack: vec![],
            env: vec![],
            frames: vec![],
            handlers: vec![vec![]; num_effects],
            try_markers: vec![],
            ip: 0,
        }
    }

    fn run(mut self) -> RunResult {
        loop {
            if self.ip >= self.code.len() {
                return RunResult::Error("IP out of bounds".into());
            }
            let code = Rc::clone(&self.code);
            let op = &code[self.ip];
            self.ip += 1;
            match *op {
                Op::Halt => {
                    return match self.stack.pop() {
                        Some(val) => RunResult::Done(val, self.strings),
                        None => RunResult::Error("Empty stack at Halt".into()),
                    };
                }
                Op::PushInt(n) => self.stack.push(Value::Int(n)),
                Op::PushBool(b) => self.stack.push(Value::Bool(b)),
                Op::PushUnit => self.stack.push(Value::Unit),
                Op::PushStr(id) => self.stack.push(Value::Str(id)),
                Op::LoadVar(idx) => {
                    let pos = match self.env.len().checked_sub(1 + idx) {
                        Some(p) => p,
                        None => {
                            return RunResult::Error(format!(
                                "LoadVar({idx}): env size {}",
                                self.env.len()
                            ));
                        }
                    };
                    self.stack.push(self.env[pos].clone());
                }
                Op::StoreEnv => {
                    let val = self.stack.pop().unwrap();
                    self.env.push(val);
                }
                Op::PopEnv(n) => {
                    let new_len = self.env.len() - n as usize;
                    self.env.truncate(new_len);
                }
                Op::Pop => {
                    self.stack.pop();
                }
                Op::MakeTagged(tag, n) => {
                    let n = n as usize;
                    let fields = self.stack.split_off(self.stack.len() - n);
                    self.stack.push(Value::Tagged(tag, fields));
                }
                Op::MakeList(n) => {
                    let items = self.stack.split_off(self.stack.len() - n as usize);
                    self.stack.push(Value::list_from_vec(items));
                }
                Op::MakeClosure(arity, addr) => {
                    self.stack.push(Value::Closure(arity, addr, Rc::new(self.env.clone())));
                }
                Op::MakeRecClosure(arity, addr) => {
                    self.stack.push(Value::RecClosure(arity, addr, Rc::new(self.env.clone())));
                }
                Op::Jump(addr) => {
                    self.ip = addr;
                }
                Op::JumpIfFalse(addr) => match self.stack.pop() {
                    Some(Value::Bool(false)) => {
                        self.ip = addr;
                    }
                    Some(Value::Bool(true)) => {}
                    _ => return RunResult::Error("JumpIfFalse: expected boolean".into()),
                },
                Op::BinOp(op) => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    match self.binop(op, a, b) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return RunResult::Error(e),
                    }
                }
                Op::UnaryOp(op) => {
                    let a = self.stack.pop().unwrap();
                    match (op, &a) {
                        (UnaryOp::Not, Value::Bool(b)) => self.stack.push(Value::Bool(!b)),
                        (UnaryOp::Neg, Value::Int(n)) => self.stack.push(Value::Int(-n)),
                        _ => return RunResult::Error("unary op: type error".into()),
                    }
                }
                Op::Builtin(builtin, n) => {
                    let n = n as usize;
                    let args = self.stack.split_off(self.stack.len() - n);
                    match self.eval_builtin(builtin, args) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return RunResult::Error(e),
                    }
                }
                Op::Call(n) => {
                    let n = n as usize;
                    let args = self.stack.split_off(self.stack.len() - n);
                    let func = self.stack.pop().unwrap();
                    match func {
                        Value::Closure(arity, addr, captured_env) => {
                            if args.len() != arity as usize {
                                return RunResult::Error(format!(
                                    "Function expects {} args, got {}",
                                    arity,
                                    args.len()
                                ));
                            }
                            self.frames.push(Frame {
                                return_ip: self.ip,
                                saved_env: std::mem::take(&mut self.env),
                            });
                            self.env = unwrap_rc(captured_env);
                            self.env.extend(args);
                            self.ip = addr;
                        }
                        Value::RecClosure(arity, addr, captured_env) => {
                            if args.len() != arity as usize {
                                return RunResult::Error(format!(
                                    "Function expects {} args, got {}",
                                    arity,
                                    args.len()
                                ));
                            }
                            self.frames.push(Frame {
                                return_ip: self.ip,
                                saved_env: std::mem::take(&mut self.env),
                            });
                            let self_val = Value::RecClosure(arity, addr, Rc::clone(&captured_env));
                            self.env = unwrap_rc(captured_env);
                            self.env.push(self_val);
                            self.env.extend(args);
                            self.ip = addr;
                        }
                        Value::Continuation(cont_rc) => {
                            if args.len() != 1 {
                                return RunResult::Error("Continuation expects 1 argument".into());
                            }
                            let val = args.into_iter().next().unwrap();
                            let cont_data = unwrap_rc(cont_rc);

                            // Push a return frame so the result comes back
                            // to the resume call site when the delimited
                            // computation finishes (via CleanupTry with
                            // is_resume_delimiter).
                            self.frames.push(Frame {
                                return_ip: self.ip,
                                saved_env: std::mem::take(&mut self.env),
                            });
                            let frames_base = self.frames.len();
                            let stack_base = self.stack.len();

                            // Move the continuation's delta onto current VM.
                            self.stack.extend(cont_data.stack);
                            self.env = cont_data.env;
                            self.frames.extend(cont_data.frames);

                            // Push a resume-delimiter try marker. When
                            // CleanupTry pops this, it returns the result
                            // to the resume caller via Frame above.
                            self.try_markers.push(TryMarker {
                                after_addr: 0,
                                saved_stack_len: stack_base,
                                saved_env_len: 0,
                                saved_frames_len: frames_base,
                                saved_handler_lens: self.handlers.iter().map(|h| h.len()).collect(),
                                is_resume_delimiter: true,
                            });

                            self.ip = cont_data.ip;
                            self.stack.push(val);
                        }
                        _ => {
                            return RunResult::Error(format!(
                                "Cannot call non-function: {}",
                                func.display_with(&self.strings)
                            ));
                        }
                    }
                }
                Op::Return => {
                    let result = self.stack.pop().unwrap();
                    let frame = self.frames.pop().unwrap();
                    self.env = frame.saved_env;
                    self.ip = frame.return_ip;
                    self.stack.push(result);
                }
                Op::SetupTry(after_addr) => {
                    self.try_markers.push(TryMarker {
                        after_addr,
                        saved_stack_len: self.stack.len(),
                        saved_env_len: self.env.len(),
                        saved_frames_len: self.frames.len(),
                        saved_handler_lens: self.handlers.iter().map(|h| h.len()).collect(),
                        is_resume_delimiter: false,
                    });
                }
                Op::PushHandler(eff_id) => {
                    let closure = self.stack.pop().unwrap();
                    let try_depth = self.try_markers.len() - 1;
                    while self.handlers.len() <= eff_id {
                        self.handlers.push(vec![]);
                    }
                    self.handlers[eff_id].push(HandlerEntry { closure, try_depth });
                }
                Op::CleanupTry => {
                    let marker = self.try_markers.pop().unwrap();
                    for (eff_id, &saved_len) in marker.saved_handler_lens.iter().enumerate() {
                        if eff_id < self.handlers.len() {
                            self.handlers[eff_id].truncate(saved_len);
                        }
                    }
                    if marker.is_resume_delimiter {
                        // Delimited computation finished — truncate
                        // stack/frames to composition base, then return
                        // the result to the resume() call site.
                        let result = self.stack.pop().unwrap();
                        self.stack.truncate(marker.saved_stack_len);
                        self.frames.truncate(marker.saved_frames_len);
                        let frame = self.frames.pop().unwrap();
                        self.env = frame.saved_env;
                        self.ip = frame.return_ip;
                        self.stack.push(result);
                    }
                }
                Op::Effect(eff_id, n) => {
                    let n = n as usize;
                    let args = self.stack.split_off(self.stack.len() - n);

                    let handler = if eff_id < self.handlers.len() {
                        self.handlers[eff_id].pop()
                    } else {
                        None
                    };

                    if let Some(entry) = handler {
                        let marker = self.try_markers[entry.try_depth].clone();

                        // Save the env prefix for the handler's return frame
                        // (replicates the old truncate-then-take behavior).
                        let env_base_len = marker.saved_env_len.min(self.env.len());
                        let handler_base_env = self.env[..env_base_len].to_vec();

                        // Capture the delta above the try marker (move, not clone).
                        let cont_data = ContData {
                            ip: self.ip,
                            env: std::mem::take(&mut self.env),
                            stack: self.stack.split_off(marker.saved_stack_len),
                            frames: self.frames.split_off(marker.saved_frames_len),
                        };
                        let continuation = Value::Continuation(Rc::new(cont_data));

                        // Rollback: try_markers and handlers to pre-try state.
                        self.try_markers.truncate(entry.try_depth);
                        for (i, &saved_len) in marker.saved_handler_lens.iter().enumerate() {
                            if i < self.handlers.len() {
                                self.handlers[i].truncate(saved_len);
                            }
                        }

                        let (arity, fn_addr, closure_env_rc) = match entry.closure {
                            Value::Closure(a, addr, env) => (a, addr, env),
                            _ => return RunResult::Error("Effect handler is not a closure".into()),
                        };
                        if (1 + args.len()) != arity as usize {
                            return RunResult::Error(format!(
                                "Handler expects {} args, got {}",
                                arity,
                                1 + args.len()
                            ));
                        }

                        self.frames.push(Frame {
                            return_ip: marker.after_addr,
                            saved_env: handler_base_env,
                        });
                        self.env = unwrap_rc(closure_env_rc);
                        self.env.push(continuation);
                        self.env.extend(args);
                        self.ip = fn_addr;
                    } else {
                        // Unhandled effect → return to host (move self, no clone)
                        let name = if eff_id < self.effects.len() {
                            self.effects[eff_id].clone()
                        } else {
                            format!("<effect:{eff_id}>")
                        };
                        return RunResult::Effect(name, args, self);
                    }
                }
                Op::TestMatch(ref pattern, target) => {
                    let scrutinee = self.stack.last().unwrap();
                    if let Some(bindings) = match_pattern(pattern, scrutinee) {
                        self.stack.pop();
                        self.env.extend(bindings);
                        self.ip = target;
                    }
                }
                Op::MatchFail => {
                    let val = self.stack.pop().unwrap_or(Value::Unit);
                    return RunResult::Error(format!(
                        "no matching pattern for {}",
                        val.display_with(&self.strings)
                    ));
                }
            }
        }
    }

    fn binop(&self, op: BinOp, a: Value, b: Value) -> Result<Value, String> {
        match op {
            BinOp::Eq => Ok(Value::Bool(a == b)),
            BinOp::Ne => Ok(Value::Bool(a != b)),
            _ => match (&a, &b) {
                (Value::Int(x), Value::Int(y)) => match op {
                    BinOp::Add => Ok(Value::Int(x + y)),
                    BinOp::Sub => Ok(Value::Int(x - y)),
                    BinOp::Mul => Ok(Value::Int(x * y)),
                    BinOp::Div => {
                        if *y == 0 {
                            return Err("Division by zero".into());
                        }
                        Ok(Value::Int(x / y))
                    }
                    BinOp::Mod => {
                        if *y == 0 {
                            return Err("Modulo by zero".into());
                        }
                        Ok(Value::Int(x % y))
                    }
                    BinOp::Lt => Ok(Value::Bool(x < y)),
                    BinOp::Gt => Ok(Value::Bool(x > y)),
                    BinOp::Le => Ok(Value::Bool(x <= y)),
                    BinOp::Ge => Ok(Value::Bool(x >= y)),
                    _ => unreachable!(),
                },
                _ => Err(format!(
                    "Arithmetic on non-integers: {} and {}",
                    a.display_with(&self.strings),
                    b.display_with(&self.strings)
                )),
            },
        }
    }

    fn eval_builtin(&self, builtin: Builtin, args: Vec<Value>) -> Result<Value, String> {
        match (builtin, args.as_slice()) {
            (Builtin::Tag, [val]) => match val {
                Value::Tagged(tag, _) => Ok(Value::Str(*tag)),
                Value::Int(_) => {
                    let id = self.strings.iter().position(|s| s == "Int");
                    Ok(Value::Str(id.unwrap_or(usize::MAX)))
                }
                Value::Bool(_) => {
                    let id = self.strings.iter().position(|s| s == "Bool");
                    Ok(Value::Str(id.unwrap_or(usize::MAX)))
                }
                Value::List(_) => {
                    let id = self.strings.iter().position(|s| s == "List");
                    Ok(Value::Str(id.unwrap_or(usize::MAX)))
                }
                _ => Err("tag: unsupported value type".into()),
            },
            (Builtin::Fields, [val]) => match val {
                Value::Tagged(_, fields) => Ok(Value::list_from_vec(fields.clone())),
                _ => Err("fields: expected tagged value".into()),
            },
            (Builtin::Head, [val]) => match val {
                Value::List(node) => match node.as_ref() {
                    ListNode::Cons(head, _) => Ok(head.clone()),
                    ListNode::Nil => Err("head: empty list".into()),
                },
                _ => Err("head: expected list".into()),
            },
            (Builtin::Tail, [val]) => match val {
                Value::List(node) => match node.as_ref() {
                    ListNode::Cons(_, tail) => Ok(Value::List(Rc::clone(tail))),
                    ListNode::Nil => Err("tail: empty list".into()),
                },
                _ => Err("tail: expected list".into()),
            },
            (Builtin::IsEmpty, [val]) => match val {
                Value::List(node) => Ok(Value::Bool(matches!(node.as_ref(), ListNode::Nil))),
                _ => Err("is_empty: expected list".into()),
            },
            (Builtin::Cons, [elem, list]) => match list {
                Value::List(tail) => {
                    Ok(Value::List(Rc::new(ListNode::Cons(elem.clone(), Rc::clone(tail)))))
                }
                _ => Err("cons: expected list".into()),
            },
            (Builtin::Len, [val]) => match val {
                Value::List(node) => {
                    let mut count = 0i64;
                    let mut cur = node.as_ref();
                    while let ListNode::Cons(_, tail) = cur {
                        count += 1;
                        cur = tail.as_ref();
                    }
                    Ok(Value::Int(count))
                }
                _ => Err("len: expected list".into()),
            },
            (Builtin::Panic, [val]) => Err(format!("panic: {}", val.display_with(&self.strings))),
            _ => Err("builtin: wrong number of arguments".into()),
        }
    }

    pub fn resume(mut self, value: Value) -> Result<VMResult, String> {
        self.stack.push(value);
        self.run().into_vm_result()
    }

    pub fn intern_str(&mut self, s: &str) -> StrId {
        intern(&mut self.strings, s)
    }
}

fn match_pattern(pattern: &Pattern, value: &Value) -> Option<Vec<Value>> {
    match pattern {
        Pattern::Wildcard => Some(vec![]),
        Pattern::Var => Some(vec![value.clone()]),
        Pattern::IntLit(n) => {
            if matches!(value, Value::Int(v) if v == n) {
                Some(vec![])
            } else {
                None
            }
        }
        Pattern::BoolLit(b) => {
            if matches!(value, Value::Bool(v) if v == b) {
                Some(vec![])
            } else {
                None
            }
        }
        Pattern::UnitLit => {
            if matches!(value, Value::Unit) {
                Some(vec![])
            } else {
                None
            }
        }
        Pattern::Tag(tag_id, fields) => match value {
            Value::Tagged(val_tag, val_fields) => {
                if val_tag != tag_id || val_fields.len() != fields.len() {
                    return None;
                }
                let mut bindings = vec![];
                for (field_pat, field_val) in fields.iter().zip(val_fields) {
                    match field_pat {
                        PatField::Bind => bindings.push(field_val.clone()),
                        PatField::Wildcard => {}
                    }
                }
                Some(bindings)
            }
            _ => None,
        },
    }
}

pub fn run_program(program: &Program) -> Result<VMResult, String> {
    VM::from_program(program).run().into_vm_result()
}

pub fn run(code: &str) -> Result<Value, String> {
    let program = compile(code)?;
    match run_program(&program)? {
        VMResult::Done(val, _) => Ok(val),
        VMResult::Effect { name, .. } => Err(format!("Unhandled effect: {name}!")),
    }
}

pub fn run_display(code: &str) -> String {
    match compile(code) {
        Ok(program) => match run_program(&program) {
            Ok(VMResult::Done(val, strings)) => val.display_with(&strings),
            Ok(VMResult::Effect { name, .. }) => format!("EFFECT: {name}!"),
            Err(e) => format!("ERROR: {e}"),
        },
        Err(e) => format!("COMPILE ERROR: {e}"),
    }
}

pub fn run_with_arithmetic_host(program: &Program) -> Result<Value, String> {
    let mut result = run_program(program)?;
    loop {
        match result {
            VMResult::Done(val, _) => return Ok(val),
            VMResult::Effect { name, args, continuation } => {
                let val = host_arithmetic(&name, &args)?;
                result = continuation.resume(val)?;
            }
        }
    }
}

fn host_arithmetic(name: &str, args: &[Value]) -> Result<Value, String> {
    match (name, args) {
        ("add", [Value::Int(a), Value::Int(b)]) => Ok(Value::Int(a + b)),
        ("sub", [Value::Int(a), Value::Int(b)]) => Ok(Value::Int(a - b)),
        ("mul", [Value::Int(a), Value::Int(b)]) => Ok(Value::Int(a * b)),
        ("div", [Value::Int(a), Value::Int(b)]) => {
            if *b == 0 {
                return Err("Division by zero".into());
            }
            Ok(Value::Int(a / b))
        }
        ("mod", [Value::Int(a), Value::Int(b)]) => {
            if *b == 0 {
                return Err("Modulo by zero".into());
            }
            Ok(Value::Int(a % b))
        }
        ("eq", [Value::Int(a), Value::Int(b)]) => Ok(Value::Bool(a == b)),
        ("lt", [Value::Int(a), Value::Int(b)]) => Ok(Value::Bool(a < b)),
        _ => Err(format!("Unhandled host effect: {name}! with {} args", args.len())),
    }
}

pub fn intern(strings: &mut Vec<String>, s: &str) -> StrId {
    if let Some(i) = strings.iter().position(|x| x == s) {
        i
    } else {
        let i = strings.len();
        strings.push(s.to_string());
        i
    }
}

pub fn str_to_charlist(s: &str, strings: &mut Vec<String>) -> Value {
    let chars: Vec<Value> = s
        .chars()
        .map(|c| {
            let id = intern(strings, &c.to_string());
            Value::Tagged(id, vec![])
        })
        .collect();
    Value::list_from_vec(chars)
}

pub fn flatten(val: &Value, strings: &[String], out: &mut String) {
    match val {
        Value::Tagged(id, fields) if fields.is_empty() => {
            if let Some(s) = strings.get(*id) {
                out.push_str(s);
            }
        }
        Value::List(node) => {
            let mut cur = node.as_ref();
            while let ListNode::Cons(head, tail) = cur {
                flatten(head, strings, out);
                cur = tail.as_ref();
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn r(code: &str) -> String {
        run_display(code)
    }

    fn p(code: &str) -> String {
        parse(code)
            .map(|exprs| exprs.iter().map(|e| e.to_string()).collect::<Vec<_>>().join("; "))
            .unwrap_or_else(|e| format!("ERROR: {e}"))
    }

    // --- Parser tests ---

    #[test]
    fn parse_atoms() {
        assert_eq!(p("42"), "42");
        assert_eq!(p("x"), "x");
        assert_eq!(p("Foo"), "Foo");
        assert_eq!(p("\"hello\""), "hello");
    }

    #[test]
    fn parse_prefix() {
        assert_eq!(p("f(x)"), "f(x)");
        assert_eq!(p("f(x, y)"), "f(x, y)");
        assert_eq!(p("Pair(x, y)"), "Pair(x, y)");
    }

    #[test]
    fn parse_infix() {
        assert_eq!(p("a + b"), "(a + b)");
        assert_eq!(p("a + b + c"), "((a + b) + c)");
        assert_eq!(p("x = 42"), "(x = 42)");
    }

    #[test]
    fn parse_blocks() {
        assert_eq!(p("{ x }"), "{ x }");
        assert_eq!(p("{}"), "{}");
        assert_eq!(p("{ a, b }"), "{ a; b }");
    }

    #[test]
    fn parse_lists() {
        assert_eq!(p("[1, 2, 3]"), "[1, 2, 3]");
        assert_eq!(p("[]"), "[]");
    }

    #[test]
    fn parse_trailing_block() {
        assert_eq!(p("f = (x) { x }"), "(f = x { x })");
        assert_eq!(p("Pair(x, y) -> { x }"), "(Pair(x, y) -> { x })");
    }

    #[test]
    fn parse_lambda() {
        assert_eq!(p("(x) => { x }"), "(x => { x })");
        assert_eq!(p("(x, y) => { x }"), "((x, y) => { x })");
    }

    #[test]
    fn parse_effect() {
        assert_eq!(p("read!(\"file\")"), "read!(file)");
    }

    #[test]
    fn parse_grouping() {
        assert_eq!(p("(x)"), "(x)");
        assert_eq!(p("f((a + b))"), "f(((a + b)))");
    }

    #[test]
    fn parse_comments() {
        assert_eq!(p("x // comment\ny"), "x; y");
    }

    #[test]
    fn parse_negative_int() {
        assert_eq!(p("-1"), "-1");
    }

    // --- Basic evaluation ---

    #[test]
    fn eval_int() {
        assert_eq!(r("42"), "42");
    }

    #[test]
    fn eval_arithmetic() {
        assert_eq!(r("2 + 3"), "5");
        assert_eq!(r("10 - 3"), "7");
        assert_eq!(r("4 * 5"), "20");
        assert_eq!(r("10 / 3"), "3");
        assert_eq!(r("10 % 3"), "1");
    }

    #[test]
    fn eval_comparison() {
        assert_eq!(r("1 == 1"), "True");
        assert_eq!(r("1 == 2"), "False");
        assert_eq!(r("1 < 2"), "True");
        assert_eq!(r("2 < 1"), "False");
        assert_eq!(r("1 != 2"), "True");
    }

    #[test]
    fn eval_bool() {
        assert_eq!(r("True"), "True");
        assert_eq!(r("False"), "False");
        assert_eq!(r("not(True)"), "False");
    }

    #[test]
    fn eval_if() {
        assert_eq!(r("if (True) { 1 } else: { 2 }"), "1");
        assert_eq!(r("if (False) { 1 } else: { 2 }"), "2");
    }

    #[test]
    fn eval_block() {
        assert_eq!(r("{ 1, 2, 3 }()"), "3");
    }

    #[test]
    fn eval_binding() {
        assert_eq!(r("x = 42, x"), "42");
        assert_eq!(r("x = 1, y = 2, x + y"), "3");
    }

    #[test]
    fn eval_nested_binding() {
        assert_eq!(r("x = 10, { y = 20, x + y }()"), "30");
    }

    #[test]
    fn eval_tagged() {
        assert_eq!(r("Pair(1, 2)"), "Pair(1, 2)");
        assert_eq!(r("Just(42)"), "Just(42)");
        assert_eq!(r("Nothing"), "Nothing");
    }

    #[test]
    fn eval_list() {
        assert_eq!(r("[1, 2, 3]"), "[1, 2, 3]");
        assert_eq!(r("head([1, 2, 3])"), "1");
        assert_eq!(r("tail([1, 2, 3])"), "[2, 3]");
        assert_eq!(r("is_empty([])"), "True");
        assert_eq!(r("is_empty([1])"), "False");
        assert_eq!(r("cons(0, [1, 2])"), "[0, 1, 2]");
        assert_eq!(r("len([1, 2, 3])"), "3");
    }

    #[test]
    fn eval_tag_fields() {
        assert_eq!(r("fields(Pair(1, 2))"), "[1, 2]");
    }

    // --- Functions ---

    #[test]
    fn eval_lambda() {
        assert_eq!(r("f = ((x) => { x + 1 }), f(41)"), "42");
    }

    #[test]
    fn eval_fn_def() {
        assert_eq!(r("f = (x) { x + 1 }, f(41)"), "42");
    }

    #[test]
    fn eval_recursive_fn() {
        assert_eq!(
            r("factorial = (n) {
                if (n == 0) { 1 } else: { n * factorial(n - 1) }
            }
            factorial(5)"),
            "120"
        );
    }

    #[test]
    fn eval_multi_arg_fn() {
        assert_eq!(
            r("add = (x, y) { x + y }
            add(3, 4)"),
            "7"
        );
    }

    #[test]
    fn eval_higher_order() {
        assert_eq!(
            r("apply = (f, x) { f(x) }
            double = (x) { x * 2 }
            apply(double, 21)"),
            "42"
        );
    }

    // --- Pattern matching ---

    #[test]
    fn match_wildcard() {
        assert_eq!(r("match (42) [x -> { x }]"), "42");
    }

    #[test]
    fn match_tag() {
        assert_eq!(r("match (Just(42)) [Just(x) -> { x }]"), "42");
    }

    #[test]
    fn match_tag_dispatch() {
        assert_eq!(
            r("match (Left(1)) [
                Right(x) -> { x + 100 }
                Left(x) -> { x + 200 }
            ]"),
            "201"
        );
    }

    #[test]
    fn match_zero_field_tag() {
        assert_eq!(
            r("match (Nothing) [
                Just(x) -> { x }
                Nothing -> { 0 }
            ]"),
            "0"
        );
    }

    #[test]
    fn match_int_literal() {
        assert_eq!(
            r("match (0) [
                0 -> { True }
                x -> { False }
            ]"),
            "True"
        );
    }

    #[test]
    fn match_destructure() {
        assert_eq!(
            r("match (Pair(10, 20)) [
                Pair(x, y) -> { x + y }
            ]"),
            "30"
        );
    }

    #[test]
    fn match_no_match_panics() {
        let result = r("match (42) [Nothing -> { 0 }]");
        assert!(result.starts_with("ERROR:"), "got: {result}");
    }

    // --- Effects ---

    #[test]
    fn effect_bubbles_to_host() {
        let program = compile("read!(\"file.txt\")").unwrap();
        match run_program(&program).unwrap() {
            VMResult::Effect { name, args, .. } => {
                assert_eq!(name, "read");
                assert_eq!(args.len(), 1);
            }
            VMResult::Done(v, _) => panic!("Expected effect, got done: {:?}", v),
        }
    }

    #[test]
    fn effect_handled_by_guest() {
        assert_eq!(
            r("try { read!(\"file.txt\") } catch: [
                read!: (resume, path) => { 42 }
            ]"),
            "42"
        );
    }

    #[test]
    fn effect_resume_continuation() {
        assert_eq!(
            r("try {
                x = read!(\"file.txt\")
                x + 1
            } catch: [
                read!: (resume, path) => { resume(41) }
            ]"),
            "42"
        );
    }

    #[test]
    fn effect_multi_shot_via_clone() {
        let program = compile("x = choose!()\nx * 10").unwrap();
        match run_program(&program).unwrap() {
            VMResult::Effect { name, continuation, .. } => {
                assert_eq!(name, "choose");
                let r1 = continuation.clone().resume(Value::Int(3)).unwrap();
                let r2 = continuation.resume(Value::Int(7)).unwrap();
                match (r1, r2) {
                    (VMResult::Done(v1, s1), VMResult::Done(v2, s2)) => {
                        assert_eq!(v1.display_with(&s1), "30");
                        assert_eq!(v2.display_with(&s2), "70");
                    }
                    _ => panic!("expected Done"),
                }
            }
            VMResult::Done(v, _) => panic!("Expected effect, got {:?}", v),
        }
    }

    #[test]
    fn effect_mutable_state() {
        let prelude = "
            run_state = (state, thunk) {
                try { thunk() } catch: [
                    get!: (resume) => { run_state(state, { resume(state) }) },
                    set!: (resume, new_state) => { run_state(new_state, { resume(Unit) }) }
                ]
            }
        ";
        // Basic: set then get
        assert_eq!(r(&format!("{prelude}\nrun_state(0, {{ set!(42), get!() }})")), "42");
        // Increment twice from 0
        assert_eq!(
            r(&format!(
                "{prelude}\nrun_state(0, {{ set!(get!() + 1), set!(get!() + 1), get!() }})"
            )),
            "2"
        );
        // Read initial state
        assert_eq!(r(&format!("{prelude}\nrun_state(99, {{ get!() }})")), "99");
        // Factorial via mutable accumulator
        assert_eq!(
            r(&format!(
                "{prelude}
                factorial = (n) {{
                    if (n == 0) {{ get!() }} else: {{
                        set!(get!() * n),
                        factorial(n - 1)
                    }}
                }}
                run_state(1, {{ factorial(20) }})"
            )),
            "2432902008176640000"
        );
    }

    // ==========================================
    // Natural number implementations
    // ==========================================

    // Helper to run code with the host arithmetic effect handler
    fn run_host(code: &str) -> String {
        match compile(code) {
            Ok(program) => match run_with_arithmetic_host(&program) {
                Ok(val) => val.display(&program),
                Err(e) => format!("ERROR: {e}"),
            },
            Err(e) => format!("COMPILE ERROR: {e}"),
        }
    }

    // --- 1. Built-in ints (baseline) ---

    const BUILTIN_FACTORIAL: &str = "
        factorial = (n) {
            if (n == 0) { 1 } else: { n * factorial(n - 1) }
        }";

    const BUILTIN_FIB: &str = "
        fib = (n) {
            if (n < 2) { n } else: { fib(n - 1) + fib(n - 2) }
        }";

    const BUILTIN_SUM_TO: &str = "
        sum_to = (n) {
            if (n == 0) { 0 } else: { n + sum_to(n - 1) }
        }";

    #[test]
    fn builtin_factorial() {
        assert_eq!(r(&format!("{BUILTIN_FACTORIAL}\nfactorial(0)")), "1");
        assert_eq!(r(&format!("{BUILTIN_FACTORIAL}\nfactorial(1)")), "1");
        assert_eq!(r(&format!("{BUILTIN_FACTORIAL}\nfactorial(5)")), "120");
        assert_eq!(r(&format!("{BUILTIN_FACTORIAL}\nfactorial(10)")), "3628800");
    }

    #[test]
    fn builtin_fib() {
        assert_eq!(r(&format!("{BUILTIN_FIB}\nfib(0)")), "0");
        assert_eq!(r(&format!("{BUILTIN_FIB}\nfib(1)")), "1");
        assert_eq!(r(&format!("{BUILTIN_FIB}\nfib(10)")), "55");
    }

    #[test]
    fn builtin_sum_to() {
        assert_eq!(r(&format!("{BUILTIN_SUM_TO}\nsum_to(0)")), "0");
        assert_eq!(r(&format!("{BUILTIN_SUM_TO}\nsum_to(100)")), "5050");
    }

    // --- 2. Host-handled (symbiotic) ints ---
    // All arithmetic is done through effects: add!, sub!, mul!, div!, mod!, eq!, lt!

    const HOST_FACTORIAL: &str = "
        factorial = (n) {
            if (eq!(n, 0)) { 1 } else: { mul!(n, factorial(sub!(n, 1))) }
        }";

    const HOST_FIB: &str = "
        fib = (n) {
            if (lt!(n, 2)) { n } else: { add!(fib(sub!(n, 1)), fib(sub!(n, 2))) }
        }";

    const HOST_SUM_TO: &str = "
        sum_to = (n) {
            if (eq!(n, 0)) { 0 } else: { add!(n, sum_to(sub!(n, 1))) }
        }";

    #[test]
    fn host_factorial() {
        assert_eq!(run_host(&format!("{HOST_FACTORIAL}\nfactorial(0)")), "1");
        assert_eq!(run_host(&format!("{HOST_FACTORIAL}\nfactorial(1)")), "1");
        assert_eq!(run_host(&format!("{HOST_FACTORIAL}\nfactorial(5)")), "120");
        assert_eq!(run_host(&format!("{HOST_FACTORIAL}\nfactorial(10)")), "3628800");
    }

    #[test]
    fn host_fib() {
        assert_eq!(run_host(&format!("{HOST_FIB}\nfib(0)")), "0");
        assert_eq!(run_host(&format!("{HOST_FIB}\nfib(1)")), "1");
        assert_eq!(run_host(&format!("{HOST_FIB}\nfib(10)")), "55");
    }

    #[test]
    fn host_sum_to() {
        assert_eq!(run_host(&format!("{HOST_SUM_TO}\nsum_to(0)")), "0");
        assert_eq!(run_host(&format!("{HOST_SUM_TO}\nsum_to(100)")), "5050");
    }

    // --- 3. Peano arithmetic (guest-only, unary encoding) ---
    // Z = zero, S(n) = successor
    // Conversion: to_peano(n), from_peano(p)

    const PEANO_PRELUDE: &str = "
        to_peano = (n) {
            if (n == 0) { Z } else: { S(to_peano(n - 1)) }
        }
        from_peano = (p) {
            match (p) [
                Z -> { 0 }
                S(n) -> { 1 + from_peano(n) }
            ]
        }
        peano_add = (a, b) {
            match (a) [
                Z -> { b }
                S(n) -> { S(peano_add(n, b)) }
            ]
        }
        peano_sub = (a, b) {
            match (b) [
                Z -> { a }
                S(m) -> {
                    match (a) [
                        Z -> { Z }
                        S(n) -> { peano_sub(n, m) }
                    ]
                }
            ]
        }
        peano_mul = (a, b) {
            match (a) [
                Z -> { Z }
                S(n) -> { peano_add(b, peano_mul(n, b)) }
            ]
        }
        peano_eq = (a, b) {
            match (a) [
                Z -> { match (b) [ Z -> { True }, S(m) -> { False } ] }
                S(n) -> { match (b) [ Z -> { False }, S(m) -> { peano_eq(n, m) } ] }
            ]
        }
        peano_lt = (a, b) {
            match (b) [
                Z -> { False }
                S(m) -> { match (a) [ Z -> { True }, S(n) -> { peano_lt(n, m) } ] }
            ]
        }
    ";

    const PEANO_FACTORIAL: &str = "
        factorial = (n) {
            if (peano_eq(n, Z)) { S(Z) } else: { peano_mul(n, factorial(peano_sub(n, S(Z)))) }
        }
    ";

    const PEANO_FIB: &str = "
        fib = (n) {
            if (peano_lt(n, S(S(Z)))) { n } else: { peano_add(fib(peano_sub(n, S(Z))), fib(peano_sub(n, S(S(Z))))) }
        }
    ";

    const PEANO_SUM_TO: &str = "
        sum_to = (n) {
            if (peano_eq(n, Z)) { Z } else: { peano_add(n, sum_to(peano_sub(n, S(Z)))) }
        }
    ";

    fn peano_prog(body: &str) -> String {
        format!("{PEANO_PRELUDE}\n{body}")
    }

    #[test]
    fn peano_conversions() {
        assert_eq!(r(&peano_prog("from_peano(Z)")), "0");
        assert_eq!(r(&peano_prog("from_peano(S(Z))")), "1");
        assert_eq!(r(&peano_prog("from_peano(S(S(S(Z))))")), "3");
        assert_eq!(r(&peano_prog("from_peano(to_peano(7))")), "7");
    }

    #[test]
    fn peano_add() {
        assert_eq!(r(&peano_prog("from_peano(peano_add(to_peano(3), to_peano(4)))")), "7");
    }

    #[test]
    fn peano_sub() {
        assert_eq!(r(&peano_prog("from_peano(peano_sub(to_peano(5), to_peano(3)))")), "2");
        assert_eq!(r(&peano_prog("from_peano(peano_sub(to_peano(2), to_peano(5)))")), "0");
    }

    #[test]
    fn peano_mul() {
        assert_eq!(r(&peano_prog("from_peano(peano_mul(to_peano(3), to_peano(4)))")), "12");
    }

    #[test]
    fn peano_comparisons() {
        assert_eq!(r(&peano_prog("peano_eq(Z, Z)")), "True");
        assert_eq!(r(&peano_prog("peano_eq(S(Z), Z)")), "False");
        assert_eq!(r(&peano_prog("peano_lt(Z, S(Z))")), "True");
        assert_eq!(r(&peano_prog("peano_lt(S(Z), Z)")), "False");
    }

    #[test]
    fn peano_factorial() {
        let code = peano_prog(&format!("{PEANO_FACTORIAL} from_peano(factorial(to_peano(5)))"));
        assert_eq!(r(&code), "120");
    }

    #[test]
    fn peano_fib() {
        let code = peano_prog(&format!("{PEANO_FIB} from_peano(fib(to_peano(10)))"));
        assert_eq!(r(&code), "55");
    }

    #[test]
    fn peano_sum_to() {
        let code = peano_prog(&format!("{PEANO_SUM_TO} from_peano(sum_to(to_peano(10)))"));
        assert_eq!(r(&code), "55");
    }
}
