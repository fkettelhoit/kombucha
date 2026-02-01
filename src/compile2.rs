use std::{collections::HashMap, rc::Rc};

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
}

impl std::fmt::Display for Tok<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tok::Sep(c) => write!(f, "'{c}'"),
            Tok::Var(s) | Tok::Key(s) | Tok::Str(s) => write!(f, "'{s}'"),
        }
    }
}

fn scan(code: &str) -> Vec<(Pos, Tok<'_>)> {
    let mut toks = vec![];
    let mut i = 0;
    let mut chars = code.char_indices().chain(std::iter::once((code.len(), ' ')));
    fn push_ident<'a>(toks: &mut Vec<(Pos, Tok<'a>)>, code: &'a str, i: usize, j: usize) {
        let s = &code[i..j];
        match s.chars().next() {
            None => {}
            Some(c) if c.is_ascii_uppercase() => toks.push((Pos(i), Tok::Str(s))),
            _ => toks.push((Pos(i), Tok::Var(s))),
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
                i = chars.by_ref().find(|(_, c)| *c == '"').map_or(code.len(), |(j, _)| j + 1);
                toks.push((Pos(j), Tok::Str(&code[j + 1..i - 1])));
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

#[derive(Debug, Clone)]
pub enum Ast<'code> {
    Var(Pos, &'code str),
    String(Pos, &'code str),
    List(Pos, Vec<Ast<'code>>),
    Tuple(Pos, Vec<Ast<'code>>),
    Block(Pos, Vec<Ast<'code>>),
    Prefix(Pos, Box<Ast<'code>>, Vec<Ast<'code>>),
    Infix(Pos, Box<Ast<'code>>, [Box<Ast<'code>>; 2], Option<Box<Ast<'code>>>),
}

struct Parser<'code> {
    end_pos: Pos,
    toks: std::iter::Peekable<std::vec::IntoIter<(Pos, Tok<'code>)>>,
}

impl<'c> Parser<'c> {
    fn expr(&mut self, expected: &str) -> Result<Ast<'c>, (Pos, String)> {
        if let Some((i, Tok::Key(k))) = self.toks.peek().copied() {
            self.toks.next();
            return Ok(Ast::Tuple(i, vec![Ast::String(i, k), self.infix("value after key")?]));
        }
        let (i, expr, mut args) = match self.infix(expected)? {
            Ast::Infix(i, f, [x, y], None) => match self.toks.peek().map(|(_, t)| t) {
                Some(Tok::Sep('[' | '{')) => {
                    let trailing = self.value("a trailing [...] or {...}")?;
                    return Ok(Ast::Infix(i, f, [x, y], Some(trailing.into())));
                }
                _ => return Ok(Ast::Infix(i, f, [x, y], None)),
            },
            Ast::Prefix(i, f, args) => (i, f, Some(args)),
            Ast::Var(i, v) => (i, Box::new(Ast::Var(i, v)), None),
            expr => return Ok(expr),
        };
        while let Some((_, Tok::Sep('[' | '{'))) = self.toks.peek() {
            args.get_or_insert_with(Vec::new).push(self.value("a trailing [...] or {...}")?);
        }
        let mut kw_args = vec![];
        while let Some((i, Tok::Key(k))) = self.toks.peek().copied() {
            self.toks.next();
            kw_args.push(Ast::Tuple(i, vec![Ast::String(i, k), self.infix("a keyword argument")?]));
        }
        if let Some(Ast::Tuple(i, _)) = kw_args.first() {
            args.get_or_insert_with(Vec::new).push(Ast::List(*i, kw_args))
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
                return Err((j, format!("Expected the infix function '{f}', found '{g}'")));
            }
            let y = match self.prefix("an infix argument")? {
                Ast::Tuple(_, mut elems) if elems.len() == 1 => elems.pop().unwrap(),
                y => y,
            };
            x = Ast::Infix(i, Box::new(Ast::Var(i, f)), [x.into(), y.into()], None);
        }
        Ok(x)
    }

    fn prefix(&mut self, expected: &str) -> Result<Ast<'c>, (Pos, String)> {
        let mut expr = self.value(expected)?;
        while let Some((i, _)) = self.toks.next_if(|(_, t)| *t == Tok::Sep('(')) {
            let args = self.exprs("function arguments", Some(Tok::Sep(')')))?;
            expr = Ast::Prefix(i, Box::new(expr), args);
        }
        Ok(expr)
    }

    fn value(&mut self, expected: &str) -> Result<Ast<'c>, (Pos, String)> {
        match self.toks.next() {
            Some((i, Tok::Sep('['))) => {
                Ok(Ast::List(i, self.exprs("list elements after '['", Some(Tok::Sep(']')))?))
            }
            Some((i, Tok::Sep('('))) => {
                Ok(Ast::Tuple(i, self.exprs("tuple elements after '('", Some(Tok::Sep(')')))?))
            }
            Some((i, Tok::Sep('{'))) => {
                Ok(Ast::Block(i, self.exprs("block elements after '{'", Some(Tok::Sep('}')))?))
            }
            Some((i, Tok::Var(s))) => Ok(Ast::Var(i, s)),
            Some((i, Tok::Str(s))) => Ok(Ast::String(i, s)),
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

impl<'c> Ast<'c> {
    fn bindings(&self) -> Vec<&'c str> {
        match self {
            Ast::Var(_, v) => vec![v],
            Ast::String(_, _) | Ast::Block(_, _) => vec![],
            Ast::List(_, xs) | Ast::Tuple(_, xs) => xs.iter().flat_map(|x| x.bindings()).collect(),
            Ast::Prefix(_, f, xs) => {
                std::iter::once(f.as_ref()).chain(xs.iter()).flat_map(|x| x.bindings()).collect()
            }
            Ast::Infix(_, f, [x, y], trailing) => {
                [f, x, y].into_iter().chain(trailing.iter()).flat_map(|x| x.bindings()).collect()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Op {
    LoadFn { args: usize, ops: Rc<Vec<Op>> },
    LoadVar(usize),
    LoadString(usize),
    List(usize),
    Tuple(usize),
    Apply(usize),
    Jump(usize),
}

#[derive(Debug, Clone, Default)]
struct Compiler<'code> {
    ops: Vec<Op>,
    meta: Vec<Pos>,
    env: Vec<&'code str>,
    strs: HashMap<&'code str, usize>,
    builtins: HashMap<&'code str, (usize, Rc<Vec<Op>>)>,
}

impl<'c> Compiler<'c> {
    fn push_op(&mut self, pos: &Pos, op: Op) -> Result<(), (Pos, String)> {
        self.ops.push(op);
        self.meta.push(*pos);
        Ok(())
    }

    fn push_fn(&mut self, pos: &Pos, start: usize, args: usize) -> Result<(), (Pos, String)> {
        let ops = Rc::new(self.ops.drain(start..).collect::<Vec<_>>());
        self.push_op(pos, Op::LoadFn { args, ops })
    }

    fn resolve_str(&mut self, s: &'c str) -> usize {
        let len = self.strs.len();
        *self.strs.entry(s).or_insert(len)
    }

    fn compile_block(&mut self, elems: &[Ast<'c>]) -> Result<usize, (Pos, String)> {
        let start = self.ops.len();
        let mut scopes = vec![];
        for elem in elems {
            match elem {
                Ast::Infix(pos, f, [x, y], trailing) => {
                    let bindings = x.bindings();
                    self.compile(x)?;
                    self.compile(y)?;
                    if let Some(_trailing) = trailing {
                        todo!("trailing arg in macro with block scope")
                    }
                    let args = if trailing.is_none() { 3 } else { 4 };
                    scopes.push((pos, f, bindings.len(), self.ops.len(), args));
                    self.env.extend(bindings);
                }
                elem => self.compile(elem)?,
            }
        }
        while let Some((pos, f, bindings, start, args)) = scopes.pop() {
            self.push_fn(pos, start, bindings)?;
            self.compile(f)?;
            self.push_op(pos, Op::Apply(args))?;
        }
        Ok(start)
    }

    fn compile(&mut self, ast: &Ast<'c>) -> Result<(), (Pos, String)> {
        match ast {
            Ast::Var(pos, v) => match self.env.iter().rev().position(|var| var == v) {
                Some(v) => self.push_op(pos, Op::LoadVar(v)),
                None => match self.builtins.get(v).cloned() {
                    Some((args, ops)) => self.push_op(pos, Op::LoadFn { args, ops }),
                    _ => return Err((*pos, format!("Unbound variable '{v}'"))),
                },
            },
            Ast::String(pos, s) => {
                let interned = self.resolve_str(s);
                self.push_op(pos, Op::LoadString(interned))
            }
            Ast::List(pos, elems) => {
                elems.iter().try_for_each(|elem| self.compile(elem))?;
                self.push_op(pos, Op::List(elems.len()))
            }
            Ast::Tuple(pos, elems) => {
                elems.iter().try_for_each(|elem| self.compile(elem))?;
                self.push_op(pos, Op::Tuple(elems.len()))
            }
            Ast::Block(pos, elems) if elems.is_empty() => self.push_op(pos, Op::Tuple(0)),
            Ast::Block(pos, elems) => {
                let start = self.compile_block(elems)?;
                self.push_fn(pos, start, 0)
            }
            Ast::Prefix(pos, f, args) => {
                args.iter().chain(std::iter::once(f.as_ref())).try_for_each(|x| self.compile(x))?;
                self.push_op(pos, Op::Apply(args.len()))
            }
            Ast::Infix(pos, f, [x, y], trailing) => {
                match y.as_ref() {
                    Ast::Block(pos, elems) => {
                        let bindings = x.bindings();
                        self.compile(x)?;
                        self.env.extend(bindings.iter());
                        let start = self.compile_block(elems)?;
                        self.push_fn(pos, start, bindings.len())?;
                    }
                    y => {
                        self.compile(x)?;
                        self.compile(y)?;
                    }
                }
                if let Some(_trailing) = trailing {
                    todo!("trailing arg in macro with argument scope");
                }
                self.compile(f)?;
                self.push_op(pos, Op::Apply(2))
            }
        }
    }
}

pub fn compile(code: &str) -> Result<Vec<Op>, String> {
    let ast = parse(code)?;
    let builtins = HashMap::from_iter(vec![
        ("=", (3, Rc::new(vec![Op::LoadVar(1), Op::LoadVar(0), Op::Apply(1)]))),
        ("=>", (2, Rc::new(vec![Op::LoadVar(0)]))),
        ("__fix", (0, Rc::new(vec![]))),
        ("__unpack", (0, Rc::new(vec![]))),
        ("__compare", (0, Rc::new(vec![]))),
    ]);
    let mut compiler = Compiler { builtins, ..Default::default() };
    compiler.compile_block(&ast).map_err(|(pos, msg)| format!("{msg} at {}", pos.line_in(code)))?;
    Ok(compiler.ops)
}
