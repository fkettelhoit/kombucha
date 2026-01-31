#[derive(Debug, Clone, Copy)]
pub struct Pos(usize);

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
    Prefix(Box<Ast<'code>>, Vec<Ast<'code>>),
    Infix(Box<Ast<'code>>, [Box<Ast<'code>>; 2], Option<Box<Ast<'code>>>),
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

type E = (Pos, String);

struct Parser<'code> {
    end_pos: Pos,
    toks: std::iter::Peekable<std::vec::IntoIter<(Pos, Tok<'code>)>>,
}

impl<'c> Parser<'c> {
    fn expr(&mut self, expected: &str) -> Result<Ast<'c>, E> {
        if let Some((i, Tok::Key(k))) = self.toks.peek().copied() {
            self.toks.next();
            return Ok(Ast::Tuple(i, vec![Ast::String(i, k), self.infix("value after key")?]));
        }
        let (expr, mut args) = match self.infix(expected)? {
            Ast::Infix(f, [x, y], None) => match self.toks.peek().map(|(_, t)| t) {
                Some(Tok::Sep('[' | '{')) => {
                    let trailing = self.value("a trailing [...] or {...}")?;
                    return Ok(Ast::Infix(f, [x, y], Some(trailing.into())));
                }
                _ => return Ok(Ast::Infix(f, [x, y], None)),
            },
            Ast::Prefix(f, args) => (f, Some(args)),
            Ast::Var(i, v) => (Box::new(Ast::Var(i, v)), None),
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
            Some(args) => Ok(Ast::Prefix(expr, args)),
        }
    }

    fn infix(&mut self, expected: &str) -> Result<Ast<'c>, E> {
        let mut x = self.prefix(expected)?;
        let Some((i, Tok::Var(f))) = self.toks.peek().copied() else {
            return Ok(x);
        };
        while let Some((j, Tok::Var(g))) = self.toks.next_if(|(_, t)| matches!(t, Tok::Var(_))) {
            if f != g {
                return Err((j, format!("Expected the infix function '{f}', found '{g}'")));
            }
            let y = self.prefix("an infix argument")?;
            x = Ast::Infix(Box::new(Ast::Var(i, f)), [x.into(), y.into()], None);
        }
        Ok(x)
    }

    fn prefix(&mut self, expected: &str) -> Result<Ast<'c>, E> {
        let mut expr = self.value(expected)?;
        while let Some(_) = self.toks.next_if(|(_, t)| *t == Tok::Sep('(')) {
            let args = self.exprs("function arguments", Some(Tok::Sep(')')))?;
            expr = Ast::Prefix(Box::new(expr), args);
        }
        Ok(expr)
    }

    fn value(&mut self, expected: &str) -> Result<Ast<'c>, E> {
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

    fn exprs(&mut self, expected: &str, until: Option<Tok<'c>>) -> Result<Vec<Ast<'c>>, E> {
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
                    return Err((self.end_pos, format!("Expected {expected} to end with {until}")));
                }
                (None, Some((i, t)), None) => {
                    return Err((*i, format!("Expected ',' or '\\n', found {t}")));
                }
                (None, Some((i, t)), Some(until)) => {
                    return Err((*i, format!("Expected ',', '\\n', or {until}, found {t}")));
                }
                (Some(_), Some(_), _) => {
                    exprs.push(self.expr(expected)?);
                    last_sep = None;
                }
            }
        }
    }
}

pub fn parse(code: &str) -> Result<Vec<Ast<'_>>, String> {
    Parser { end_pos: Pos(code.len()), toks: scan(code).into_iter().peekable() }
        .exprs("an expression", None)
        .map_err(|(Pos(i), msg)| format!("{msg} at {}", pos_at(i, code)))
}
