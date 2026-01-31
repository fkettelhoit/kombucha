#[derive(Debug, Clone, Copy)]
pub struct Pos(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tok<'code> {
    Sep(char),
    Var(&'code str),
    Str(&'code str),
}

impl std::fmt::Display for Tok<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tok::Sep(c) => write!(f, "'{c}'"),
            Tok::Var(s) | Tok::Str(s) => write!(f, "'{s}'"),
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
            _ if matches!(code[j..].chars().next(), Some(':')) => toks.push((Pos(i), Tok::Str(s))),
            _ => toks.push((Pos(i), Tok::Var(s))),
        }
    }
    while let Some((j, c)) = chars.next() {
        match c {
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
    toks: std::iter::Peekable<std::vec::IntoIter<(Pos, Tok<'code>)>>,
}

impl<'c> Parser<'c> {
    fn expr(&mut self) -> Result<Ast<'c>, E> {
        let (expr, mut args) = match self.infix()? {
            Ast::Infix(f, [x, y], None) => match self.toks.peek().map(|(_, t)| t) {
                Some(Tok::Sep('[' | '{')) => {
                    return Ok(Ast::Infix(f, [x, y], Some(self.value()?.into())));
                }
                _ => return Ok(Ast::Infix(f, [x, y], None)),
            },
            Ast::Prefix(f, args) => (f, Some(args)),
            Ast::Var(i, v) => (Box::new(Ast::Var(i, v)), None),
            expr => return Ok(expr),
        };
        while let Some((_, Tok::Sep('[' | '{'))) = self.toks.peek() {
            args.get_or_insert_with(Vec::new).push(self.value()?);
        }
        let mut kw_args = vec![];
        while let Some((i, Tok::Str(k))) = self.toks.peek().copied() {
            let (_, Some((_, Tok::Sep(':')))) = (self.toks.next(), self.toks.next()) else {
                return Err((i, format!("Expected keyword '{k}' to end with ':'")));
            };
            kw_args.push(Ast::Tuple(i, vec![Ast::String(i, k), self.infix()?]));
        }
        if let Some(Ast::Tuple(i, _)) = kw_args.first() {
            args.get_or_insert_with(Vec::new).push(Ast::List(*i, kw_args))
        }
        match args {
            None => Ok(*expr),
            Some(args) => Ok(Ast::Prefix(expr, args)),
        }
    }

    fn infix(&mut self) -> Result<Ast<'c>, E> {
        let mut x = self.prefix()?;
        let Some((i, Tok::Var(f))) = self.toks.peek().copied() else {
            return Ok(x);
        };
        while let Some((j, Tok::Var(g))) = self.toks.next_if(|(_, t)| matches!(t, Tok::Var(_))) {
            if f != g {
                return Err((j, format!("Expected infix {f}, found {g}")));
            }
            x = Ast::Infix(Box::new(Ast::Var(i, f)), [x.into(), self.prefix()?.into()], None);
        }
        Ok(x)
    }

    fn prefix(&mut self) -> Result<Ast<'c>, E> {
        let mut expr = self.value()?;
        while let Some(_) = self.toks.next_if(|(_, t)| *t == Tok::Sep('(')) {
            expr = Ast::Prefix(Box::new(expr), self.exprs(Some(Tok::Sep(')')))?);
        }
        Ok(expr)
    }

    fn value(&mut self) -> Result<Ast<'c>, E> {
        match self.toks.next() {
            None => Err((Pos(0), "Expected a value. But the code ended.".to_string())),
            Some((i, Tok::Var(s))) => Ok(Ast::Var(i, s)),
            Some((i, Tok::Str(s))) => Ok(Ast::String(i, s)),
            Some((i, Tok::Sep('['))) => Ok(Ast::List(i, self.exprs(Some(Tok::Sep(']')))?)),
            Some((i, Tok::Sep('('))) => Ok(Ast::Tuple(i, self.exprs(Some(Tok::Sep(')')))?)),
            Some((i, Tok::Sep('{'))) => Ok(Ast::Block(i, self.exprs(Some(Tok::Sep('}')))?)),
            Some((i, t)) => Err((i, format!("Expected a value, found {t}"))),
        }
    }

    fn exprs(&mut self, until: Option<Tok<'c>>) -> Result<Vec<Ast<'c>>, E> {
        let mut exprs = vec![];
        let mut last_sep = Some((Pos(0), Tok::Sep(',')));
        loop {
            match self.toks.peek() {
                tok if tok.map(|(_, t)| *t) == until => {
                    self.toks.next();
                    return Ok(exprs);
                }
                Some((_, Tok::Sep(',' | '\n'))) => last_sep = self.toks.next(),
                Some((i, t)) if last_sep.is_none() => {
                    return Err((*i, format!("Expected ',' or '\\n', found {t}")));
                }
                _ => match self.expr() {
                    Ok(expr) => {
                        exprs.push(expr);
                        last_sep = None;
                    }
                    Err(e) => {
                        // todo
                        return Err(e);
                    }
                },
            }
        }
    }
}

pub fn parse(code: &str) -> Result<Vec<Ast<'_>>, String> {
    let mut parser = Parser { toks: scan(code).into_iter().peekable() };
    parser.exprs(None).map_err(|(Pos(i), msg)| format!("Error at {}\n{msg}", pos_at(i, code)))
}
