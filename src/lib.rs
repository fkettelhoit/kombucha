use std::{
    iter::{Peekable, once},
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
    LBracket,
    RBracket,
    Dot,
    Separator,
}

fn scan<'code>(code: &'code str) -> Vec<(Tok, usize, &'code str)> {
    let mut toks = vec![];
    let mut i = 0;
    for (j, c) in code.chars().into_iter().chain(once(' ')).enumerate() {
        let tok = match c {
            '(' => Some(Tok::LParen),
            ')' => Some(Tok::RParen),
            '{' => Some(Tok::LBrace),
            '}' => Some(Tok::RBrace),
            '[' => Some(Tok::LBracket),
            ']' => Some(Tok::RBracket),
            '.' => Some(Tok::Dot),
            ',' | '\n' => Some(Tok::Separator),
            _ => None,
        };
        if tok.is_some() || c.is_ascii_whitespace() {
            if let (Some(n), Some(l)) = (code[i..j].chars().next(), code[i..j].chars().last()) {
                let tok = match (n, l) {
                    (c, _) if c.is_ascii_uppercase() => Tok::String,
                    ('\'', _) => Tok::Binding,
                    (_, ':') => Tok::Keyword,
                    (_, _) => Tok::Symbol,
                };
                toks.push((tok, j, &code[i..j]));
            }
            i = j + 1;
        }
        if let Some(tok) = tok {
            toks.push((tok, j, &code[j..j + 1]));
        }
    }
    toks
}

enum Ast<'code> {
    Nil,
    Var(&'code str),
    String(&'code str),
    Binding(&'code str),
    List(Vec<Ast<'code>>),
    Block(Vec<Ast<'code>>),
    PrefixCall(Box<Ast<'code>>, Vec<Ast<'code>>),
    InfixCall(Box<Ast<'code>>, &'code str, Box<Ast<'code>>),
}

pub struct Prg<'code>(Vec<Ast<'code>>);

enum E<'code> {
    NoParenAfterGroup(&'code str),
    EndOfGroup,
    ClosingTok(&'code str),
    InvalidArgTok(&'code str),
    NeedsSep(&'code str),
}

fn msg<'code>(e: &E<'code>) -> String {
    match e {
        E::NoParenAfterGroup(s) => format!("Expected (...) to be closed with ')', but found '{s}'"),
        E::EndOfGroup => format!("Expected (...) to be closed with ')', but the code just ended"),
        E::ClosingTok(s) => format!("Expected an expression, but found an unmatched '{s}'"),
        E::InvalidArgTok(s) => format!("Expected a simple expression, but found '{s}'"),
        E::NeedsSep(s) => {
            format!("Expressions must be separated using ',' or '\\n', but found '{s}'")
        }
    }
}

pub fn parse<'code>(code: &'code str) -> Result<Prg<'code>, String> {
    struct Error<'code>(usize, E<'code>);
    type Toks<'code> = Peekable<IntoIter<(Tok, usize, &'code str)>>;
    let mut toks = scan(code).into_iter().peekable();
    fn _expr<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, Option<Error<'c>>> {
        _infix_call(toks)
    }
    fn _infix_call<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, Option<Error<'c>>> {
        let expr = _prefix_call(toks)?;
        if let Some((Tok::Symbol, _, s)) = toks.peek().copied() {
            toks.next();
            let expr2 = _prefix_call(toks)?;
            Ok(Ast::InfixCall(Box::new(expr), s, Box::new(expr2)))
        } else {
            Ok(expr)
        }
    }
    fn _prefix_call<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, Option<Error<'c>>> {
        let mut expr = _arg(toks)?;
        while let Some((Tok::LParen, _, _)) = toks.peek() {
            toks.next();
            let args = _exprs(toks, Some(Tok::RParen))?;
            expr = Ast::PrefixCall(Box::new(expr), args);
        }
        Ok(expr)
    }
    fn _arg<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, Option<Error<'c>>> {
        let Some((tok, i, s)) = toks.next() else {
            return Err(None);
        };
        match tok {
            Tok::Symbol => Ok(Ast::Var(s)),
            Tok::String => Ok(Ast::String(s)),
            Tok::Binding => Ok(Ast::Binding(s)),
            Tok::LParen => {
                if let Some((Tok::RParen, _, _)) = toks.peek() {
                    toks.next();
                    return Ok(Ast::Nil);
                }
                let expr = _expr(toks)?;
                match toks.next() {
                    Some((Tok::RParen, _, _)) => Ok(expr),
                    Some((_, i, s)) => Err(Some(Error(i, E::NoParenAfterGroup(s)))),
                    None => Err(Some(Error(i, E::EndOfGroup))),
                }
            }
            Tok::LBrace => Ok(Ast::Block(_exprs(toks, Some(Tok::RBrace))?)),
            Tok::LBracket => Ok(Ast::List(_exprs(toks, Some(Tok::RBracket))?)),
            Tok::RParen | Tok::RBrace | Tok::RBracket => Err(Some(Error(i, E::ClosingTok(s)))),
            Tok::Keyword | Tok::Dot | Tok::Separator => Err(Some(Error(i, E::InvalidArgTok(s)))),
        }
    }
    fn _exprs<'c>(toks: &mut Toks<'c>, t: Option<Tok>) -> Result<Vec<Ast<'c>>, Option<Error<'c>>> {
        let mut exprs = vec![];
        let mut needs_separator = false;
        loop {
            match toks.peek() {
                tok if tok.map(|(tok, _, _)| *tok) == t => {
                    toks.next();
                    return Ok(exprs);
                }
                Some((Tok::Separator, _, _)) => {
                    toks.next();
                    needs_separator = false;
                }
                Some((_, i, s)) if needs_separator => {
                    return Err(Some(Error(*i, E::NeedsSep(*s))));
                }
                _ => {
                    exprs.push(_expr(toks)?);
                    needs_separator = true;
                }
            }
        }
    }
    match _exprs(&mut toks, None) {
        Ok(elems) => Ok(Prg(elems)),
        Err(None) => todo!(),
        Err(Some(Error(i, e))) => {
            let msg = msg(&e);
            let mut line = 0;
            let mut col = 0;
            for c in code.chars().take(i) {
                if c == '\n' {
                    line += 1;
                    col = 0;
                } else {
                    col += 1;
                }
            }
            Err(format!("{msg}, at line {line}, col {col}"))
        }
    }
}

impl<'code> Ast<'code> {
    const MULTILINE_SIZE_THRESHOLD: usize = 10;

    fn size(&self) -> usize {
        match self {
            Ast::Nil | Ast::Var(_) | Ast::String(_) | Ast::Binding(_) => 1,
            Ast::List(xs) | Ast::Block(xs) => xs.iter().map(|x| x.size()).sum::<usize>() + 1,
            Ast::PrefixCall(f, args) => f.size() + args.iter().map(|x| x.size()).sum::<usize>() + 1,
            Ast::InfixCall(a, _, b) => a.size() + b.size() + 1,
        }
    }

    fn pretty(&self, buf: &mut String, lvl: usize, wrap: bool) {
        fn indent(buf: &mut String, lvl: usize) {
            for _ in 0..lvl {
                buf.push_str("  ");
            }
        }
        fn pretty_exprs<'c>(exprs: &[Ast<'c>], buf: &mut String, lvl: usize, is_multiline: bool) {
            if is_multiline {
                for expr in exprs {
                    buf.push('\n');
                    indent(buf, lvl + 1);
                    expr.pretty(buf, lvl + 1, false);
                }
                buf.push('\n');
                indent(buf, lvl);
            } else {
                for (i, expr) in exprs.iter().enumerate() {
                    if i != 0 {
                        buf.push_str(", ");
                    }
                    expr.pretty(buf, lvl, false);
                }
            }
        }
        let is_multiline = self.size() > Self::MULTILINE_SIZE_THRESHOLD;
        match self {
            Ast::Nil => buf.push_str("()"),
            Ast::Var(s) | Ast::String(s) | Ast::Binding(s) => buf.push_str(s),
            Ast::List(elems) => {
                buf.push('[');
                pretty_exprs(elems, buf, lvl, is_multiline);
                buf.push(']');
            }
            Ast::Block(elems) => {
                buf.push('{');
                pretty_exprs(elems, buf, lvl, is_multiline);
                buf.push('}');
            }
            Ast::PrefixCall(f, args) => {
                f.pretty(buf, lvl, true);
                buf.push('(');
                pretty_exprs(args, buf, lvl, is_multiline);
                buf.push(')');
            }
            Ast::InfixCall(a, f, b) => {
                if wrap {
                    buf.push('(');
                }
                a.pretty(buf, lvl, true);
                buf.push(' ');
                buf.push_str(f);
                buf.push(' ');
                b.pretty(buf, lvl, true);
                if wrap {
                    buf.push(')');
                }
            }
        }
    }
}

impl<'code> std::fmt::Display for Prg<'code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, elem) in self.0.iter().enumerate() {
            if i != 0 {
                f.write_str("\n\n")?;
            }
            let mut buf = String::new();
            elem.pretty(&mut buf, 0, false);
            f.write_str(&buf)?;
        }
        Ok(())
    }
}

enum Value {}

impl<'code> Ast<'code> {
    fn eval(self) -> Value {
        todo!()
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pretty_var() {
        let code = "foo";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_string() {
        let code = "Foo";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_binding() {
        let code = "'foo";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_block() {
        let code = "{foo, Foo, 'foo}";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_list() {
        let code = "[foo, Foo, 'foo]";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_prefix_call() {
        let code = "f(g(), h(Bar), Baz)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_infix_call() {
        let code = "f(x) foo g(y)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_grouped_infix_call() {
        let code = "a + (b * c)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty() {
        let code = "'foo = Foo

'id = ('x => {x})

'x = id(id(foo))

Pair(x, x)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }
}
