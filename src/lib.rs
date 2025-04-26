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
    Separator,
}

fn scan<'code>(code: &'code str) -> Vec<(Tok, usize, &'code str)> {
    let mut toks = vec![];
    let mut i = 0;
    for (j, c) in code.chars().chain(once(' ')).enumerate() {
        let tok = match c {
            '(' => Some(Tok::LParen),
            ')' => Some(Tok::RParen),
            '{' => Some(Tok::LBrace),
            '}' => Some(Tok::RBrace),
            '[' => Some(Tok::LBracket),
            ']' => Some(Tok::RBracket),
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
enum Ast<'code> {
    Var(&'code str),
    String(&'code str),
    Binding(&'code str),
    List(Vec<Ast<'code>>),
    Block(Vec<Ast<'code>>),
    PrefixCall(Box<Ast<'code>>, Vec<Ast<'code>>),
    InfixCall(Box<Ast<'code>>, &'code str, Vec<Ast<'code>>),
}

#[derive(Debug, Clone)]
pub struct Prg<'code>(Vec<Ast<'code>>);

pub fn parse<'code>(code: &'code str) -> Result<Prg<'code>, String> {
    type Toks<'c> = Peekable<IntoIter<(Tok, usize, &'c str)>>;
    fn _infix<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, Option<Error<'c>>> {
        let expr = _prefix(toks)?;
        let mut f = None;
        let mut args = vec![];
        while let Some((Tok::Symbol, i, s)) = toks.peek().copied() {
            toks.next();
            match f {
                Some((f, h)) if f != s => return Err(Some(Error(h, E::InfixMismatch(f, i, s)))),
                _ => f = Some((s, i)),
            }
            match _prefix(toks) {
                Ok(arg) => args.push(arg),
                Err(None) => return Err(Some(Error(i, E::ExpectedInfixArg(s, None)))),
                Err(Some(Error(j, E::Unmatched(t) | E::SeparatorAsValue(t)))) => {
                    return Err(Some(Error(i, E::ExpectedInfixArg(s, Some((j, t))))));
                }
                Err(Some(e)) => return Err(Some(e)),
            }
        }
        match f {
            Some((f, _)) => Ok(Ast::InfixCall(Box::new(expr), f, args)),
            None => Ok(expr),
        }
    }
    fn _prefix<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, Option<Error<'c>>> {
        let mut expr = _value(toks)?;
        while let Some((Tok::LParen, i, _)) = toks.peek().copied() {
            toks.next();
            expr = Ast::PrefixCall(Box::new(expr), _exprs(toks, i, Some(Tok::RParen))?);
        }
        Ok(expr)
    }
    fn _value<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, Option<Error<'c>>> {
        let Some((t, i, s)) = toks.next() else {
            return Err(None);
        };
        match t {
            Tok::Symbol => Ok(Ast::Var(s)),
            Tok::String => Ok(Ast::String(s)),
            Tok::Binding => Ok(Ast::Binding(s)),
            Tok::LParen => {
                let expr = _infix(toks)
                    .map_err(|e| e.unwrap_or_else(|| Error(i, E::ExpectedRParen(None))))?;
                match toks.next() {
                    Some((Tok::RParen, _, _)) => Ok(expr),
                    Some((_, j, s)) => Err(Some(Error(i, E::ExpectedRParen(Some((j, s)))))),
                    None => Err(Some(Error(i, E::ExpectedRParen(None)))),
                }
            }
            Tok::LBrace => Ok(Ast::Block(_exprs(toks, i, Some(Tok::RBrace))?)),
            Tok::LBracket => Ok(Ast::List(_exprs(toks, i, Some(Tok::RBracket))?)),
            Tok::RParen | Tok::RBrace | Tok::RBracket => Err(Some(Error(i, E::Unmatched(s)))),
            Tok::Keyword => Err(Some(Error(i, E::KeywordAsValue(s)))),
            Tok::Separator => Err(Some(Error(i, E::SeparatorAsValue(s)))),
        }
    }
    fn _exprs<'c>(ts: &mut Toks<'c>, i: usize, t: Option<Tok>) -> Result<Vec<Ast<'c>>, Error<'c>> {
        let mut exprs = vec![];
        let mut needs_separator = false;
        loop {
            match ts.peek().copied() {
                tok if tok.map(|t| t.0) == t => {
                    ts.next();
                    return Ok(exprs);
                }
                Some((Tok::Separator, _, _)) => {
                    ts.next();
                    needs_separator = false;
                }
                Some((_, j, s)) if needs_separator => {
                    return Err(Error(i, E::ExpectedSeparator(j, s)));
                }
                _ => match _infix(ts) {
                    Ok(expr) => {
                        exprs.push(expr);
                        needs_separator = true;
                    }
                    Err(None) => return Err(Error(i, E::ExpectedClosing(t, None))),
                    Err(Some(Error(j, E::Unmatched(closing)))) => {
                        return Err(Error(i, E::ExpectedClosing(t, Some((j, closing)))));
                    }
                    Err(Some(e)) => return Err(e),
                },
            }
        }
    }
    struct Error<'c>(usize, E<'c>);
    enum E<'c> {
        Unmatched(&'c str),
        ExpectedRParen(Option<(usize, &'c str)>),
        KeywordAsValue(&'c str),
        SeparatorAsValue(&'c str),
        ExpectedClosing(Option<Tok>, Option<(usize, &'c str)>),
        ExpectedInfixArg(&'c str, Option<(usize, &'c str)>),
        InfixMismatch(&'c str, usize, &'c str),
        ExpectedSeparator(usize, &'c str),
    }
    fn at(i: usize, code: &str) -> String {
        let mut line = 1;
        let mut col = 1;
        for c in code.chars().take(i) {
            if c == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        format!("line {line}, col {col}")
    }
    fn msg<'code>(e: E<'code>, i: usize, code: &'code str) -> String {
        let pos = at(i, code);
        match e {
            E::Unmatched(s) => format!("Expected a value, but found an unmatched '{s}' at {pos}"),
            E::ExpectedRParen(s) => format!(
                "Expected the '(' at {pos} to be followed by a single expression and closed with ')', {}",
                match s {
                    None => "but the code just ended".to_string(),
                    Some((j, s)) => format!("but found '{s}' at {}", at(j, code)),
                }
            ),
            E::KeywordAsValue(s) => {
                format!("Expected a value, but found the keyword '{s}' at {pos}")
            }
            E::SeparatorAsValue(s) => format!(
                "Expected a value, but found the separator '{}' at {pos}",
                s.replace("\n", "\\n")
            ),
            E::ExpectedClosing(expected, actual) => format!(
                "Expected the expressions starting at {pos} to be closed {}, {}",
                match expected {
                    Some(Tok::RParen) => "with ')'",
                    Some(Tok::RBrace) => "with '}'",
                    Some(Tok::RBracket) => "with ']'",
                    _ => "by the end of the code",
                },
                match actual {
                    None => "but the code just ended".to_string(),
                    Some((j, s)) => format!("but found an unmatched '{s}' at {}", at(j, code)),
                }
            ),
            E::ExpectedInfixArg(f, s) => format!(
                "Expected an infix argument after function '{f}' at {pos}, {}",
                match s {
                    None => "but the code just ended".to_string(),
                    Some((j, s)) => format!("but found '{s}' at {}", at(j, code)),
                }
            ),
            E::InfixMismatch(f1, j, f2) => format!(
                "All functions in an infix call must be the same or use (...) to disambiguate precedence, but found '{f1}' at {pos} and then '{f2}' at {}",
                at(j, code),
            ),
            E::ExpectedSeparator(j, s) => format!(
                "Expected a ',' or '\\n' to separate the expressions starting at {pos}, but found '{s}' at {}",
                at(j, code)
            ),
        }
    }
    let mut toks = scan(code).into_iter().peekable();
    match _exprs(&mut toks, 0, None) {
        Ok(elems) => Ok(Prg(elems)),
        Err(Error(i, e)) => Err(msg(e, i, &code)),
    }
}

impl<'code> Ast<'code> {
    fn size(&self) -> usize {
        match self {
            Ast::Var(_) | Ast::String(_) | Ast::Binding(_) => 1,
            Ast::List(xs) | Ast::Block(xs) => xs.iter().map(|x| x.size()).sum::<usize>() + 1,
            Ast::PrefixCall(f, xs) => f.size() + xs.iter().map(|x| x.size()).sum::<usize>(),
            Ast::InfixCall(a, _, xs) => a.size() + 1 + xs.iter().map(|x| x.size()).sum::<usize>(),
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
        let is_multiline = self.size() > 10;
        match self {
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
            Ast::InfixCall(arg, f, args) => {
                if wrap {
                    buf.push('(');
                }
                arg.pretty(buf, lvl, true);
                for arg in args {
                    buf.push(' ');
                    buf.push_str(f);
                    buf.push(' ');
                    arg.pretty(buf, lvl, true);
                }
                if wrap {
                    buf.push(')');
                }
            }
        }
    }
}

impl<'code> std::fmt::Display for Prg<'code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, expr) in self.0.iter().enumerate() {
            if i != 0 {
                f.write_str("\n\n")?;
            }
            let mut buf = String::new();
            expr.pretty(&mut buf, 0, false);
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
    fn parse_error_unmatched() {
        let code = "[)]";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("unmatched ')'"));
        assert!(e.contains("line 1, col 2"));
    }

    #[test]
    fn parse_error_infix_mismatch() {
        let code = "a + b - c";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("'+' at line 1, col 3"));
        assert!(e.contains("'-' at line 1, col 7"));
    }

    #[test]
    fn parse_error_missing_infix_arg() {
        let code = "a +";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("infix argument after function '+' at line 1, col 3"));
    }

    #[test]
    fn parse_error_missing_infix_arg_in_call() {
        let code = "f(a +)";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("infix argument after function '+' at line 1, col 5"));
    }

    #[test]
    fn parse_error_invalid_fn_args() {
        for code in ["f(", "f(a"] {
            let e = parse(code).unwrap_err();
            eprintln!("'{code}': {e}");
            assert!(e.contains("expressions"));
            assert!(e.contains("at line 1, col 2"));
            assert!(e.contains("')'"));
        }
    }

    #[test]
    fn parse_error_fn_args_without_separators() {
        let code = "f([a] [b]";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("',' or '\\n'"));
        assert!(e.contains("'[' at line 1, col 7"));
    }

    #[test]
    fn parse_error_fn_args_without_r_paren() {
        let code = "[f(a]";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("']' at line 1, col 5"));
    }

    #[test]
    fn parse_error_invalid_group() {
        for code in ["(", "(a", "([a] [b])", "([a], [b])"] {
            let e = parse(code).unwrap_err();
            eprintln!("'{code}': {e}");
            assert!(e.contains("'(' at line 1, col 1"));
            assert!(e.contains("single expression"));
            assert!(e.contains("')'"));
        }
    }

    #[test]
    fn parse_error_keyword_as_value() {
        let code = "a + keyword:";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("keyword 'keyword:' at line 1, col 5"));
    }

    #[test]
    fn parse_error_separator_as_value() {
        let code = "a + ,";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("infix argument"));
        assert!(e.contains("',' at line 1, col 5"));
    }

    #[test]
    fn parse_error_list_without_closing_bracket() {
        for code in ["x + [a, b", "x + [a, )"] {
            let e = parse(code).unwrap_err();
            eprintln!("'{code}': {e}");
            assert!(e.contains("closed with ']'"));
            assert!(e.contains("starting at line 1, col 5"));
        }
    }

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
    fn pretty_curried_prefix_call() {
        let code = "f(g(x)(y)())";
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
