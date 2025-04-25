use std::{
    iter::{Peekable, once},
    vec::IntoIter,
};

#[derive(Clone, Copy, PartialEq, Eq)]
enum Tok {
    Symbol,
    Keyword,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Dot,
    Separator,
}

fn scan<'code>(code: &'code str) -> Vec<(usize, &'code str, Tok)> {
    let mut chars = code.chars().chain(once(' ')).enumerate();
    let mut toks = vec![];
    let mut i = 0;
    while let Some((j, c)) = chars.next() {
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
            let curr = &code[i..j];
            match curr.chars().next() {
                Some(first) if first.is_uppercase() => toks.push((i, curr, Tok::Keyword)),
                _ => toks.push((i, curr, Tok::Symbol)),
            }
            i = j + 1;
        }
        if let Some(tok) = tok {
            toks.push((j, &code[j - 1..j], tok));
        }
    }
    toks
}

enum Ast<'code> {
    Nil,
    Var(&'code str),
    Keyword(&'code str),
    List(Vec<Ast<'code>>),
    Block(Vec<Ast<'code>>),
    PrefixCall(Box<Ast<'code>>, Vec<Ast<'code>>),
    PipedCall(Box<Ast<'code>>, Box<Ast<'code>>, Vec<Ast<'code>>),
    InfixCall(Box<Ast<'code>>, &'code str, Box<Ast<'code>>),
}

fn parse<'code>(code: &'code str) -> Result<Ast<'code>, String> {
    type Toks<'code> = Peekable<IntoIter<(usize, &'code str, Tok)>>;
    let mut toks = scan(code).into_iter().peekable();
    fn parse_expr<'code>(toks: &mut Toks<'code>) -> Result<Ast<'code>, Option<String>> {
        let expr = parse_call(toks)?;
        if let Some((_, s, Tok::Symbol)) = toks.peek().copied() {
            toks.next();
            match parse_call(toks) {
                Ok(expr2) => Ok(Ast::InfixCall(Box::new(expr), s, Box::new(expr2))),
                Err(None) => Err(Some(format!(
                    "Expected a second argument after the infix call '{s}'"
                ))),
                Err(e) => Err(e),
            }
        } else {
            Ok(expr)
        }
    }
    fn parse_call<'code>(toks: &mut Toks<'code>) -> Result<Ast<'code>, Option<String>> {
        let expr = parse_value(toks)?;
        if let Some((_, _, Tok::LParen)) = toks.peek().copied() {
            toks.next();
            let args = parse_exprs(toks, Some(Tok::RParen))?;
            Ok(Ast::PrefixCall(Box::new(expr), args))
        } else {
            Ok(expr)
        }
    }
    fn parse_value<'code>(toks: &mut Toks<'code>) -> Result<Ast<'code>, Option<String>> {
        let Some((_, s, t)) = toks.next() else {
            return Err(None);
        };
        match t {
            Tok::Symbol => Ok(Ast::Var(s)),
            Tok::Keyword => Ok(Ast::Keyword(s)),
            Tok::LParen => {
                if let Some((_, _, Tok::RParen)) = toks.peek() {
                    toks.next();
                    return Ok(Ast::Nil);
                }
                let expr = parse_expr(toks)?;
                match toks.next() {
                    None => Err(Some(format!(
                        "Expected a closing ')', but the code just ended"
                    ))),
                    Some((_, _, Tok::RParen)) => Ok(expr),
                    Some((_, s, _)) => {
                        Err(Some(format!("Expected a closing ')', but found '{s}'")))
                    }
                }
            }
            Tok::LBrace => Ok(Ast::Block(parse_exprs(toks, Some(Tok::RBrace))?)),
            Tok::LBracket => Ok(Ast::List(parse_exprs(toks, Some(Tok::RBracket))?)),
            Tok::RParen | Tok::RBrace | Tok::RBracket | Tok::Dot | Tok::Separator => {
                Err(Some(format!("Expected an expression, found '{s}'")))
            }
        }
    }
    fn parse_exprs<'code>(
        toks: &mut Toks<'code>,
        closing_tok: Option<Tok>,
    ) -> Result<Vec<Ast<'code>>, Option<String>> {
        let mut exprs = vec![];
        let mut expect_separator = false;
        loop {
            match toks.peek() {
                Some((_, _, Tok::Separator)) => {
                    toks.next();
                    expect_separator = false;
                }
                t if t.map(|(_, _, t)| *t) == closing_tok => {
                    toks.next();
                    return Ok(exprs);
                }
                _ if expect_separator => {
                    return Err(Some(format!(
                        "Expected a ',' or '\\n' to separate expressions"
                    )));
                }
                _ => {
                    exprs.push(parse_expr(toks)?);
                    expect_separator = true;
                }
            }
        }
    }
    let exprs = parse_exprs(&mut toks, None)
        .map_err(|e| e.unwrap_or_else(|| format!("Expected an expression, but found nothing")))?;
    Ok(Ast::Block(exprs))
}

impl<'code> std::fmt::Display for Ast<'code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
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
    fn eval_fn_app() {
        let code = "fn 'x { x } Foo";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().to_string(), "Foo");
    }

    #[test]
    fn eval_let() {
        let code = "=('x, Foo, { x })";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().to_string(), "Foo");
    }

    #[test]
    fn eval_block() {
        let code = "{ Foo }(Nil)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().to_string(), "Foo");
    }

    #[test]
    fn eval_let_in_block() {
        let code = "'bar = Bar, Foo(bar)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().to_string(), "Foo(Bar)");
    }

    #[test]
    fn eval_multiple_let_in_block() {
        let code = "'x = X, 'y = Y, Pair(x, y)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().to_string(), "Pair(X, Y)");
    }

    #[test]
    fn eval_multiple_let_and_side_effect_in_block() {
        let code = "'x = X, Nil, 'y = Y, Pair(x, y)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().to_string(), "Pair(X, Y)");
    }

    #[test]
    fn eval_simple_if_else() {
        let code = "if-eq(Foo, Foo) { True } { False }";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().to_string(), "True");
    }

    #[test]
    fn eval_rec() {
        let code =
            "'r = rec(fn('r, { Cons(Foo, r) })), pop(r, fn(Pair('xs, 'x), { xs }), { Error })";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().to_string(), "Cons(Foo)");
    }

    #[test]
    fn eval_std_lib() {
        let code = "'x = Foo, if-eq(x, Foo) { True } { False }";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().to_string(), "True");
    }

    #[test]
    fn eval_list() {
        let code = "[Foo(Bar), [Baz, Qux]]";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().to_string(), "[Foo(Bar), [Baz, Qux]]");
    }

    #[test]
    fn eval_nil() {
        let code = "()(Bar, Baz())";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().to_string(), "()(Bar, Baz(()))");
    }

    #[test]
    fn eval_match() {
        let code = include_str!("../examples/eq.vo");
        let parsed = parse(code).unwrap();
        assert_eq!(
            parsed.eval().to_string(),
            "[True, False, True, False, True]"
        );
    }
}
