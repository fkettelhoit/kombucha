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
    Block(Vec<Ast<'code>>),
    PrefixCall(Box<Ast<'code>>, Vec<Ast<'code>>),
    InfixCall(&'code str, Vec<Ast<'code>>),
    KeywordCall(Vec<(&'code str, Ast<'code>)>),
}

#[derive(Debug, Clone)]
pub struct Prg<'code>(Vec<Ast<'code>>);

pub fn parse<'code>(code: &'code str) -> Result<Prg<'code>, String> {
    struct E<'c>(usize, Option<(usize, &'c str)>, Exp<'c>);
    enum Exp<'c> {
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
        let Some((Tok::Keyword, _, _)) = toks.peek().copied() else {
            return _infix(toks);
        };
        let mut args = vec![];
        while let Some((Tok::Keyword, i, s)) = toks.peek().copied() {
            toks.next();
            match _infix(toks) {
                Ok(arg) => args.push((s, arg)),
                Err(E(_, tok, Exp::Value)) => return Err(E(i, tok, Exp::KeywordArg(s))),
                Err(e) => return Err(e),
            }
        }
        Ok(Ast::KeywordCall(args))
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
                return Err(E(i, Some((j, s)), Exp::InfixFn(f)));
            }
            match _prefix(toks) {
                Ok(arg) => args.push(arg),
                Err(E(_, tok, Exp::Value)) => return Err(E(j, tok, Exp::InfixArg(s))),
                Err(e) => return Err(e),
            }
        }
        Ok(Ast::InfixCall(f, args))
    }
    fn _prefix<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, E<'c>> {
        let mut expr = _value(toks)?;
        while let Some((Tok::LParen, i, _)) = toks.peek().copied() {
            toks.next();
            expr = Ast::PrefixCall(Box::new(expr), _exprs(toks, i, Some(Tok::RParen))?);
        }
        Ok(expr)
    }
    fn _value<'c>(toks: &mut Toks<'c>) -> Result<Ast<'c>, E<'c>> {
        match toks.next() {
            None => Err(E(0, None, Exp::Value)),
            Some((t, i, s)) => match t {
                Tok::Symbol => Ok(Ast::Var(s)),
                Tok::String => Ok(Ast::String(s)),
                Tok::Binding => Ok(Ast::Binding(s)),
                Tok::LParen => match _expr(toks) {
                    Err(E(i, tok, Exp::Value)) => Err(E(i, tok, Exp::RParen)),
                    Err(e) => Err(e),
                    Ok(expr) => match toks.next() {
                        Some((Tok::RParen, _, _)) => Ok(expr),
                        tok => Err(E(i, tok.map(|(_, i, s)| (i, s)), Exp::RParen)),
                    },
                },
                Tok::LBrace => Ok(Ast::Block(_exprs(toks, i, Some(Tok::RBrace))?)),
                Tok::RParen | Tok::RBrace | Tok::Separator | Tok::Keyword => {
                    Err(E(i, Some((i, s)), Exp::Value))
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
                Some((_, i, s)) if needs_sep => return Err(E(i, Some((i, s)), Exp::Sep)),
                _ => match _expr(toks) {
                    Ok(expr) => {
                        exprs.push(expr);
                        needs_sep = true;
                    }
                    Err(E(_, tok, Exp::Value)) => match t {
                        Some(Tok::RParen) => return Err(E(i, tok, Exp::RParen)),
                        _ => return Err(E(i, tok, Exp::RBrace)),
                    },
                    Err(e) => return Err(e),
                },
            }
        }
    }
    let mut toks = scan(code).into_iter().peekable();
    match _exprs(&mut toks, 0, None) {
        Ok(exprs) => Ok(Prg(exprs)),
        Err(E(i, actual, expected)) => {
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
            let p = pos_at(i, code);
            let expected = match expected {
                Exp::RParen => format!("the '(' at {p} to be closed with ')'"),
                Exp::RBrace => format!("the '{{' at {p} to be closed with '}}'"),
                Exp::Sep => format!("a ',' or '\\n' to separate the expressions starting at {p}"),
                Exp::Value => format!("a value"),
                Exp::InfixFn(f) => format!("all infix functions starting at {p} to be named '{f}'"),
                Exp::InfixArg(f) => format!("an infix argument after the function '{f}' at {p}"),
                Exp::KeywordArg(k) => format!("an argument after the keyword '{k}' at {p}"),
            };
            let instead = match actual {
                None => "but the code just ended".to_string(),
                Some((j, s)) => format!("but found '{s}' at {}", pos_at(j, code)),
            };
            Err(format!("Expected {expected}, {instead}"))
        }
    }
}

impl<'code> Ast<'code> {
    fn size(&self) -> usize {
        match self {
            Ast::Var(_) | Ast::String(_) | Ast::Binding(_) => 1,
            Ast::Block(xs) => xs.iter().map(|x| x.size()).sum::<usize>() + 1,
            Ast::PrefixCall(f, xs) => f.size() + xs.iter().map(|x| x.size()).sum::<usize>(),
            Ast::InfixCall(_, xs) => 1 + xs.iter().map(|x| x.size()).sum::<usize>(),
            Ast::KeywordCall(xs) => xs.iter().map(|(_, x)| x.size() + 1).sum(),
        }
    }

    fn pretty(&self, lvl: usize) -> String {
        fn one_line<'c>(xs: &[Ast<'c>], lvl: usize) -> String {
            let xs = xs.iter().map(|x| x.pretty(lvl));
            xs.collect::<Vec<_>>().join(", ")
        }
        fn multi_line<'c>(xs: &[Ast<'c>], lvl: usize) -> String {
            let xs = xs.iter().map(|x| "  ".repeat(lvl) + &x.pretty(lvl) + "\n");
            "\n".to_string() + &xs.collect::<String>() + &"  ".repeat(lvl - 1)
        }
        match (self, self.size() >= 10) {
            (Ast::Var(s) | Ast::String(s) | Ast::Binding(s), _) => s.to_string(),
            (Ast::Block(xs), false) => "{ ".to_string() + &one_line(xs, lvl + 1) + " }",
            (Ast::Block(xs), true) => "{".to_string() + &multi_line(xs, lvl + 1) + "}",
            (Ast::PrefixCall(f, xs), false) => f.pretty(lvl) + "(" + &one_line(xs, lvl + 1) + ")",
            (Ast::PrefixCall(f, xs), true) => f.pretty(lvl) + "(" + &multi_line(xs, lvl + 1) + ")",
            (Ast::InfixCall(f, args), _) => {
                let args = args.iter().map(|a| match a {
                    Ast::KeywordCall(_) | Ast::InfixCall(_, _) => format!("({})", a.pretty(lvl)),
                    _ => a.pretty(lvl),
                });
                args.collect::<Vec<_>>().join(&format!(" {f} "))
            }
            (Ast::KeywordCall(elems), _) => {
                let args = elems.iter().map(|(k, arg)| match arg {
                    Ast::KeywordCall(_) => format!("{k} ({})", arg.pretty(lvl)),
                    _ => format!("{k} {}", arg.pretty(lvl)),
                });
                args.collect::<Vec<_>>().join(&format!(" "))
            }
        }
    }
}

impl<'code> std::fmt::Display for Prg<'code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elems = self.0.iter().map(|x| x.pretty(0));
        f.write_str(&elems.collect::<Vec<_>>().join("\n\n"))
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
        let code = "{)}";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("')'"));
        assert!(e.contains("line 1, col 2"));
    }

    #[test]
    fn parse_error_infix_mismatch() {
        let code = "a + b - c";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("all infix functions starting at line 1, col 3 to be named '+'"));
        assert!(e.contains("'-' at line 1, col 7"));
    }

    #[test]
    fn parse_error_missing_infix_arg() {
        let code = "a +";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("infix argument after the function '+' at line 1, col 3"));
    }

    #[test]
    fn parse_error_missing_infix_arg_in_call() {
        let code = "f(a +)";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("infix argument after the function '+' at line 1, col 5"));
    }

    #[test]
    fn parse_error_invalid_fn_args() {
        for code in ["f(", "f(a"] {
            let e = parse(code).unwrap_err();
            eprintln!("'{code}': {e}");
            assert!(e.contains("at line 1, col 2"));
            assert!(e.contains("')'"));
        }
    }

    #[test]
    fn parse_error_fn_args_without_separators() {
        let code = "f({a} {b}";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("',' or '\\n'"));
        assert!(e.contains("'{' at line 1, col 7"));
    }

    #[test]
    fn parse_error_fn_args_without_r_paren() {
        let code = "{f(a}";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("'}' at line 1, col 5"));
    }

    #[test]
    fn parse_error_invalid_group() {
        for code in ["(", "(a", "({a} {b})", "({a}, {b})"] {
            let e = parse(code).unwrap_err();
            eprintln!("'{code}': {e}");
            assert!(e.contains("'(' at line 1, col 1"));
            assert!(e.contains("')'"));
        }
    }

    #[test]
    fn parse_error_keyword_as_value() {
        let code = "a + keyword:";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("infix argument"));
        assert!(e.contains("'keyword:' at line 1, col 5"));
    }

    #[test]
    fn parse_error_double_keyword() {
        let code = "foo: bar:";
        let e = parse(code).unwrap_err();
        eprintln!("{e}");
        assert!(e.contains("argument"));
        assert!(e.contains("'bar:' at line 1, col 6"));
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
        for code in ["x + {a, b", "x + {a, )"] {
            let e = parse(code).unwrap_err();
            eprintln!("'{code}': {e}");
            assert!(e.contains("closed with '}'"));
            assert!(e.contains("at line 1, col 5"));
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
        let code = "{ foo, Foo, 'foo }";
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
        let code = "(a * b) + (c * d)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_keyword_call() {
        let code = "if: x == y do: { foo() } else: Bar";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty_grouped_keyword_call() {
        let code = "if: x == y do: (foo: Bar baz: Qux) else: Bar";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }

    #[test]
    fn pretty() {
        let code = "'foo = Foo

'id = ('x => { x })

f'('x, 'y) = {
  match: Pair(x, y) with: Vec(
    Pair('x, 'x) => { x }
    Pair('x, 'y) => { Mismatch }
    ' => { InvalidPair }
  )
}

if: x == y do: { print(Equal) } else: { print(NotEqual) }

'x = id(id(foo))

if: x == Foo do: Pair(x, x) else: Error";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
    }
}
