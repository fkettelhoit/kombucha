use std::{
    collections::{HashMap, HashSet},
    iter::{Peekable, once},
    slice::Iter,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Comma,
    LParen,
    RParen,
    Symbol(String),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Comma => f.write_str(","),
            Token::LParen => f.write_str("("),
            Token::RParen => f.write_str(")"),
            Token::Symbol(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Pos {
    line: usize,
    col: usize,
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, col {}", self.line, self.col)
    }
}

fn scan(code: &str) -> Vec<(Token, Pos)> {
    let mut tokens = vec![];
    let mut curr = String::new();
    let mut col = 1;
    let mut line = 1;
    let mut start = Pos { col, line };
    for char in code.chars().chain(once(' ')) {
        match char {
            ' ' | '\n' | '(' | ')' | ',' => {
                if !curr.is_empty() {
                    tokens.push((Token::Symbol(curr), start));
                    curr = String::new();
                }
                match char {
                    '\n' => {
                        col = 0;
                        line += 1;
                    }
                    ',' => tokens.push((Token::Comma, Pos { col, line })),
                    '(' => tokens.push((Token::LParen, Pos { col, line })),
                    ')' => tokens.push((Token::RParen, Pos { col, line })),
                    _ => {}
                }
                col += 1;
                start = Pos { col, line };
            }
            c => {
                col += 1;
                curr.push(c);
            }
        }
    }
    tokens
}

#[derive(Debug, Clone)]
pub struct Meta {
    alias: Option<String>,
    pos: Pos,
}

#[derive(Debug, Clone)]
pub struct Expr(ExprEnum, Meta);

#[derive(Debug, Clone)]
enum ExprEnum {
    Var(u32),
    Tag(u32),
    Abs(Box<Expr>),
    Rec(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Cnd {
        value: Box<Expr>,
        pattern: (u32, u32),
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
}

pub fn parse(code: &str) -> Result<Expr, String> {
    fn parse_expr(
        tokens: &mut Peekable<Iter<(Token, Pos)>>,
        vars: &mut Vec<String>,
        tags: &mut HashMap<String, u32>,
    ) -> Result<Expr, String> {
        let Some((token, pos)) = tokens.next() else {
            return Err("Expected an expression, but found nothing".into());
        };
        let mut expr = match token {
            Token::Symbol(s) if is_tag(s) => Expr(
                ExprEnum::Tag(resolve_tag(tags, s)),
                Meta {
                    alias: Some(s.clone()),
                    pos: *pos,
                },
            ),
            Token::Symbol(s) if s.as_str() == "if" => {
                let v = parse_expr(tokens, vars, tags)?;
                expect(tokens, Token::Symbol("is".into()))?;
                let (tag, mut alias) = match tokens.next() {
                    Some((Token::Symbol(t), _)) if is_tag(t) => (resolve_tag(tags, t), t.clone()),
                    Some((t, pos)) => {
                        return Err(format!(
                            "Expected the pattern to start with a tag, but found {t} at {pos}"
                        ));
                    }
                    None => return Err("Expected a pattern, but found nothing".into()),
                };
                let mut bindings = 0;
                if let Some((Token::LParen, _)) = tokens.peek() {
                    tokens.next();
                    let mut bound_vars = HashSet::new();
                    alias += "(";
                    loop {
                        let (binding, pos) = match tokens.next() {
                            Some((Token::Symbol(s), pos)) if !is_tag(s) => (s.clone(), *pos),
                            Some((t, pos)) => {
                                return Err(format!("Expected a variable, found '{t}' at {pos}"));
                            }
                            None => return Err("Expected a variable, but found nothing".into()),
                        };
                        if bound_vars.contains(&binding) {
                            return Err(format!(
                                "All bound variables in a pattern must be unique, but '{binding} is used twice at {pos}"
                            ));
                        }
                        alias += &binding;
                        bound_vars.insert(binding.clone());
                        vars.push(binding.clone());
                        bindings += 1;
                        match tokens.next() {
                            Some((Token::RParen, _)) => {
                                alias += ")";
                                break;
                            }
                            Some((Token::Comma, _)) => alias += ", ",
                            Some((t, pos)) => {
                                return Err(format!(
                                    "Expected ',' or ')', but found '{t}' at {pos}"
                                ));
                            }
                            None => return Err("Expected a pattern, but found nothing".into()),
                        }
                    }
                }
                expect(tokens, Token::Symbol("then".into()))?;
                let then_expr = parse_expr(tokens, vars, tags)?;
                expect(tokens, Token::Symbol("else".into()))?;
                for _ in 0..bindings {
                    vars.pop();
                }
                let else_expr = parse_expr(tokens, vars, tags)?;
                Expr(
                    ExprEnum::Cnd {
                        value: Box::new(v),
                        pattern: (tag, bindings),
                        then_expr: Box::new(then_expr),
                        else_expr: Box::new(else_expr),
                    },
                    Meta {
                        alias: Some(alias),
                        pos: *pos,
                    },
                )
            }
            Token::Symbol(s) => match tokens.peek() {
                Some((Token::Symbol(t), pos)) if t.as_str() == "=>" => {
                    tokens.next();
                    vars.push(s.clone());
                    let body = parse_expr(tokens, vars, tags)?;
                    vars.pop();
                    Expr(
                        ExprEnum::Abs(Box::new(body)),
                        Meta {
                            alias: Some(s.clone()),
                            pos: *pos,
                        },
                    )
                }
                Some((Token::Symbol(t), _)) if t.as_str() == "~>" => {
                    tokens.next();
                    vars.push(s.clone());
                    let body = parse_expr(tokens, vars, tags)?;
                    vars.pop();
                    Expr(
                        ExprEnum::Rec(Box::new(body)),
                        Meta {
                            alias: Some(s.clone()),
                            pos: *pos,
                        },
                    )
                }
                _ => Expr(
                    ExprEnum::Var(resolve_var(vars, s)?),
                    Meta {
                        alias: Some(s.clone()),
                        pos: *pos,
                    },
                ),
            },
            Token::LParen => {
                let expr = parse_expr(tokens, vars, tags)?;
                expect(tokens, Token::RParen)?;
                expr
            }
            t => return Err(format!("Unexpected token '{t}' at {pos}",)),
        };
        let expr = match tokens.peek() {
            Some((Token::LParen, _)) => {
                tokens.next();
                loop {
                    let arg = parse_expr(tokens, vars, tags)?;
                    expr = Expr(
                        ExprEnum::App(Box::new(expr), Box::new(arg)),
                        Meta {
                            alias: None,
                            pos: *pos,
                        },
                    );
                    if let Some((Token::RParen, _)) = tokens.peek() {
                        tokens.next();
                        break expr;
                    }
                    expect(tokens, Token::Comma)?;
                }
            }
            _ => expr,
        };
        Ok(expr)
    }
    fn expect(tokens: &mut Peekable<Iter<(Token, Pos)>>, tok: Token) -> Result<(), String> {
        match tokens.next() {
            Some((t, _)) if t == &tok => Ok(()),
            Some((t, pos)) => Err(format!("Expected '{tok}', found '{t}' at {pos}")),
            None => Err(format!("Expected '{tok}', but found nothing")),
        }
    }
    fn resolve_var(vars: &[String], s: &str) -> Result<u32, String> {
        for (i, t) in vars.iter().rev().enumerate() {
            if &s == t {
                return Ok(i as u32);
            }
        }
        Err(format!("Could not find binding for symbol '{s}'"))
    }
    fn resolve_tag(tags: &mut HashMap<String, u32>, t: &str) -> u32 {
        match tags.get(t) {
            Some(t) => *t,
            None => {
                let n = tags.len() as u32;
                tags.insert(t.to_string(), n);
                n
            }
        }
    }
    fn is_tag(s: &str) -> bool {
        s.chars().next().unwrap_or_default().is_uppercase()
    }
    let tokens = scan(code);
    let mut tokens = tokens.iter().peekable();
    let expr = parse_expr(&mut tokens, &mut vec![], &mut HashMap::new())?;
    if let Some((token, pos)) = tokens.next() {
        Err(format!("Unexpected token {token} at {pos}"))
    } else {
        Ok(expr)
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty(f, false, 0)
    }
}

impl Expr {
    fn is_sugared_as_let_or_loop(&self) -> bool {
        match &self.0 {
            ExprEnum::App(f, _) if matches!(f.as_ref(), Expr(ExprEnum::Abs(_), _)) => true,
            _ => false,
        }
    }

    fn is_simple(&self) -> bool {
        if self.is_sugared_as_let_or_loop() {
            return false;
        }
        match &self.0 {
            ExprEnum::Var(_) | ExprEnum::Tag(_) => true,
            ExprEnum::Cnd { .. } => false,
            ExprEnum::Abs(body) | ExprEnum::Rec(body) => body.is_simple(),
            ExprEnum::App(f, arg) => f.is_simple() && arg.is_simple(),
        }
    }

    fn pretty(&self, f: &mut std::fmt::Formatter<'_>, group: bool, lvl: usize) -> std::fmt::Result {
        fn indent(f: &mut std::fmt::Formatter<'_>, level: usize) -> std::fmt::Result {
            for _ in 0..level {
                f.write_str("  ")?;
            }
            Ok(())
        }
        match &self.0 {
            ExprEnum::Var(s) | ExprEnum::Tag(s) => match &self.1.alias {
                Some(alias) => f.write_str(&alias),
                _ => write!(f, "'{s}"),
            },
            ExprEnum::App(expr, arg) => {
                if let Expr(ExprEnum::Abs(body), _) = expr.as_ref() {
                    if let Expr(ExprEnum::Rec(b), _) = arg.as_ref() {
                        // loop
                        match (&expr.1.alias, &arg.1.alias) {
                            (Some(abs_name), Some(rec_name)) if abs_name == rec_name => {
                                write!(f, "loop {abs_name} = ")?;
                                arg.pretty(f, false, lvl)?;
                                f.write_str("\n")?;
                                indent(f, lvl + 1)?;
                                b.pretty(f, false, lvl + 1)?;
                                f.write_str("\n\n")?;
                                indent(f, lvl)?;
                                return body.pretty(f, false, lvl);
                            }
                            _ => {}
                        }
                    }
                    // let
                    if let Some(name) = &expr.1.alias {
                        write!(f, "let {name} = ")?;
                        arg.pretty(f, false, lvl)?;
                        f.write_str("\n")?;
                        indent(f, lvl)?;
                        return body.pretty(f, false, lvl);
                    }
                }

                let mut args = vec![arg];
                let mut inner = expr;
                while let Expr(ExprEnum::App(expr, arg), _) = inner.as_ref() {
                    inner = expr;
                    args.push(arg);
                }
                inner.pretty(f, true, lvl)?;
                f.write_str("(")?;
                let is_simple = args.iter().all(|arg| arg.is_simple());
                if !is_simple {
                    f.write_str("\n")?;
                    indent(f, lvl + 1)?;
                }
                for (i, arg) in args.iter().rev().enumerate() {
                    if i != 0 {
                        if is_simple {
                            f.write_str(", ")?;
                        } else {
                            f.write_str(",\n")?;
                            indent(f, lvl + 1)?;
                        }
                    }
                    if is_simple {
                        arg.pretty(f, false, lvl)?;
                    } else {
                        arg.pretty(f, false, lvl + 1)?;
                    }
                }
                if !is_simple {
                    f.write_str("\n")?;
                    indent(f, lvl)?;
                }
                write!(f, ")")
            }
            ExprEnum::Abs(body) | ExprEnum::Rec(body) => {
                let arrow = match self.0 {
                    ExprEnum::Abs(_) => "=>",
                    _ => "~>",
                };
                let var_arrow = match &self.1.alias {
                    Some(var) => format!("{var} {arrow}"),
                    _ => arrow.to_string(),
                };
                if group {
                    f.write_str("(")?;
                }
                if body.is_simple() {
                    write!(f, "{var_arrow} ")?;
                    body.pretty(f, false, lvl)?;
                } else {
                    write!(f, "{var_arrow}\n")?;
                    indent(f, lvl + 1)?;
                    body.pretty(f, false, lvl + 1)?;
                }
                if group {
                    if !body.is_simple() {
                        f.write_str("\n")?;
                        indent(f, lvl)?;
                    }
                    f.write_str(")")?;
                }
                Ok(())
            }
            ExprEnum::Cnd {
                value,
                pattern,
                then_expr,
                else_expr,
            } => {
                f.write_str("if ")?;
                value.pretty(f, false, lvl)?;
                f.write_str(" is ")?;
                if let Some(alias) = &self.1.alias {
                    f.write_str(alias)?;
                } else {
                    let (tag, bindings) = pattern;
                    write!(f, "'{tag} {bindings}")?;
                }
                f.write_str(" then\n")?;
                indent(f, lvl + 1)?;
                then_expr.pretty(f, false, lvl + 1)?;
                f.write_str("\n")?;
                indent(f, lvl)?;
                f.write_str("else\n")?;
                indent(f, lvl + 1)?;
                else_expr.pretty(f, false, lvl + 1)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let code = "x => Foo(x)";
        let parsed = parse(code).unwrap();
        assert_eq!(format!("{parsed}"), code);
    }
}
