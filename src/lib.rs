use std::{
    collections::{HashMap, HashSet},
    iter::{Peekable, once},
    slice::Iter,
    sync::Arc,
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

impl Expr {
    fn new(inner: ExprEnum, alias: Option<String>, pos: Pos) -> Self {
        Self(inner, Meta { alias, pos })
    }
}

#[derive(Debug, Clone)]
enum ExprEnum {
    Var(usize),
    Tag(usize),
    Abs(Box<Expr>),
    Rec(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Cnd {
        value: Box<Expr>,
        pattern: (usize, usize),
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
}

pub fn parse(code: &str) -> Result<Expr, String> {
    fn parse_expr(
        tokens: &mut Peekable<Iter<(Token, Pos)>>,
        vars: &mut Vec<String>,
        tags: &mut HashMap<String, usize>,
    ) -> Result<Expr, String> {
        let Some((token, pos)) = tokens.next() else {
            return Err("Expected an expression, but found nothing".into());
        };
        let mut expr = match token {
            Token::Symbol(s) if is_tag(s) => {
                Expr::new(ExprEnum::Tag(resolve_tag(tags, s)), Some(s.clone()), *pos)
            }
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
                        let (binding, pos) = expect_var(tokens)?;
                        if bound_vars.contains(&binding) {
                            return Err(format!(
                                "All variables in a pattern must be unique, but '{binding} is used twice at {pos}"
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
                let cnd = ExprEnum::Cnd {
                    value: Box::new(v),
                    pattern: (tag, bindings),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                };
                Expr::new(cnd, Some(alias), *pos)
            }
            Token::Symbol(s) if s.as_str() == "let" => {
                let (name, _) = expect_var(tokens)?;
                expect(tokens, Token::Symbol("=".into()))?;
                let binding = parse_expr(tokens, vars, tags)?;
                vars.push(name.clone());
                let body = parse_expr(tokens, vars, tags)?;
                vars.pop();
                let abs = Expr::new(ExprEnum::Abs(Box::new(body)), Some(name.clone()), *pos);
                Expr::new(ExprEnum::App(Box::new(abs), Box::new(binding)), None, *pos)
            }
            Token::Symbol(s) if s.as_str() == "loop" => {
                let (name, _) = expect_var(tokens)?;
                expect(tokens, Token::Symbol("=".into()))?;
                vars.push(name.clone());
                let binding = parse_expr(tokens, vars, tags)?;
                let body = parse_expr(tokens, vars, tags)?;
                vars.pop();
                let abs = Expr::new(ExprEnum::Abs(Box::new(body)), Some(name.clone()), *pos);
                let rec = Expr::new(ExprEnum::Rec(Box::new(binding)), Some(name.clone()), *pos);
                Expr::new(ExprEnum::App(Box::new(abs), Box::new(rec)), None, *pos)
            }
            Token::Symbol(s) => match tokens.peek() {
                Some((Token::Symbol(t), pos)) if t.as_str() == "=>" => {
                    tokens.next();
                    vars.push(s.clone());
                    let body = parse_expr(tokens, vars, tags)?;
                    vars.pop();
                    Expr::new(ExprEnum::Abs(Box::new(body)), Some(s.clone()), *pos)
                }
                Some((Token::Symbol(t), _)) if t.as_str() == "~>" => {
                    tokens.next();
                    vars.push(s.clone());
                    let body = parse_expr(tokens, vars, tags)?;
                    vars.pop();
                    Expr::new(ExprEnum::Rec(Box::new(body)), Some(s.clone()), *pos)
                }
                _ => Expr::new(ExprEnum::Var(resolve_var(vars, s)?), Some(s.clone()), *pos),
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
                    expr = Expr::new(ExprEnum::App(Box::new(expr), Box::new(arg)), None, *pos);
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
    fn expect_var(tokens: &mut Peekable<Iter<(Token, Pos)>>) -> Result<(String, Pos), String> {
        match tokens.next() {
            Some((Token::Symbol(s), pos)) if !is_tag(s) => Ok((s.clone(), *pos)),
            Some((t, pos)) => Err(format!("Expected a variable, found {t} at {pos}")),
            None => Err("Expected a variable, but found nothing".into()),
        }
    }
    fn resolve_var(vars: &[String], s: &str) -> Result<usize, String> {
        for (i, t) in vars.iter().rev().enumerate() {
            if &s == t {
                return Ok(i);
            }
        }
        Err(format!("Could not find binding for symbol '{s}'"))
    }
    fn resolve_tag(tags: &mut HashMap<String, usize>, t: &str) -> usize {
        match tags.get(t) {
            Some(t) => *t,
            None => {
                let n = tags.len();
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

    fn is_kept_inline(&self) -> bool {
        matches!(&self.0, ExprEnum::Abs(_) | ExprEnum::Rec(_))
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
                Some(alias) => f.write_str(alias),
                None => write!(f, "'{s}"),
            },
            ExprEnum::App(expr, arg) => {
                if let Expr(ExprEnum::Abs(body), _) = expr.as_ref() {
                    if let Expr(ExprEnum::Rec(b), _) = arg.as_ref() {
                        // loop
                        match (&expr.1.alias, &arg.1.alias) {
                            (Some(abs_name), Some(rec_name)) if abs_name == rec_name => {
                                write!(f, "loop {abs_name} =")?;
                                if b.is_simple() || b.is_kept_inline() {
                                    f.write_str(" ")?;
                                    b.pretty(f, false, lvl)?;
                                } else {
                                    f.write_str("\n")?;
                                    indent(f, lvl + 1)?;
                                    b.pretty(f, false, lvl + 1)?;
                                }
                                if lvl == 0 {
                                    f.write_str("\n")?;
                                }
                                f.write_str("\n")?;
                                indent(f, lvl)?;
                                return body.pretty(f, false, lvl);
                            }
                            _ => {}
                        }
                    }
                    // let
                    if let Some(name) = &expr.1.alias {
                        write!(f, "let {name} =")?;
                        if arg.is_simple() || arg.is_kept_inline() {
                            f.write_str(" ")?;
                            arg.pretty(f, false, lvl)?;
                        } else {
                            f.write_str("\n")?;
                            indent(f, lvl + 1)?;
                            arg.pretty(f, false, lvl + 1)?;
                        }
                        if lvl == 0 {
                            f.write_str("\n")?;
                        }
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
                if group {
                    f.write_str("(")?;
                }
                let arrow = match self.0 {
                    ExprEnum::Abs(_) => "=>",
                    _ => "~>",
                };
                let var_arrow = match &self.1.alias {
                    Some(var) => format!("{var} {arrow}"),
                    _ => arrow.to_string(),
                };
                if body.is_simple() || body.is_kept_inline() {
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

#[derive(Debug, Clone)]
pub enum Value {
    Fn(Expr, Arc<Env<Value>>, Meta),
    Rec(Expr, Arc<Env<Value>>, Meta),
    Tag(usize, Vec<Value>, Meta),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Fn(body, _, meta) => match &meta.alias {
                Some(alias) => write!(f, "{alias} => {body}"),
                None => write!(f, "=> {body}"),
            },
            Value::Rec(body, _, meta) => match &meta.alias {
                Some(alias) => write!(f, "{alias} ~> {body}"),
                None => write!(f, "~> {body}"),
            },
            Value::Tag(t, values, meta) => {
                match &meta.alias {
                    Some(alias) => f.write_str(alias)?,
                    None => write!(f, "'{t}")?,
                }
                if !values.is_empty() {
                    f.write_str("(")?;
                    for (i, v) in values.iter().enumerate() {
                        if i > 0 {
                            f.write_str(", ")?;
                        }
                        v.fmt(f)?;
                    }
                    f.write_str(")")?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Env<T> {
    Empty,
    Some(T, Arc<Env<T>>),
}

impl<T> Env<T> {
    fn get(&self, var: usize) -> Option<&T> {
        match self {
            Env::Empty => None,
            Env::Some(expr, _) if var == 0 => Some(expr),
            Env::Some(_, env) => env.get(var - 1),
        }
    }

    fn push(self: &Arc<Self>, val: T) -> Arc<Env<T>> {
        Arc::new(Env::Some(val, Arc::clone(self)))
    }

    fn extend(self: &Arc<Self>, vals: Vec<T>) -> Arc<Env<T>> {
        let mut env = Arc::clone(self);
        for v in vals {
            env = env.push(v);
        }
        env
    }
}

#[derive(Clone)]
enum V {
    Fn(Expr, Arc<Env<V>>, Meta),
    Tag(usize, Vec<V>, Meta),
    Any,
}

impl Expr {
    pub fn eval(self) -> Result<Value, String> {
        self.check(&Arc::new(Env::Empty), &[]).map(|_| ())?;
        self._eval(&Arc::new(Env::Empty))
    }

    fn check(&self, env: &Arc<Env<V>>, stack: &[&Expr]) -> Result<Vec<V>, String> {
        // Possible recursion: an expr whose set of possible functions contains `f` is called as a
        // function inside of `f` (in other words while the call stack contains `f`) with arg `f`
        fn stacktrace(stack: &[&Expr]) -> String {
            let mut msg = format!("Implicit recursion detected!");
            for expr in stack {
                if !expr.is_sugared_as_let_or_loop() {
                    let pos = expr.1.pos;
                    msg += &format!("\n\n...at {pos}:\n{expr}");
                }
            }
            msg
        }
        let Expr(inner, meta) = self;
        match inner {
            ExprEnum::Var(var) => match env.get(*var) {
                Some(expr) => Ok(vec![expr.clone()]),
                None => Err(format!("No binding for {self} at {}", meta.pos)),
            },
            ExprEnum::Tag(t) => Ok(vec![V::Tag(*t, vec![], meta.clone())]),
            ExprEnum::Abs(body) => {
                body.check(&env.push(V::Any), stack)?;
                Ok(vec![V::Fn(*body.clone(), Arc::clone(env), meta.clone())])
            }
            ExprEnum::Rec(body) => body.check(&env.push(V::Any), stack),
            ExprEnum::App(f, arg) => {
                let fs = f.check(env, stack)?;
                let args = arg.check(env, stack)?;
                let mut results = vec![];
                for v in fs {
                    for arg in args.iter() {
                        match &v {
                            V::Any => {
                                results.push(V::Any);
                            }
                            V::Tag(t, values, meta) => {
                                let vs = values.iter().cloned().chain([arg.clone()]).collect();
                                results.push(V::Tag(*t, vs, meta.clone()))
                            }
                            V::Fn(body, fn_env, f_meta) => {
                                let mut stack = stack.to_vec();
                                for caller in stack.iter() {
                                    if caller.1.pos == meta.pos {
                                        if let V::Fn(_, _, arg_meta) = arg {
                                            if arg_meta.pos == f_meta.pos {
                                                return Err(stacktrace(&stack));
                                            }
                                        }
                                    }
                                }
                                stack.push(self);
                                let env = fn_env.push(arg.clone());
                                results.extend(body.check(&env, &stack)?);
                            }
                        }
                    }
                }
                Ok(results)
            }
            ExprEnum::Cnd {
                value,
                pattern: (tag, vars),
                then_expr,
                else_expr,
            } => {
                let values = value.check(env, stack)?;
                let mut results = vec![];
                for value in values {
                    let bound = match value {
                        V::Tag(t, values, _) if t == *tag && values.len() == *vars => values,
                        V::Any | V::Tag(_, _, _) | V::Fn(_, _, _) => vec![V::Any; *vars],
                    };
                    results.extend(then_expr.check(&env.extend(bound), stack)?);
                    results.extend(else_expr.check(env, stack)?);
                }
                Ok(results)
            }
        }
    }

    pub fn _eval(self, env: &Arc<Env<Value>>) -> Result<Value, String> {
        let pos = self.1.pos;
        match self.0 {
            ExprEnum::Var(var) => env
                .get(var)
                .cloned()
                .ok_or_else(|| format!("No binding for {self} at {pos}")),
            ExprEnum::Tag(t) => Ok(Value::Tag(t, vec![], self.1.clone())),
            ExprEnum::Abs(body) => Ok(Value::Fn(*body, Arc::clone(env), self.1.clone())),
            ExprEnum::Rec(body) => Ok(Value::Rec(*body, Arc::clone(env), self.1.clone())),
            ExprEnum::App(f, arg) => {
                let mut f = f._eval(env)?;
                loop {
                    match f {
                        Value::Rec(body, env, meta) => {
                            let env = env.push(Value::Rec(body.clone(), Arc::clone(&env), meta));
                            f = body._eval(&env)?;
                        }
                        Value::Tag(t, mut values, meta) => {
                            values.push(arg._eval(env)?);
                            break Ok(Value::Tag(t, values, meta));
                        }
                        Value::Fn(body, fn_env, _) => {
                            break body._eval(&fn_env.push(arg._eval(env)?));
                        }
                    }
                }
            }
            ExprEnum::Cnd {
                value,
                pattern: (tag, vars),
                then_expr,
                else_expr,
            } => {
                let mut value = value._eval(env)?;
                loop {
                    match value {
                        Value::Rec(body, env, meta) => {
                            let env = env.push(Value::Rec(body.clone(), Arc::clone(&env), meta));
                            value = body._eval(&env)?;
                        }
                        Value::Tag(t, values, _) if t == tag && values.len() == vars => {
                            break then_expr._eval(&env.extend(values));
                        }
                        Value::Tag(_, _, _) | Value::Fn(_, _, _) => break else_expr._eval(env),
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_fn_app() {
        let code = "let f = x => Foo(x)

f(Bar)";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "Foo(Bar)");
    }

    #[test]
    fn eval_fn_closure() {
        let code = "let f = x => y => Pair(x, y)

let foo = f(Foo)

foo(Bar)";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "Pair(Foo, Bar)");
    }

    #[test]
    fn eval_rec_fn_app() {
        let code = "loop map = f => xs =>
  if xs is Cons(x, xs) then
    Cons(f(x), map(f, xs))
  else
    xs

map(x => Foo(x), Cons(Bar, Cons(Baz, Nil)))";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "Cons(Foo(Bar), Cons(Foo(Baz), Nil))");
    }

    #[test]
    fn eval_rec_value_cond() {
        let code = "loop r = Cons(Foo, r)

let head = l =>
  if l is Cons(x, xs) then
    x
  else
    Nil

let tail = l =>
  if l is Cons(x, xs) then
    xs
  else
    Nil

head(tail(tail(r)))";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "Foo");
    }

    #[test]
    fn eval_normal_recursion() {
        let code = "loop add = x => y =>
  if x is Suc(x) then
    add(x, Suc(y))
  else
    y

add(Suc(Suc(Zero)), Suc(Zero))";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "Suc(Suc(Suc(Zero)))");
    }

    #[test]
    fn eval_identity_identity() {
        let code = "let identity1 = x => x

let identity2 = x => identity1(x)

identity2(identity2)";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "x => identity1(x)");
    }

    #[test]
    fn eval_twice_identity() {
        let code = "let twice = f => f(f)

let identity = x => x

twice(identity)";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "x => x");
    }

    #[test]
    fn eval_nested_app() {
        let code = "let apply = f => x => f(x)

let identity = x => x

apply(x => apply(identity, x), Foo(Foo(Bar)))";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "Foo(Foo(Bar))");
    }

    #[test]
    fn reject_twice_twice() {
        let code = "let twice = f => f(f)

twice(twice)";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let err = parsed.eval().unwrap_err();
        assert_eq!(
            err,
            "Implicit recursion detected!

...at line 3, col 1:
twice(twice)

...at line 1, col 18:
f(f)"
        );
    }

    #[test]
    fn reject_twice1_twice2() {
        let code = "let twice1 = f => f(f)

let twice2 = f => f(f)

twice1(twice2)";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let err = parsed.eval().unwrap_err();
        assert_eq!(
            err,
            "Implicit recursion detected!

...at line 5, col 1:
twice1(twice2)

...at line 1, col 19:
f(f)

...at line 3, col 19:
f(f)"
        );
    }

    #[test]
    fn reject_twice1_twice2_branch() {
        let code = "let twice1 = f => f(f)

let twice2 = f => f(f)

if Foo(twice2) is Foo(f) then
  twice1(f)
else
  Nothing";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let err = parsed.eval().unwrap_err();
        assert_eq!(
            err,
            "Implicit recursion detected!

...at line 6, col 3:
twice1(f)

...at line 1, col 19:
f(f)

...at line 3, col 19:
f(f)"
        );
    }

    #[test]
    fn reject_fixed_point_combinator() {
        let code = "let fix = f =>
  let x = x => f(x(x))
  f(x(x))

let add' = f => x => y =>
  if x is Suc(x) then
    f(x, Suc(y))
  else
    y

let add = x => y => fix(add', x, y)

add(Suc(Suc(Zero)), Suc(Zero))";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let err = parsed.eval().unwrap_err();
        assert_eq!(
            err,
            "Implicit recursion detected!

...at line 3, col 5:
x(x)

...at line 2, col 18:
x(x)"
        );
    }

    #[test]
    fn reject_fixed_point_combinator_without_app() {
        let code = "f =>
  let x = x => f(x(x))
  f(x(x))";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let err = parsed.eval().unwrap_err();
        assert_eq!(
            err,
            "Implicit recursion detected!

...at line 3, col 5:
x(x)

...at line 2, col 18:
x(x)"
        );
    }

    #[test]
    fn reject_dependent_on_rec1() {
        let code = "let twice = f => f(f)

loop add = x => y =>
  if x is Suc(x) then
    add(x, Suc(y))
  else
    y

if add(Suc(Zero), Zero) is Zero then
  Zero
else
  twice(twice)";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let err = parsed.eval().unwrap_err();
        assert_eq!(
            err,
            "Implicit recursion detected!

...at line 12, col 3:
twice(twice)

...at line 1, col 18:
f(f)"
        );
    }

    #[test]
    fn reject_dependent_on_rec2() {
        let code = "let twice = f => f(f)

loop add = x => y =>
  if x is Suc(x) then
    add(x, Suc(y))
  else
    y

if add(Zero, Zero) is Zero then
  twice(twice)
else
  Zero";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let err = parsed.eval().unwrap_err();
        assert_eq!(
            err,
            "Implicit recursion detected!

...at line 10, col 3:
twice(twice)

...at line 1, col 18:
f(f)"
        );
    }
}
