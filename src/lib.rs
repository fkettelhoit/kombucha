use std::{
    collections::HashMap,
    iter::{Peekable, once},
    slice::Iter,
    sync::Arc,
};

//
// SCANNER
//

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Separator,
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Tag(String),
    Symbol(String),
    Binding(String),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Separator => f.write_str(","),
            Token::ParenOpen => f.write_str("("),
            Token::ParenClose => f.write_str(")"),
            Token::BraceOpen => f.write_str("{"),
            Token::BraceClose => f.write_str("}"),
            Token::Tag(s) | Token::Symbol(s) | Token::Binding(s) => write!(f, "{s}"),
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
            ' ' | '\n' | '(' | ')' | '{' | '}' | ',' => {
                if let Some(first) = curr.chars().next() {
                    if first.is_uppercase() {
                        tokens.push((Token::Tag(curr), start));
                    } else if first == '\'' {
                        tokens.push((Token::Binding(curr[1..].to_string()), start));
                    } else {
                        tokens.push((Token::Symbol(curr), start));
                    }
                    curr = String::new();
                }
                match char {
                    '\n' => {
                        col = 0;
                        line += 1;
                    }
                    ',' => tokens.push((Token::Separator, Pos { col, line })),
                    '(' => tokens.push((Token::ParenOpen, Pos { col, line })),
                    ')' => tokens.push((Token::ParenClose, Pos { col, line })),
                    '{' => tokens.push((Token::BraceOpen, Pos { col, line })),
                    '}' => tokens.push((Token::BraceClose, Pos { col, line })),

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

//
// PARSER
//

#[derive(Debug, Clone)]
pub struct Expr(_Expr, Pool);

#[derive(Debug, Clone)]
struct _Expr(ExprEnum, Id);

#[derive(Debug, Clone)]
enum ExprEnum {
    Var(usize),
    Tag(usize),
    Abs(Box<_Expr>),
    Rec(Box<_Expr>),
    App(Box<_Expr>, Box<_Expr>),
    Cnd {
        value: Box<_Expr>,
        pattern: (usize, usize),
        then_expr: Box<_Expr>,
        else_expr: Box<_Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Id(u32);

impl Id {
    fn internal() -> Self {
        Self(0)
    }
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub fn parse(code: &str) -> Result<Expr, String> {
    let tokens = scan(code);
    let mut tokens = tokens.iter().peekable();
    let Some((t, pos)) = tokens.next() else {
        return Err(format!("Could not parse anything as the input is empty"));
    };
    let mut pool = Pool {
        tags: HashMap::new(),
        bindings: HashMap::new(),
        nodes: HashMap::new(),
    };
    let mut stack = vec!["let".to_string()];
    let expr = parse_expr(t, pos, &mut tokens, &mut pool, &mut stack, &mut vec![])?;
    if let Some((token, pos)) = tokens.next() {
        return Err(format!("Unexpected token {token} at {pos}"));
    } else {
        return Ok(Expr(expr, pool));
    };
    fn parse_expr(
        t: &Token,
        pos: &Pos,
        tokens: &mut Peekable<Iter<(Token, Pos)>>,
        pool: &mut Pool,
        stack: &mut Vec<String>,
        bindings: &mut Vec<String>,
    ) -> Result<_Expr, String> {
        let mut expr = match t {
            Token::Tag(t) => _Expr(
                ExprEnum::Tag(pool.intern_tag(t)),
                pool.register(*pos, Parsed::Tag(t.clone())),
            ),
            Token::Symbol(s) => {
                if let Some((i, _)) = stack.iter().rev().enumerate().find(|(_, var)| s == *var) {
                    _Expr(
                        ExprEnum::Var(i),
                        pool.register(*pos, Parsed::Var(s.clone())),
                    )
                } else {
                    return Err(format!("Could not find binding for '{s}' at {pos}"));
                }
            }
            Token::Binding(b) => {
                bindings.push(b.clone());
                _Expr(
                    ExprEnum::Tag(pool.intern_binding(b)),
                    pool.register(*pos, Parsed::Binding(b.clone())),
                )
            }
            Token::BraceOpen => {
                let Some((t, pos)) = tokens.next() else {
                    return Err(format!(
                        "Expected a block to start after '{{' at {pos}, but found nothing"
                    ));
                };
                // `{ a('x), b('y), c }`
                // desugars to `a('x, { b('y), c })`
                // desugars to `a('x, { b('y, { c }) })`
                let n_bindings = bindings.len();
                stack.extend(bindings.drain(..));
                let mut expr = parse_expr(t, pos, tokens, pool, stack, bindings)?;
                for i in 0..n_bindings {
                    let info = if i == 0 {
                        Parsed::Block
                    } else {
                        Parsed::InnerBlock
                    };
                    expr = _Expr(ExprEnum::Abs(Box::new(expr)), pool.register(*pos, info));
                    stack.pop();
                }
                loop {
                    match tokens.peek() {
                        Some((Token::BraceClose, _)) => {
                            tokens.next();
                            break expr;
                        }
                        Some((Token::Separator, _)) => {
                            tokens.next();
                            stack.extend(bindings.clone());
                            let Some((t, pos)) = tokens.next() else {
                                return Err(format!(
                                    "Expected another expression in the block after ',' at {pos}, but found nothing"
                                ));
                            };
                            let mut block = parse_expr(t, pos, tokens, pool, stack, bindings)?;
                            for i in 0..bindings.len() {
                                let info = if i == 0 {
                                    Parsed::Block
                                } else {
                                    Parsed::InnerBlock
                                };
                                block = _Expr(
                                    ExprEnum::Abs(Box::new(block)),
                                    pool.register(*pos, info),
                                );
                                stack.pop();
                            }
                            expr = _Expr(
                                ExprEnum::App(Box::new(expr), Box::new(block)),
                                pool.register(*pos, Parsed::App),
                            )
                        }
                        t => todo!("unexpected {t:?}"),
                    }
                }
            }
            t => return Err(format!("Unexpected token '{t}' at {pos}")),
        };
        loop {
            match tokens.peek() {
                Some((Token::ParenOpen, pos)) => {
                    tokens.next();
                    let (mut prev_t, mut prev_pos) = (Token::ParenOpen, *pos);
                    expr = loop {
                        let Some((t, pos)) = tokens.next() else {
                            return Err(format!(
                                "Expected a function argument after '{prev_t}' at {prev_pos}, but found nothing"
                            ));
                        };
                        let arg = parse_expr(&t, &pos, tokens, pool, stack, bindings)?;
                        expr = _Expr(
                            ExprEnum::App(Box::new(expr), Box::new(arg)),
                            pool.register(*pos, Parsed::App),
                        );
                        if let Some((Token::ParenClose, _)) = tokens.peek() {
                            tokens.next();
                            break expr;
                        }
                        (prev_t, prev_pos) = (Token::Separator, expect(tokens, Token::Separator)?);
                    }
                }
                _ => break Ok(expr),
            }
        }
    }
    fn expect(tokens: &mut Peekable<Iter<(Token, Pos)>>, tok: Token) -> Result<Pos, String> {
        match tokens.next() {
            Some((t, pos)) if t == &tok => Ok(*pos),
            Some((t, pos)) => Err(format!("Expected '{tok}', found '{t}' at {pos}")),
            None => Err(format!("Expected '{tok}', but found nothing")),
        }
    }
}

#[derive(Debug, Clone)]
struct Pool {
    tags: HashMap<String, usize>,
    bindings: HashMap<String, usize>,
    nodes: HashMap<Id, (Pos, Parsed)>,
}

#[derive(Debug, Clone)]
enum Parsed {
    Var(String),
    Tag(String),
    Binding(String),
    Block,
    App,
    InnerBlock,
}

impl Pool {
    fn next_interned(&self) -> usize {
        self.tags.len() + self.bindings.len()
    }

    fn intern_tag(&mut self, s: &String) -> usize {
        self.tags.get(s).copied().unwrap_or_else(|| {
            let id = self.next_interned();
            self.tags.insert(s.clone(), id);
            id
        })
    }

    fn intern_binding(&mut self, s: &String) -> usize {
        self.bindings.get(s).copied().unwrap_or_else(|| {
            let id = self.next_interned();
            self.bindings.insert(s.clone(), id);
            id
        })
    }

    fn register(&mut self, pos: Pos, info: Parsed) -> Id {
        let id = Id(self.nodes.len() as u32 + 1);
        self.nodes.insert(id, (pos, info));
        id
    }

    fn resolve(&self, id: Id) -> Result<&Parsed, Id> {
        self.nodes.get(&id).map(|(_, info)| info).ok_or(id)
    }
}

//
// PRINT
//

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.sugared() {
            Ok(s) => f.write_str(&s),
            Err(id) => write!(f, "<unprintable_id_{id}>"),
        }
    }
}

impl Expr {
    fn sugared(&self) -> Result<String, Id> {
        self.0.sugar(&self.1)
    }

    fn desugared(&self) -> String {
        self.0.to_string()
    }
}

impl _Expr {
    fn is_simple(&self) -> bool {
        match &self.0 {
            ExprEnum::Var(_) | ExprEnum::Tag(_) => true,
            ExprEnum::Cnd { .. } => false,
            ExprEnum::Abs(body) | ExprEnum::Rec(body) => body.is_simple(),
            ExprEnum::App(f, arg) => f.is_simple() && arg.is_simple(),
        }
    }

    fn sugar(&self, pool: &Pool) -> Result<String, Id> {
        let mut buf = String::new();
        self.pretty(&mut buf, &pool, false, 0)?;
        Ok(buf)
    }

    fn pretty(&self, buf: &mut String, pool: &Pool, wrap: bool, lvl: usize) -> Result<(), Id> {
        let id = self.1;
        fn indent(s: &mut String, lvl: usize) {
            for _ in 0..lvl {
                s.push_str("  ");
            }
        }
        match &self.0 {
            ExprEnum::Var(_) => match pool.resolve(id)? {
                Parsed::Var(var) => {
                    buf.push_str(var);
                    Ok(())
                }
                _ => Err(id),
            },
            ExprEnum::Tag(_) => match pool.resolve(id)? {
                Parsed::Tag(t) => {
                    buf.push_str(t);
                    Ok(())
                }
                Parsed::Binding(b) => {
                    buf.push('\'');
                    buf.push_str(b);
                    Ok(())
                }
                _ => Err(id),
            },
            ExprEnum::Abs(body) => match pool.resolve(id)? {
                Parsed::Block => {
                    buf.push_str("{ ");
                    body.pretty(buf, pool, wrap, lvl)?;
                    buf.push_str(" }");
                    Ok(())
                }
                Parsed::InnerBlock => body.pretty(buf, pool, wrap, lvl),
                _ => Err(id),
            },
            ExprEnum::Rec(_expr) => todo!(),
            ExprEnum::App(f, arg) => {
                let mut args = vec![arg];
                let mut inner = f;
                while let _Expr(ExprEnum::App(expr, arg), _) = inner.as_ref() {
                    inner = expr;
                    args.push(arg);
                }
                inner.pretty(buf, &pool, true, lvl)?;
                buf.push_str("(");
                let is_simple = args.iter().all(|arg| arg.is_simple());
                if !is_simple {
                    buf.push_str("\n");
                    indent(buf, lvl + 1);
                }
                for (i, arg) in args.iter().rev().enumerate() {
                    if i != 0 {
                        if is_simple {
                            buf.push_str(", ");
                        } else {
                            buf.push_str(",\n");
                            indent(buf, lvl + 1);
                        }
                    }
                    if is_simple {
                        arg.pretty(buf, &pool, false, lvl)?;
                    } else {
                        arg.pretty(buf, &pool, false, lvl + 1)?;
                    }
                }
                if !is_simple {
                    buf.push_str("\n");
                    indent(buf, lvl);
                }
                buf.push_str(")");
                Ok(())
            }
            ExprEnum::Cnd {
                value,
                pattern,
                then_expr,
                else_expr,
            } => todo!(),
        }
    }
}

impl std::fmt::Display for _Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            ExprEnum::Var(s) => write!(f, "{s}"),
            ExprEnum::Tag(t) => write!(f, "_{t}"),
            ExprEnum::Abs(expr) => {
                f.write_str("(=> ")?;
                expr.fmt(f)?;
                f.write_str(")")
            }
            ExprEnum::Rec(expr) => {
                f.write_str("(~> ")?;
                expr.fmt(f)?;
                f.write_str(")")
            }
            ExprEnum::App(fun, arg) => {
                fun.fmt(f)?;
                f.write_str("(")?;
                arg.fmt(f)?;
                f.write_str(")")
            }
            ExprEnum::Cnd {
                value,
                pattern: (t, vars),
                then_expr,
                else_expr,
            } => {
                f.write_str("(if ")?;
                value.fmt(f)?;
                write!(f, " is {t}/{vars} then ")?;
                then_expr.fmt(f)?;
                f.write_str(" else ")?;
                else_expr.fmt(f)?;
                f.write_str(")")
            }
        }
    }
}

//
// EVAL
//

impl Expr {
    pub fn eval(self) -> Result<String, String> {
        let empty = Arc::new(Env::Empty);
        let null_id = Id(0);
        // let = (_, x, f) => f(x)
        // desugared: (=> (=> (=> 0(1))))
        let builtin_let = _Value::Fn(
            _Expr(
                ExprEnum::Abs(Box::new(_Expr(
                    ExprEnum::Abs(Box::new(_Expr(
                        ExprEnum::App(
                            Box::new(_Expr(ExprEnum::Var(0), null_id)),
                            Box::new(_Expr(ExprEnum::Var(1), null_id)),
                        ),
                        null_id,
                    ))),
                    null_id,
                ))),
                null_id,
            ),
            Arc::clone(&empty),
            null_id,
        );
        let env = empty.push(builtin_let);
        match self.0.eval(&env) {
            Ok(v) => _Expr::from(v)
                .sugar(&self.1)
                .map_err(|id| format!("Could not print node with id {id}")),
            Err(expr) => Err(format!("Could not eval {expr}")),
        }
    }
}

#[derive(Debug, Clone)]
enum _Value {
    Fn(_Expr, Arc<Env<_Value>>, Id),
    Rec(_Expr, Arc<Env<_Value>>, Id),
    Tag(usize, Vec<_Value>, Id),
}

impl From<_Value> for _Expr {
    fn from(value: _Value) -> Self {
        match value {
            _Value::Fn(body, _, id) => _Expr(ExprEnum::Abs(Box::new(body)), id),
            _Value::Rec(body, _, id) => _Expr(ExprEnum::Rec(Box::new(body)), id),
            _Value::Tag(t, values, id) => {
                let mut expr = _Expr(ExprEnum::Tag(t), id);
                for v in values {
                    expr = _Expr(
                        ExprEnum::App(Box::new(expr), Box::new(_Expr::from(v))),
                        Id::internal(),
                    )
                }
                expr
            }
        }
    }
}

impl std::fmt::Display for _Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            _Value::Fn(body, _, _) => body.fmt(f),
            _Value::Rec(body, _, _) => body.fmt(f),
            _Value::Tag(t, values, id) => {
                _Expr(ExprEnum::Tag(*t), *id).fmt(f)?;
                for arg in values {
                    f.write_str("(")?;
                    arg.fmt(f)?;
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

impl _Expr {
    fn eval(self, env: &Arc<Env<_Value>>) -> Result<_Value, _Expr> {
        match self.0 {
            ExprEnum::Var(var) => env.get(var).cloned().ok_or_else(|| self),
            ExprEnum::Tag(t) => Ok(_Value::Tag(t, vec![], self.1)),
            ExprEnum::Abs(body) => Ok(_Value::Fn(*body, Arc::clone(env), self.1)),
            ExprEnum::Rec(body) => Ok(_Value::Rec(*body, Arc::clone(env), self.1)),
            ExprEnum::App(f, arg) => {
                let mut f = f.eval(env)?;
                loop {
                    match f {
                        _Value::Rec(body, env, meta) => {
                            let env = env.push(_Value::Rec(body.clone(), Arc::clone(&env), meta));
                            f = body.eval(&env)?;
                        }
                        _Value::Tag(t, mut values, meta) => {
                            values.push(arg.eval(env)?);
                            break Ok(_Value::Tag(t, values, meta));
                        }
                        _Value::Fn(body, fn_env, _) => {
                            break body.eval(&fn_env.push(arg.eval(env)?));
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
                let mut value = value.eval(env)?;
                loop {
                    match value {
                        _Value::Rec(body, env, meta) => {
                            let env = env.push(_Value::Rec(body.clone(), Arc::clone(&env), meta));
                            value = body.eval(&env)?;
                        }
                        _Value::Tag(t, values, _) if t == tag && values.len() == vars => {
                            break then_expr.eval(&env.extend(values));
                        }
                        _Value::Tag(_, _, _) | _Value::Fn(_, _, _) => break else_expr.eval(env),
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
    fn eval_let() {
        let code = "let('x, Foo, { x })";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.desugared(), "0(_0)(_1)((=> 0))");
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "Foo");
    }

    #[test]
    fn eval_let_in_block() {
        let code = "{ let('bar, Bar), Foo(bar) }";
        let parsed = parse(code).unwrap();
        assert_eq!("let('bar, Bar, { Foo(bar) })", parsed.to_string());
        assert_eq!(parsed.desugared(), "0(_0)(_1)((=> _2(0)))");
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "Foo(Bar)");
    }

    #[test]
    fn eval_multiple_let_in_block() {
        let code = "{ let('x, Foo), let('y, Bar), Pair(x, y) }";
        let parsed = parse(code).unwrap();
        assert_eq!(
            "let('x, Foo, { let('y, Bar, { Pair(x, y) }) })",
            parsed.to_string()
        );
        assert_eq!(parsed.desugared(), "0(_0)(_1)((=> 0))");
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "Foo");
    }

    // #[test]
    // fn eval_multiple_let_and_side_effect_in_block() {
    //     let code = "{ let('x, Foo), Nil, let('y, Bar), Pair(x, y) }";
    //     todo!()
    // }
}
