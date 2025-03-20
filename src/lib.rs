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
enum Token<'code> {
    Separator,
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Tag(&'code str),
    Symbol(&'code str),
    Binding(&'code str),
}

impl std::fmt::Display for Token<'_> {
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
    let mut col = 1;
    let mut line = 1;
    let mut start = Pos { col, line };
    let mut i = 0;
    for (j, char) in code.chars().chain(once(' ')).enumerate() {
        match char {
            ' ' | '\n' | '(' | ')' | '{' | '}' | ',' => {
                if i < j {
                    let curr = &code[i..j];
                    if let Some(first) = curr.chars().next() {
                        if first.is_uppercase() {
                            tokens.push((Token::Tag(curr), start));
                        } else if first == '\'' {
                            tokens.push((Token::Binding(&curr[1..]), start));
                        } else {
                            tokens.push((Token::Symbol(curr), start));
                        }
                    }
                }
                i = j + 1;
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
            _ => {
                col += 1;
            }
        }
    }

    tokens
}

//
// PARSER
//

#[derive(Debug, Clone)]
pub struct Expr<'code>(_Expr, Pool<'code>);

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

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

type Tokens<'a, 'code> = Peekable<Iter<'a, (Token<'code>, Pos)>>;

pub fn parse(code: &str) -> Result<Expr<'_>, String> {
    let tokens = scan(code);
    let mut tokens = tokens.iter().peekable();
    let mut pool = Pool::new();
    let mut stack = vec![];
    let block = parse_block(&mut tokens, &mut pool, &mut stack, &mut vec![])?;
    if let Some((token, pos)) = tokens.next() {
        return Err(format!(
            "Expected the code to end, but found {token} at {pos}"
        ));
    } else if let _Expr(ExprEnum::Abs(block), _) = block {
        return Ok(Expr(*block, pool));
    } else {
        unreachable!("Expected the code to be parsed as a block.");
    };

    fn parse_expr<'code>(
        tokens: &mut Tokens<'_, 'code>,
        pool: &mut Pool<'code>,
        stack: &mut Vec<&'code str>,
        bindings: &mut Vec<&'code str>,
    ) -> Result<(_Expr, Pos), String> {
        let Some((t, pos)) = tokens.next() else {
            return Err("Expected an expression, but the code just ended".to_string());
        };
        fn expect<'code>(
            ts: &mut Tokens<'_, 'code>,
            msg: &str,
            exp: Token<'code>,
        ) -> Result<(), String> {
            match ts.next() {
                Some((t, _)) if t == &exp => Ok(()),
                Some((t, pos)) => Err(format!("{msg}, expected '{exp}', found '{t}' at {pos}")),
                None => Err(format!("{msg}, but the code just ended")),
            }
        }
        let mut pos = *pos;
        let mut expr = match t {
            Token::Tag(t) => _Expr(
                ExprEnum::Tag(pool.intern_tag(t)),
                pool.register(pos, Parsed::Tag(t)),
            ),
            Token::Symbol(s) => {
                if let Some((i, _)) = stack.iter().rev().enumerate().find(|(_, var)| s == *var) {
                    _Expr(ExprEnum::Var(i), pool.register(pos, Parsed::Var(s)))
                } else if *s == "let" {
                    // let('binding, arg, { f })
                    let msg = "The built-in 'let' function must be called correctly";
                    expect(tokens, msg, Token::ParenOpen)?;
                    let (binding, _) = match tokens.next() {
                        Some((Token::Binding(b), _)) => (b, pool.intern_binding(b)),
                        Some((t, pos)) => {
                            return Err(format!("{msg}, expected a binding, found '{t}' at {pos}"));
                        }
                        None => return Err(format!("{msg}, but the code just ended")),
                    };
                    bindings.push(binding);
                    expect(tokens, msg, Token::Separator)?;
                    let (value, _) = parse_expr(tokens, pool, stack, bindings)?;
                    match tokens.next() {
                        Some((Token::ParenClose, _)) => _Expr(
                            ExprEnum::Abs(Box::new(_Expr(
                                ExprEnum::App(
                                    Box::new(_Expr(
                                        ExprEnum::Var(0),
                                        pool.register(pos, Parsed::Hidden),
                                    )),
                                    Box::new(value),
                                ),
                                pool.register(pos, Parsed::AppHiddenFun),
                            ))),
                            pool.register(pos, Parsed::PrimitiveLet(binding)),
                        ),
                        Some((Token::Separator, _)) => {
                            expect(tokens, msg, Token::BraceOpen)?;
                            let f = parse_block(tokens, pool, stack, bindings)?;
                            expect(tokens, msg, Token::BraceClose)?;
                            expect(tokens, msg, Token::ParenClose)?;
                            _Expr(
                                ExprEnum::App(Box::new(f), Box::new(value)),
                                pool.register(pos, Parsed::PrimitiveLet(binding)),
                            )
                        }
                        Some((t, pos)) => {
                            return Err(format!(
                                "{msg}, expected ',' or ')', found '{t}' at {pos}"
                            ));
                        }
                        None => return Err(format!("{msg}, but the code just ended")),
                    }
                } else if *s == "rec" {
                    // rec('binding, { arg })
                    let msg = "The built-in 'rec' function must be called correctly";
                    expect(tokens, msg, Token::ParenOpen)?;
                    let (binding, _) = match tokens.next() {
                        Some((Token::Binding(b), _)) => (b, pool.intern_binding(b)),
                        Some((t, pos)) => {
                            return Err(format!("{msg}, expected a binding, found '{t}' at {pos}"));
                        }
                        None => return Err(format!("{msg}, but the code just ended")),
                    };
                    bindings.push(binding);
                    expect(tokens, msg, Token::Separator)?;
                    expect(tokens, msg, Token::BraceOpen)?;
                    let body = parse_block(tokens, pool, stack, bindings)?;
                    expect(tokens, msg, Token::BraceClose)?;
                    expect(tokens, msg, Token::ParenClose)?;
                    _Expr(
                        ExprEnum::Rec(Box::new(body)),
                        pool.register(pos, Parsed::PrimitiveRec(binding)),
                    )
                } else if *s == "if" {
                    // if(v, Tag, { then }, { else })
                    let msg = "The built-in 'if' function must be called correctly";
                    expect(tokens, msg, Token::ParenOpen)?;
                    let (value, _) = parse_expr(tokens, pool, stack, bindings)?;
                    expect(tokens, msg, Token::Separator)?;
                    let (tag, tag_id) = match tokens.next() {
                        Some((Token::Tag(t), _)) => (t, pool.intern_tag(t)),
                        Some((t, pos)) => {
                            return Err(format!("{msg}, expected a tag, found '{t}' at {pos}"));
                        }
                        None => return Err(format!("{msg}, but the code just ended")),
                    };
                    let mut pattern_vars = vec![];
                    match tokens.next() {
                        Some((Token::Separator, _)) => {}
                        Some((Token::ParenOpen, _)) => loop {
                            let (binding, _) = match tokens.next() {
                                Some((Token::Binding(b), _)) => (b, pool.intern_binding(b)),
                                Some((t, pos)) => {
                                    return Err(format!(
                                        "{msg}, expected a binding, found '{t}' at {pos}"
                                    ));
                                }
                                None => return Err(format!("{msg}, but the code just ended")),
                            };
                            if pattern_vars.contains(binding) {
                                return Err(format!(
                                    "All variables in a pattern must be unique, but '{binding} is used twice at {pos}"
                                ));
                            }
                            pattern_vars.push(*binding);
                            match tokens.next() {
                                Some((Token::ParenClose, _)) => {
                                    expect(tokens, msg, Token::Separator)?;
                                    break;
                                }
                                Some((Token::Separator, _)) => {}
                                Some((t, pos)) => {
                                    return Err(format!(
                                        "{msg}, expected ',' or ')', found '{t}' at {pos}"
                                    ));
                                }
                                None => return Err(format!("{msg}, but the code just ended")),
                            }
                        },
                        Some((t, pos)) => {
                            return Err(format!("{msg}, expected a tag, found '{t}' at {pos}"));
                        }
                        None => return Err(format!("{msg}, but the code just ended")),
                    }
                    expect(tokens, msg, Token::BraceOpen)?;
                    let vars = pattern_vars.len();
                    bindings.extend(pattern_vars.iter());
                    let mut then_expr = parse_block(tokens, pool, stack, bindings)?;
                    for _ in 0..vars {
                        bindings.pop();
                    }
                    if vars == 0 {
                        then_expr = _Expr(
                            ExprEnum::App(
                                Box::new(then_expr),
                                Box::new(_Expr(
                                    ExprEnum::Tag(Pool::nil()),
                                    pool.register(pos, Parsed::Hidden),
                                )),
                            ),
                            pool.register(pos, Parsed::AppHiddenArg),
                        );
                    } else {
                        for i in (0..vars).rev() {
                            then_expr = _Expr(
                                ExprEnum::App(
                                    Box::new(then_expr),
                                    Box::new(_Expr(
                                        ExprEnum::Var(i),
                                        pool.register(pos, Parsed::Hidden),
                                    )),
                                ),
                                pool.register(pos, Parsed::AppHiddenArg),
                            );
                        }
                    }
                    expect(tokens, msg, Token::BraceClose)?;
                    expect(tokens, msg, Token::Separator)?;
                    expect(tokens, msg, Token::BraceOpen)?;
                    let mut else_expr = parse_block(tokens, pool, stack, bindings)?;
                    else_expr = _Expr(
                        ExprEnum::App(
                            Box::new(else_expr),
                            Box::new(_Expr(ExprEnum::Tag(0), pool.register(pos, Parsed::Hidden))),
                        ),
                        pool.register(pos, Parsed::AppHiddenArg),
                    );
                    expect(tokens, msg, Token::BraceClose)?;
                    expect(tokens, msg, Token::ParenClose)?;
                    _Expr(
                        ExprEnum::Cnd {
                            value: Box::new(value),
                            pattern: (tag_id, vars),
                            then_expr: Box::new(then_expr),
                            else_expr: Box::new(else_expr),
                        },
                        pool.register(pos, Parsed::PrimitiveIf(tag, pattern_vars)),
                    )
                } else {
                    return Err(format!("Could not find binding for '{s}' at {pos}"));
                }
            }
            Token::Binding(b) => {
                bindings.push(b);
                _Expr(
                    ExprEnum::Tag(pool.intern_binding(b)),
                    pool.register(pos, Parsed::Binding(b)),
                )
            }
            Token::BraceOpen => {
                let block = parse_block(tokens, pool, stack, bindings)?;
                match tokens.next() {
                    Some(_) => block,
                    None => {
                        return Err(format!(
                            "Expected the block starting at {pos} to be closed with '}}', but found nothing"
                        ));
                    }
                }
            }
            t => return Err(format!("Unexpected token '{t}' at {pos}")),
        };
        loop {
            match tokens.peek() {
                Some((Token::ParenOpen, _)) => {
                    tokens.next();
                    (expr, pos) = loop {
                        let (arg, pos) = parse_expr(tokens, pool, stack, bindings)?;
                        expr = _Expr(
                            ExprEnum::App(Box::new(expr), Box::new(arg)),
                            pool.register(pos, Parsed::App),
                        );
                        match tokens.peek() {
                            Some((Token::ParenClose, _)) => {
                                tokens.next();
                                break (expr, pos);
                            }
                            Some((Token::Separator, _)) => {
                                tokens.next();
                            }
                            Some((t, pos)) => {
                                return Err(format!(
                                    "Expected ',' or ')', but found '{t}' at {pos}"
                                ));
                            }
                            None => {
                                return Err(
                                    "Expected ',' or ')', but the code just ended".to_string()
                                );
                            }
                        }
                    }
                }
                _ => break Ok((expr, pos)),
            }
        }
    }

    fn parse_block<'code>(
        tokens: &mut Tokens<'_, 'code>,
        pool: &mut Pool<'code>,
        stack: &mut Vec<&'code str>,
        bindings: &mut Vec<&'code str>,
    ) -> Result<_Expr, String> {
        let mut exprs = vec![];
        loop {
            let mut args = bindings.len();
            if args == 0 {
                args += 1;
                stack.push("");
            } else {
                stack.append(bindings);
            }
            let (block, pos) = parse_expr(tokens, pool, stack, bindings)?;
            exprs.push((args, block, bindings.len()));
            match tokens.peek() {
                Some((Token::Separator, _)) => {
                    tokens.next();
                }
                Some((Token::BraceClose, _)) | None => {
                    // `{ a('x), b('y), c }`
                    // desugars to `a('x, { b('y), c })`
                    // desugars to `a('x, { b('y, { c }) })`
                    let mut next = None;
                    for (j, (args, mut expr, args_next)) in exprs.into_iter().enumerate().rev() {
                        if let Some(block) = next {
                            if args_next == 0 {
                                // { f(), ... }
                                // <==> (_ => ...)(f())
                                expr = _Expr(
                                    ExprEnum::App(Box::new(block), Box::new(expr)),
                                    pool.register(pos, Parsed::ValueAppInBlock),
                                )
                            } else {
                                // { let('x, Foo), ... }
                                // <==> let('x, Foo)(x => ...)
                                expr = _Expr(
                                    ExprEnum::App(Box::new(expr), Box::new(block)),
                                    pool.register(pos, Parsed::BindingAppInBlock),
                                )
                            }
                        }
                        for i in 0..args {
                            let info = if i == 0 && j == 0 {
                                Parsed::Block
                            } else {
                                Parsed::Hidden
                            };
                            expr = _Expr(ExprEnum::Abs(Box::new(expr)), pool.register(pos, info));
                            for _ in 0..args {
                                stack.pop();
                            }
                        }
                        next = Some(expr)
                    }
                    break Ok(next.unwrap());
                }
                Some((t, pos)) => {
                    return Err(format!(
                        "Expected block to continue with ',' or '}}', but found {t} at {pos}"
                    ));
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Pool<'code> {
    tags: HashMap<&'code str, usize>,
    bindings: HashMap<&'code str, usize>,
    nodes: HashMap<Id, (Pos, Parsed<'code>)>,
}

#[derive(Debug, Clone)]
enum Parsed<'code> {
    Var(&'code str),
    Tag(&'code str),
    Binding(&'code str),
    Block,
    App,
    Hidden,
    BindingAppInBlock,
    ValueAppInBlock,
    PrimitiveLet(&'code str),
    PrimitiveIf(&'code str, Vec<&'code str>),
    AppHiddenArg,
    AppHiddenFun,
    PrimitiveRec(&'code str),
}

impl<'code> Pool<'code> {
    fn new() -> Self {
        Self {
            tags: HashMap::from_iter(vec![("Nil", 0)]),
            bindings: HashMap::new(),
            nodes: HashMap::new(),
        }
    }

    fn nil() -> usize {
        0
    }

    fn next_interned(&self) -> usize {
        self.tags.len() + self.bindings.len()
    }

    fn intern_tag(&mut self, s: &'code str) -> usize {
        self.tags.get(s).copied().unwrap_or_else(|| {
            let id = self.next_interned();
            self.tags.insert(s, id);
            id
        })
    }

    fn intern_binding(&mut self, s: &'code str) -> usize {
        self.bindings.get(s).copied().unwrap_or_else(|| {
            let id = self.next_interned();
            self.bindings.insert(s, id);
            id
        })
    }

    fn register(&mut self, pos: Pos, info: Parsed<'code>) -> Id {
        let id = Id(self.nodes.len() as u32 + 1);
        self.nodes.insert(id, (pos, info));
        id
    }

    fn resolve(&self, id: Id) -> Result<&Parsed<'code>, Id> {
        self.nodes.get(&id).map(|(_, info)| info).ok_or(id)
    }
}

//
// PRINT
//

impl std::fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.sugared() {
            Ok(s) => f.write_str(&s),
            Err(id) => write!(f, "<unprintable_id_{id}>"),
        }
    }
}

impl Expr<'_> {
    fn sugared(&self) -> Result<String, &_Expr> {
        self.0.sugar(&self.1)
    }
}

impl<'code> _Expr {
    fn is_simple(&self) -> bool {
        match &self.0 {
            ExprEnum::Var(_) | ExprEnum::Tag(_) => true,
            ExprEnum::Cnd { .. } => false,
            ExprEnum::Abs(body) | ExprEnum::Rec(body) => body.is_simple(),
            ExprEnum::App(f, arg) => f.is_simple() && arg.is_simple(),
        }
    }

    fn sugar(&self, pool: &Pool<'code>) -> Result<String, &_Expr> {
        let mut buf = String::new();
        self.pretty(&mut buf, pool, false, 0)?;
        Ok(buf)
    }

    fn pretty(
        &self,
        buf: &mut String,
        pool: &Pool<'code>,
        _wrap: bool,
        lvl: usize,
    ) -> Result<(), &_Expr> {
        fn indent(s: &mut String, lvl: usize) {
            for _ in 0..lvl {
                s.push_str("  ");
            }
        }
        let Ok(info) = pool.resolve(self.1) else {
            return Err(self);
        };
        match &self.0 {
            ExprEnum::Var(_) => match info {
                Parsed::Var(var) => {
                    buf.push_str(var);
                    Ok(())
                }
                _ => Err(self),
            },
            ExprEnum::Tag(_) => match info {
                Parsed::Tag(t) => {
                    buf.push_str(t);
                    Ok(())
                }
                Parsed::Binding(b) => {
                    buf.push('\'');
                    buf.push_str(b);
                    Ok(())
                }
                _ => Err(self),
            },
            ExprEnum::Abs(body) => match info {
                Parsed::PrimitiveLet(binding) => {
                    buf.push_str("let('");
                    buf.push_str(binding);
                    buf.push_str(", ");
                    body.pretty(buf, pool, _wrap, lvl)?;
                    buf.push(')');
                    Ok(())
                }
                Parsed::Block => {
                    buf.push_str("{ ");
                    body.pretty(buf, pool, _wrap, lvl)?;
                    buf.push_str(" }");
                    Ok(())
                }
                Parsed::Hidden => body.pretty(buf, pool, _wrap, lvl),
                _ => Err(self),
            },
            ExprEnum::Rec(body) => match info {
                Parsed::PrimitiveRec(binding) => {
                    buf.push_str("rec('");
                    buf.push_str(binding);
                    buf.push_str(", ");
                    body.pretty(buf, pool, _wrap, lvl)?;
                    buf.push(')');
                    Ok(())
                }
                _ => Err(self),
            },
            ExprEnum::App(f, arg) => match info {
                Parsed::PrimitiveLet(binding) => {
                    buf.push_str("let('");
                    buf.push_str(binding);
                    buf.push_str(", ");
                    arg.pretty(buf, pool, _wrap, lvl)?;
                    buf.push_str(", ");
                    f.pretty(buf, pool, _wrap, lvl)?;
                    buf.push(')');
                    Ok(())
                }
                Parsed::BindingAppInBlock => {
                    f.pretty(buf, pool, _wrap, lvl)?;
                    buf.push_str(", ");
                    arg.pretty(buf, pool, _wrap, lvl)
                }
                Parsed::ValueAppInBlock => {
                    arg.pretty(buf, pool, _wrap, lvl)?;
                    buf.push_str(", ");
                    f.pretty(buf, pool, _wrap, lvl)
                }
                Parsed::AppHiddenArg => f.pretty(buf, pool, _wrap, lvl),
                Parsed::AppHiddenFun => arg.pretty(buf, pool, _wrap, lvl),
                Parsed::App | Parsed::Tag(_) => {
                    let mut args = vec![arg];
                    let mut inner = f;
                    while let _Expr(ExprEnum::App(expr, arg), _) = inner.as_ref() {
                        inner = expr;
                        args.push(arg);
                    }
                    inner.pretty(buf, pool, true, lvl)?;
                    buf.push('(');
                    let is_simple = args.iter().all(|arg| arg.is_simple());
                    if !is_simple {
                        buf.push('\n');
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
                            arg.pretty(buf, pool, false, lvl)?;
                        } else {
                            arg.pretty(buf, pool, false, lvl + 1)?;
                        }
                    }
                    if !is_simple {
                        buf.push('\n');
                        indent(buf, lvl);
                    }
                    buf.push(')');
                    Ok(())
                }
                _ => Err(self),
            },
            ExprEnum::Cnd {
                value,
                pattern: _,
                then_expr,
                else_expr,
            } => match info {
                Parsed::PrimitiveIf(tag, vars) => {
                    buf.push_str("if(");
                    value.pretty(buf, pool, false, lvl)?;
                    buf.push_str(", ");
                    buf.push_str(tag);
                    if !vars.is_empty() {
                        buf.push('(');
                        for (i, var) in vars.iter().enumerate() {
                            if i != 0 {
                                buf.push_str(", ")
                            }
                            buf.push('\'');
                            buf.push_str(var);
                        }
                        buf.push(')');
                    }
                    buf.push_str(", ");
                    then_expr.pretty(buf, pool, false, lvl)?;
                    buf.push_str(", ");
                    else_expr.pretty(buf, pool, false, lvl)?;
                    buf.push(')');
                    Ok(())
                }
                _ => Err(self),
            },
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

impl Expr<'_> {
    pub fn eval(self) -> Result<String, String> {
        match self.0.eval(&Arc::new(Env::Empty)) {
            Ok(v) => _Expr::from(v)
                .sugar(&self.1)
                .map_err(|node| format!("Could not print node with id {}: {node}", node.1)),
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
                    expr = _Expr(ExprEnum::App(Box::new(expr), Box::new(_Expr::from(v))), id)
                }
                expr
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
            ExprEnum::Var(var) => env.get(var).cloned().ok_or(self),
            ExprEnum::Tag(t) => Ok(_Value::Tag(t, vec![], self.1)),
            ExprEnum::Abs(body) => Ok(_Value::Fn(*body, Arc::clone(env), self.1)),
            ExprEnum::Rec(body) => Ok(_Value::Rec(*body, Arc::clone(env), self.1)),
            ExprEnum::App(f, arg) => {
                let mut f = f.eval(env)?;
                loop {
                    match f {
                        _Value::Rec(body, env, id) => {
                            let env = env.push(_Value::Rec(body.clone(), Arc::clone(&env), id));
                            f = body.eval(&env)?;
                        }
                        _Value::Tag(t, mut values, id) => {
                            values.push(arg.eval(env)?);
                            break Ok(_Value::Tag(t, values, id));
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
                        _Value::Rec(body, env, id) => {
                            let env = env.push(_Value::Rec(body.clone(), Arc::clone(&env), id));
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
        assert_eq!(parsed.0.to_string(), "(=> 0)(_2)");
        assert_eq!(parsed.eval().unwrap(), "Foo");
    }

    #[test]
    fn eval_block() {
        let code = "{ Foo }";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.0.to_string(), "(=> _1)");
        assert_eq!(parsed.eval().unwrap(), "{ Foo }");
    }

    #[test]
    fn eval_let_in_block() {
        let code = "let('bar, Bar), Foo(bar)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.0.to_string(), "(=> 0(_2))((=> _3(0)))");
        assert_eq!(parsed.eval().unwrap(), "Foo(Bar)");
    }

    #[test]
    fn eval_multiple_let_in_block() {
        let code = "let('x, X), let('y, Y), Pair(x, y)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "Pair(X, Y)");
    }

    #[test]
    fn eval_multiple_let_and_side_effect_in_block() {
        let code = "let('x, X), Nil, let('y, Y), Pair(x, y)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "Pair(X, Y)");
    }

    #[test]
    fn eval_simple_if_else() {
        let code = "if(Foo, Foo, { True }, { False })";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "True");
    }

    #[test]
    fn eval_complex_if_else() {
        let code = "if(Pair(Foo, Bar), Pair('x, 'y), { x }, { Nil })";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "Foo");
    }

    #[test]
    fn eval_rec() {
        let code = "rec('r, { Cons(Foo, r) })";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "rec('r, { Cons(Foo, r) })");
    }
}
