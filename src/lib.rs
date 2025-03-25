use std::{
    collections::HashMap,
    iter::{Peekable, once},
    slice::Iter,
    sync::Arc,
};

//
// PRELUDE (PRE-DEFINED FUNCTIONS)
//

const PRELUDE: &'static [(&str, &str)] = &[
    (
        "=",
        "fn 'binding {
            fn 'val {
                fn 'block {
                    block(val)
                }
            }
        }",
    ),
    (
        "==",
        "fn(Args('x, 'y, 'then, 'else)) {
            pop(x, fn(Args('xx, 'x), {
                pop(y, fn(Args('yy, 'y), {
                    if-tag(xx, {
                        if-tag(yy, {
                            if-eq(x, y, then, else)
                        }, else)
                    }, else)
                }), else)
            }), else)
        }",
    ),
];

//
// SCANNER
//

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token<'code> {
    Separator(Sep),
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Tag(&'code str),
    Symbol(&'code str),
    Binding(&'code str),
    Reserved(&'code str),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Sep {
    Comma,
    Newline,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Separator(Sep::Comma) => f.write_str(","),
            Token::Separator(Sep::Newline) => f.write_str("\\n"),
            Token::ParenOpen => f.write_str("("),
            Token::ParenClose => f.write_str(")"),
            Token::BraceOpen => f.write_str("{"),
            Token::BraceClose => f.write_str("}"),
            Token::Tag(s) | Token::Symbol(s) => write!(f, "{s}"),
            Token::Binding(s) => write!(f, "'{s}"),
            Token::Reserved(s) => write!(f, "\\{s}"),
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
                        } else if first == '\\' {
                            tokens.push((Token::Reserved(&curr[1..]), start));
                        } else {
                            tokens.push((Token::Symbol(curr), start));
                        }
                    }
                }
                i = j + 1;
                match char {
                    '\n' => {
                        tokens.push((Token::Separator(Sep::Newline), Pos { col, line }));
                        col = 0;
                        line += 1;
                    }
                    ',' => tokens.push((Token::Separator(Sep::Comma), Pos { col, line })),
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
pub struct Ast<'code>(AstEnum<'code>, Pos);

#[derive(Debug, Clone)]
enum AstEnum<'code> {
    Tag(&'code str),
    Symbol(&'code str, usize),
    Binding(&'code str),
    Call(Box<Ast<'code>>, Vec<Ast<'code>>),
    InfixCall(Box<Ast<'code>>, Box<Ast<'code>>, Vec<Ast<'code>>),
    Block(
        (usize, Box<Ast<'code>>, usize),
        Vec<(usize, Ast<'code>, usize)>,
    ),
    BuiltInFn(&'code str),
    BuiltInRec(&'code str, Box<Ast<'code>>),
    BuiltInPop(&'code str),
    BuiltInIf(&'code str),
    BuiltInIfCoreTag(&'code str, &'static str),
}

type Tokens<'a, 'code> = Peekable<Iter<'a, (Token<'code>, Pos)>>;

pub fn parse(code: &str) -> Result<Ast<'_>, String> {
    let tokens = scan(code);
    let mut tokens = tokens.iter().peekable();
    let mut bindings = PRELUDE.iter().map(|(name, _)| *name).collect::<Vec<_>>();
    let pos = Pos { line: 1, col: 1 };
    let block = parse_block(pos, &mut tokens, &mut vec![], &mut bindings)?;
    if let Some((token, pos)) = tokens.next() {
        return Err(format!(
            "Expected the code to end, but found {token} at {pos}"
        ));
    }
    return Ok(block);
}

fn parse_block<'code>(
    pos: Pos,
    tokens: &mut Tokens<'_, 'code>,
    stack: &mut Vec<&'code str>,
    bindings: &mut Vec<&'code str>,
) -> Result<Ast<'code>, String> {
    if let Some((Token::Separator(_), _)) = tokens.peek() {
        tokens.next();
    }
    let mut args = bindings.len();
    if args == 0 {
        args += 1;
        stack.push("");
    } else {
        stack.append(bindings);
    }
    let mut all_args = args;
    let expr = parse_expr(tokens, stack, bindings)?;
    let first = (args, Box::new(expr), bindings.len());
    let mut rest = vec![];
    loop {
        match tokens.peek() {
            Some((Token::Separator(_), _)) => {
                tokens.next();
            }
            Some((Token::BraceClose, _)) | None => {
                bindings.clear();
                for _ in 0..all_args {
                    stack.pop();
                }
                return Ok(Ast(AstEnum::Block(first, rest), pos));
            }
            _ => {
                let mut args = bindings.len();
                if args == 0 {
                    args += 1;
                    stack.push("");
                } else {
                    stack.append(bindings);
                }
                all_args += args;
                let expr = parse_expr(tokens, stack, bindings)?;
                rest.push((args, expr, bindings.len()));
            }
        }
    }
}

fn parse_expr<'code>(
    tokens: &mut Tokens<'_, 'code>,
    stack: &mut Vec<&'code str>,
    bindings: &mut Vec<&'code str>,
) -> Result<Ast<'code>, String> {
    let Some((t, pos)) = tokens.next() else {
        return Err("Expected an expression, but the code just ended".to_string());
    };
    let pos = *pos;
    let mut expr = match t {
        Token::Tag(t) => Ast(AstEnum::Tag(t), pos),
        Token::Binding(b) => {
            bindings.push(b);
            Ast(AstEnum::Binding(b), pos)
        }
        Token::BraceOpen => {
            let block = parse_block(pos, tokens, stack, bindings)?;
            match tokens.next() {
                Some(_) => block,
                None => return Err(unexpected_block_end(pos)),
            }
        }
        Token::Symbol(s) => parse_symbol(tokens, stack, bindings, *s, pos)?,
        t => return Err(format!("Unexpected token '{t}' at {pos}")),
    };
    let mut infix = None;
    let mut arg_bindings = vec![];
    loop {
        match tokens.peek() {
            Some((Token::ParenOpen, _)) => {
                tokens.next();
                if let Some((Token::Separator(_), _)) = tokens.peek() {
                    tokens.next();
                }
                let mut args = vec![];
                expr = loop {
                    args.push(parse_expr(tokens, stack, &mut arg_bindings)?);
                    match tokens.peek() {
                        Some((Token::ParenClose, _)) => {
                            tokens.next();
                            if let Some(f) = infix {
                                let call = AstEnum::InfixCall(Box::new(expr), Box::new(f), args);
                                infix = None;
                                break Ast(call, pos);
                            } else {
                                break Ast(AstEnum::Call(Box::new(expr), args), pos);
                            }
                        }
                        Some((Token::Separator(_), _)) => {
                            tokens.next();
                        }
                        Some((t, pos)) => {
                            return Err(format!("Expected ',' or ')', but found '{t}' at {pos}"));
                        }
                        None => {
                            return Err("Expected ',' or ')', but the code just ended".to_string());
                        }
                    }
                }
            }
            Some((t @ (Token::Tag(_) | Token::Binding(_) | Token::BraceOpen), arg_pos)) => {
                tokens.next();
                let arg = match t {
                    Token::Tag(t) => Ast(AstEnum::Tag(t), *arg_pos),
                    Token::Binding(b) => {
                        arg_bindings.push(b);
                        Ast(AstEnum::Binding(b), *arg_pos)
                    }
                    Token::BraceOpen => {
                        let block = parse_block(*arg_pos, tokens, stack, &mut arg_bindings)?;
                        match tokens.next() {
                            Some(_) => block,
                            None => return Err(unexpected_block_end(pos)),
                        }
                    }
                    _ => unreachable!(),
                };
                if let Some(f) = infix {
                    let call = AstEnum::InfixCall(Box::new(expr), Box::new(f), vec![arg]);
                    expr = Ast(call, pos);
                    infix = None;
                } else {
                    expr = Ast(AstEnum::Call(Box::new(expr), vec![arg]), pos);
                }
            }
            Some((Token::Symbol(s), pos)) => {
                tokens.next();
                infix = Some(parse_symbol(tokens, stack, bindings, s, *pos)?);
            }
            _ if infix.is_some() => {
                return Err(format!(
                    "Expected an argument to the right side of infix call starting at {pos}"
                ));
            }
            _ => {
                bindings.append(&mut arg_bindings);
                return Ok(expr);
            }
        }
    }
    fn parse_symbol<'code>(
        tokens: &mut Tokens<'_, 'code>,
        stack: &mut Vec<&'code str>,
        bindings: &mut Vec<&'code str>,
        s: &'code str,
        pos: Pos,
    ) -> Result<Ast<'code>, String> {
        if let Some((i, _)) = stack.iter().rev().enumerate().find(|(_, var)| &s == *var) {
            Ok(Ast(AstEnum::Symbol(s, i), pos))
        } else if s == "fn" {
            Ok(Ast(AstEnum::BuiltInFn(s), pos))
        } else if s == "rec" {
            // rec('var, { body })
            let msg = "The built-in 'rec' must be called as rec('var, { body })";
            expect(tokens, msg, Token::ParenOpen)?;
            let (var, _) = expect_binding(tokens, msg)?;
            bindings.push(var);
            match tokens.next() {
                Some((Token::Separator(_), _)) => {}
                Some((tok, pos)) => {
                    return Err(format!(
                        "{msg}, expected ',' or '\n', found '{tok}' at {pos}"
                    ));
                }
                None => return Err(format!("{msg}, but the code just ended")),
            }
            let block_pos = expect(tokens, msg, Token::BraceOpen)?;
            let body = parse_block(block_pos, tokens, stack, bindings)?;
            expect(tokens, msg, Token::BraceClose)?;
            expect(tokens, msg, Token::ParenClose)?;
            Ok(Ast(AstEnum::BuiltInRec(var, Box::new(body)), pos))
        } else if s == "pop" {
            Ok(Ast(AstEnum::BuiltInPop(s), pos))
        } else if s == "if-eq" {
            Ok(Ast(AstEnum::BuiltInIf(s), pos))
        } else if s == "if-tag" {
            Ok(Ast(AstEnum::BuiltInIfCoreTag(s, Pool::T_TAG), pos))
        } else if s == "if-binding" {
            Ok(Ast(AstEnum::BuiltInIfCoreTag(s, Pool::T_BINDING), pos))
        } else {
            return Err(format!("Could not find binding for '{s}' at {pos}"));
        }
    }
    fn expect(toks: &mut Tokens<'_, '_>, msg: &str, t: Token<'_>) -> Result<Pos, String> {
        match toks.next() {
            Some((tok, pos)) if tok == &t => Ok(*pos),
            Some((tok, pos)) => Err(format!("{msg}, expected '{t}', found '{tok}' at {pos}")),
            None => Err(format!("{msg}, but the code just ended")),
        }
    }
    fn expect_binding<'code>(
        toks: &mut Tokens<'_, 'code>,
        msg: &str,
    ) -> Result<(&'code str, Pos), String> {
        match toks.next() {
            Some((Token::Binding(b), pos)) => Ok((b, *pos)),
            Some((tok, pos)) => Err(format!("{msg}, expected binding, found '{tok}' at {pos}")),
            None => Err(format!("{msg}, but the code just ended")),
        }
    }
    fn unexpected_block_end(pos: Pos) -> String {
        format!(
            "Expected the block starting at {pos} to be closed with '}}', but the code just ended"
        )
    }
}

impl<'code> Ast<'code> {
    fn to_expr(&self, pool: &mut Pool<'code>) -> Expr {
        let pos = self.1;
        match &self.0 {
            AstEnum::Tag(t) => Expr::app(
                pool.new_id(pos),
                Expr::tag(pool.new_id(pos), pool.intern_core(Pool::T_TAG)),
                Expr::tag(pool.new_id(pos), pool.intern_tag(t)),
            ),
            AstEnum::Binding(b) => Expr::app(
                pool.new_id(pos),
                Expr::tag(pool.new_id(pos), pool.intern_core(Pool::T_BINDING)),
                Expr::tag(pool.new_id(pos), pool.intern_binding(b)),
            ),
            AstEnum::Symbol(_, i) => Expr::var(pool.new_id(pos), *i),
            AstEnum::Call(f, args) => {
                let mut expr = f.to_expr(pool);
                for arg in args {
                    expr = Expr::app(pool.new_id(pos), expr, arg.to_expr(pool));
                }
                expr
            }
            AstEnum::InfixCall(a, f, args) => {
                let mut expr = Expr::app(pool.new_id(pos), f.to_expr(pool), a.to_expr(pool));
                for arg in args {
                    expr = Expr::app(pool.new_id(pos), expr, arg.to_expr(pool));
                }
                expr
            }
            AstEnum::Block((args, first, bindings_left), rest) => {
                // `{ a('x), b('y), c }`
                // desugars to `a('x, { b('y), c })`
                // desugars to `a('x, { b('y, { c }) })`
                let mut next = None;
                let mut exprs = vec![(*args, first.as_ref(), *bindings_left)];
                for (args, expr, bindings_left) in rest {
                    exprs.push((*args, expr, *bindings_left));
                }
                for (args, expr, args_next) in exprs.into_iter().rev() {
                    let mut expr = expr.to_expr(pool);
                    if let Some(block) = next {
                        if args_next == 0 {
                            // { f(), ... }
                            // <==> (_ => ...)(f())
                            expr = Expr::app(pool.new_id(pos), block, expr);
                        } else {
                            // { let('x, Foo), ... }
                            // <==> let('x, Foo)(x => ...)
                            expr = Expr::app(pool.new_id(pos), expr, block);
                        }
                    }
                    for _ in 0..args {
                        expr = Expr::abs(pool.new_id(pos), expr);
                    }
                    next = Some(expr)
                }
                next.unwrap()
            }
            AstEnum::BuiltInFn(_) => Expr::abs(
                pool.new_id(pos),
                Expr::abs(pool.new_id(pos), Expr::var(pool.new_id(pos), 0)),
            ),
            AstEnum::BuiltInRec(_, body) => Expr::rec(pool.new_id(pos), body.to_expr(pool)),
            AstEnum::BuiltInPop(_) => Expr::abs(
                pool.new_id(pos),
                Expr::abs(
                    pool.new_id(pos),
                    Expr::abs(
                        pool.new_id(pos),
                        Expr::builtin_pop(
                            pool.new_id(pos),
                            Expr::var(pool.new_id(pos), 2),
                            Expr::var(pool.new_id(pos), 1),
                            Expr::app(
                                pool.new_id(pos),
                                Expr::var(pool.new_id(pos), 0),
                                Expr::tag(pool.new_id(pos), pool.intern_core(Pool::T_NIL)),
                            ),
                        ),
                    ),
                ),
            ),
            AstEnum::BuiltInIf(_) => Expr::abs(
                pool.new_id(pos),
                Expr::abs(
                    pool.new_id(pos),
                    Expr::abs(
                        pool.new_id(pos),
                        Expr::abs(
                            pool.new_id(pos),
                            Expr::builtin_if(
                                pool.new_id(pos),
                                Expr::var(pool.new_id(pos), 3),
                                Expr::var(pool.new_id(pos), 2),
                                Expr::app(
                                    pool.new_id(pos),
                                    Expr::var(pool.new_id(pos), 1),
                                    Expr::tag(pool.new_id(pos), pool.intern_core(Pool::T_NIL)),
                                ),
                                Expr::app(
                                    pool.new_id(pos),
                                    Expr::var(pool.new_id(pos), 0),
                                    Expr::tag(pool.new_id(pos), pool.intern_core(Pool::T_NIL)),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            AstEnum::BuiltInIfCoreTag(s, tag) => {
                let if_expr = Ast(AstEnum::BuiltInIf(s), pos);
                let tag = Expr::tag(pool.new_id(pos), pool.intern_core(tag));
                Expr::app(pool.new_id(pos), if_expr.to_expr(pool), tag)
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Expr(ExprEnum, Id);

#[derive(Debug, Clone)]
enum ExprEnum {
    Var(usize),
    Tag(usize),
    Abs(Box<Expr>),
    Rec(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Pop(Box<Expr>, Box<Expr>, Box<Expr>),
    If((Box<Expr>, Box<Expr>), Box<Expr>, Box<Expr>),
}

impl Expr {
    fn var(id: Id, var: usize) -> Self {
        Self(ExprEnum::Var(var), id)
    }

    fn tag(id: Id, tag: usize) -> Self {
        Self(ExprEnum::Tag(tag), id)
    }

    fn abs(id: Id, body: Expr) -> Self {
        Self(ExprEnum::Abs(Box::new(body)), id)
    }

    fn rec(id: Id, body: Expr) -> Self {
        Self(ExprEnum::Rec(Box::new(body)), id)
    }

    fn app(id: Id, f: Expr, arg: Expr) -> Self {
        Self(ExprEnum::App(Box::new(f), Box::new(arg)), id)
    }

    fn builtin_pop(id: Id, v: Expr, then_expr: Expr, else_expr: Expr) -> Self {
        Self(
            ExprEnum::Pop(Box::new(v), Box::new(then_expr), Box::new(else_expr)),
            id,
        )
    }

    fn builtin_if(id: Id, a: Expr, b: Expr, then_expr: Expr, else_expr: Expr) -> Self {
        Self(
            ExprEnum::If(
                (Box::new(a), Box::new(b)),
                Box::new(then_expr),
                Box::new(else_expr),
            ),
            id,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Id(u32);

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone)]
struct Pool<'code> {
    core: HashMap<&'code str, usize>,
    tags: HashMap<&'code str, usize>,
    bindings: HashMap<&'code str, usize>,
    nodes: HashMap<Id, Pos>,
}

impl<'code> Pool<'code> {
    const T_NIL: &'static str = "nil";
    const T_TAG: &'static str = "tag";
    const T_BINDING: &'static str = "bind";

    fn new() -> Self {
        Self {
            core: HashMap::new(),
            tags: HashMap::from_iter(vec![("nil", 0)]),
            bindings: HashMap::new(),
            nodes: HashMap::new(),
        }
    }

    fn next_interned(&self) -> usize {
        self.core.len() + self.tags.len() + self.bindings.len()
    }

    fn intern_core(&mut self, s: &'code str) -> usize {
        self.core.get(s).copied().unwrap_or_else(|| {
            let id = self.next_interned();
            self.core.insert(s, id);
            id
        })
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

    fn new_id(&mut self, pos: Pos) -> Id {
        let id = Id(self.nodes.len() as u32 + 1);
        self.nodes.insert(id, pos);
        id
    }
}

//
// PRINT
//

impl std::fmt::Display for Ast<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        match &self.0 {
            AstEnum::Block((_, first, _), rest) => {
                first.pretty(&mut buf, false, 0);
                for (_, expr, _) in rest {
                    buf.push_str(", ");
                    expr.pretty(&mut buf, false, 0);
                }
            }
            _ => {
                self.pretty(&mut buf, false, 0);
            }
        }
        f.write_str(&buf)
    }
}

impl Ast<'_> {
    fn is_simple(&self) -> bool {
        self.size() < 4
    }

    fn size(&self) -> usize {
        match &self.0 {
            AstEnum::Tag(_) | AstEnum::Symbol(_, _) | AstEnum::Binding(_) => 1,
            AstEnum::Call(f, args) => {
                1 + f.size() + args.iter().map(|arg| arg.size()).sum::<usize>()
            }
            AstEnum::InfixCall(a, f, args) => {
                1 + a.size() + f.size() + args.iter().map(|arg| arg.size()).sum::<usize>()
            }
            AstEnum::Block((_, first, _), rest) => {
                1 + first.size() + rest.iter().map(|(_, arg, _)| arg.size()).sum::<usize>()
            }
            AstEnum::BuiltInRec(_, body) => 1 + body.size(),
            AstEnum::BuiltInFn(_)
            | AstEnum::BuiltInPop(_)
            | AstEnum::BuiltInIf(_)
            | AstEnum::BuiltInIfCoreTag(_, _) => 1,
        }
    }

    fn is_invocable(&self) -> bool {
        match &self.0 {
            AstEnum::Tag(_) | AstEnum::Binding(_) | AstEnum::Block(_, _) => false,
            _ => true,
        }
    }

    fn pretty(&self, buf: &mut String, _wrap: bool, lvl: usize) {
        fn indent(s: &mut String, lvl: usize) {
            for _ in 0..lvl {
                s.push_str("  ");
            }
        }
        match &self.0 {
            AstEnum::Tag(t) => {
                buf.push_str(t);
            }
            AstEnum::Symbol(var, _) => {
                buf.push_str(var);
            }
            AstEnum::Binding(b) => {
                buf.push('\'');
                buf.push_str(b);
            }
            AstEnum::Call(f, args) => {
                f.pretty(buf, true, lvl);
                let is_trailing_arg = args.len() == 1 && !args[0].is_invocable();
                if is_trailing_arg {
                    buf.push(' ');
                } else {
                    buf.push('(');
                }
                let is_simple = args.iter().all(|arg| arg.is_simple());
                if !is_simple {
                    buf.push('\n');
                    indent(buf, lvl + 1);
                }
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        if is_simple {
                            buf.push_str(", ");
                        } else {
                            buf.push_str(",\n");
                            indent(buf, lvl + 1);
                        }
                    }
                    if is_simple {
                        arg.pretty(buf, false, lvl);
                    } else {
                        arg.pretty(buf, false, lvl + 1);
                    }
                }
                if !is_simple {
                    buf.push('\n');
                    indent(buf, lvl);
                }
                if !is_trailing_arg {
                    buf.push(')');
                }
            }
            AstEnum::InfixCall(a, f, args) => {
                a.pretty(buf, _wrap, lvl);
                buf.push(' ');
                Ast(AstEnum::Call(f.clone(), args.clone()), self.1).pretty(buf, _wrap, lvl)
            }
            AstEnum::Block((_, first, _), rest) => {
                buf.push_str("{ ");
                first.pretty(buf, _wrap, lvl);
                for (_, expr, _) in rest {
                    buf.push_str(", ");
                    expr.pretty(buf, _wrap, lvl);
                }
                buf.push_str(" }");
            }
            AstEnum::BuiltInRec(var, body) => {
                buf.push_str("rec('");
                buf.push_str(var);
                buf.push_str(", ");
                body.pretty(buf, _wrap, lvl);
                buf.push(')');
            }
            AstEnum::BuiltInFn(s)
            | AstEnum::BuiltInPop(s)
            | AstEnum::BuiltInIf(s)
            | AstEnum::BuiltInIfCoreTag(s, _) => buf.push_str(s),
        }
    }
}

impl std::fmt::Display for Expr {
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
            ExprEnum::Pop(value, then_expr, else_expr) => {
                f.write_str("(pop ")?;
                value.fmt(f)?;
                f.write_str(" with ")?;
                then_expr.fmt(f)?;
                f.write_str(" else ")?;
                else_expr.fmt(f)?;
                f.write_str(")")
            }
            ExprEnum::If((a, b), then_expr, else_expr) => {
                f.write_str("(if ")?;
                a.fmt(f)?;
                f.write_str(" is ")?;
                b.fmt(f)?;
                f.write_str(" then ")?;
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

impl Ast<'_> {
    pub fn eval(self) -> Result<String, String> {
        let mut pool = Pool::new();
        let mut expr = self.to_expr(&mut pool);
        for (name, code) in PRELUDE.iter() {
            let tokens = scan(code);
            let mut tokens = tokens.iter().peekable();
            let primitive = parse_expr(&mut tokens, &mut vec![], &mut vec![])
                .unwrap_or_else(|e| panic!("Could not parse primitive {name}: {e}"))
                .to_expr(&mut pool);
            let pos = Pos { line: 0, col: 0 };
            expr = Expr::app(pool.new_id(pos), expr, primitive);
        }
        match expr.eval(&Arc::new(Env::Empty)) {
            Ok(v) => {
                let mut buf = String::new();
                v.to_string(&mut buf, &pool);
                Ok(buf)
            }
            Err(expr) => Err(format!("Could not eval {expr}")),
        }
    }
}

#[derive(Debug, Clone)]
enum Value {
    RecVal(Expr, Arc<Env<Value>>, Id),
    Val(Val),
}

#[derive(Debug, Clone)]
enum Val {
    Fn(Expr, Arc<Env<Value>>, Id),
    Tag(usize, Vec<Value>, Id),
}

impl Value {
    fn to_string(&self, buf: &mut String, pool: &Pool) {
        match self {
            Value::RecVal(_, _, id) => buf.push_str(&format!("<rec:{id}>")),
            Value::Val(Val::Fn(_, _, id)) => buf.push_str(&format!("<fn:{id}>")),
            Value::Val(Val::Tag(t, vals, _)) => {
                let mut t = *t;
                let mut skip = 0;
                if pool.core.iter().any(|(_, id)| *id == t) {
                    if let Some(Value::Val(Val::Tag(tag, inner, _))) = vals.first() {
                        if inner.is_empty() {
                            t = *tag;
                            skip = 1;
                        }
                    }
                }
                match pool.tags.iter().find(|(_, id)| **id == t) {
                    Some((tag, _)) => buf.push_str(tag),
                    None => match pool.bindings.iter().find(|(_, id)| **id == t) {
                        Some((tag, _)) => buf.push_str(&format!("'{tag}")),
                        None => match pool.core.iter().find(|(_, id)| **id == t) {
                            Some((tag, _)) => buf.push_str(&format!("\\{tag}")),
                            None => buf.push_str(&format!("<tag:{t}>")),
                        },
                    },
                }
                if vals.len() > skip {
                    buf.push('(');
                    for (i, val) in vals.iter().skip(skip).enumerate() {
                        if i != 0 {
                            buf.push_str(", ");
                        }
                        val.to_string(buf, pool);
                    }
                    buf.push(')');
                }
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
}

impl Expr {
    fn eval(self, env: &Arc<Env<Value>>) -> Result<Value, Expr> {
        fn resolve_rec(mut v: Value) -> Result<Val, Expr> {
            loop {
                match v {
                    Value::Val(val) => return Ok(val),
                    Value::RecVal(body, env, id) => {
                        let env = env.push(Value::RecVal(body.clone(), Arc::clone(&env), id));
                        v = body.eval(&env)?;
                    }
                }
            }
        }
        fn app(f: Value, arg: Value) -> Result<Value, Expr> {
            match resolve_rec(f)? {
                Val::Fn(body, fn_env, _) => body.eval(&fn_env.push(arg)),
                Val::Tag(t, mut values, id) => {
                    values.push(arg);
                    Ok(Value::Val(Val::Tag(t, values, id)))
                }
            }
        }
        match self.0 {
            ExprEnum::Var(var) => env.get(var).cloned().ok_or(self),
            ExprEnum::Tag(t) => Ok(Value::Val(Val::Tag(t, vec![], self.1))),
            ExprEnum::Abs(body) => Ok(Value::Val(Val::Fn(*body, Arc::clone(env), self.1))),
            ExprEnum::Rec(body) => Ok(Value::RecVal(*body, Arc::clone(env), self.1)),
            ExprEnum::App(f, arg) => match resolve_rec(f.eval(env)?)? {
                Val::Fn(body, fn_env, _) => body.eval(&fn_env.push(arg.eval(env)?)),
                Val::Tag(t, mut values, id) => {
                    values.push(arg.eval(env)?);
                    Ok(Value::Val(Val::Tag(t, values, id)))
                }
            },
            ExprEnum::Pop(v, then_expr, else_expr) => match resolve_rec(v.eval(env)?)? {
                Val::Tag(t, mut values, id) => {
                    if let Some(last) = values.pop() {
                        let butlast = Value::Val(Val::Tag(t, values, id));
                        app(app(then_expr.eval(env)?, butlast)?, last)
                    } else {
                        else_expr.eval(env)
                    }
                }
                Val::Fn(_, _, _) => else_expr.eval(env),
            },
            ExprEnum::If((a, b), then_expr, else_expr) => {
                match (resolve_rec(a.eval(env)?)?, resolve_rec(b.eval(env)?)?) {
                    (Val::Tag(a, rest_a, _), Val::Tag(b, rest_b, _))
                        if a == b && rest_a.is_empty() && rest_b.is_empty() =>
                    {
                        then_expr.eval(&env)
                    }
                    _ => else_expr.eval(&env),
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
        let code = "fn 'x { x } Foo";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "Foo");
    }

    #[test]
    fn eval_let() {
        let code = "=('x, Foo, { x })";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "Foo");
    }

    #[test]
    fn eval_block() {
        let code = "{ Foo } Nil";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert!(parsed.eval().unwrap().starts_with("Foo"));
    }

    #[test]
    fn eval_let_in_block() {
        let code = "'bar = Bar, Foo(bar)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "Foo(Bar)");
    }

    #[test]
    fn eval_multiple_let_in_block() {
        let code = "'x = X, 'y = Y, Pair(x, y)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "Pair(X, Y)");
    }

    #[test]
    fn eval_multiple_let_and_side_effect_in_block() {
        let code = "'x = X, Nil, 'y = Y, Pair(x, y)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "Pair(X, Y)");
    }

    #[test]
    fn eval_simple_if_else() {
        let code = "Foo == Foo { True } { False }";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "True");
    }

    // #[test]
    // fn eval_complex_if_else() {
    //     let code = "if(Pair(Foo, Bar), Pair('x, 'y), { x }, { Nil })";
    //     let parsed = parse(code).unwrap();
    //     assert_eq!(code, parsed.to_string());
    //     assert_eq!(parsed.eval().unwrap(), "Foo");
    // }

    #[test]
    fn eval_rec() {
        let code = "rec('r, { Cons(Foo, r) })";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert!(parsed.eval().unwrap().starts_with("<rec:"));
    }

    #[test]
    fn eval_std_lib() {
        let code = include_str!("../examples/std.vo");
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.eval().unwrap(), "True");
    }
}
