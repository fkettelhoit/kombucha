use std::{
    cmp::max,
    collections::HashMap,
    iter::{Peekable, once},
    slice::Iter,
    sync::Arc,
};

//
// SCAN
//

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tok<'code> {
    Separator(Sep),
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    BraceOpen,
    BraceClose,
    Tag(&'code str),
    Symbol(&'code str),
    Binding(&'code str),
    Reserved(&'code str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Sep {
    Comma,
    Newline,
}

impl std::fmt::Display for Tok<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tok::Separator(Sep::Comma) => f.write_str(","),
            Tok::Separator(Sep::Newline) => f.write_str("\\n"),
            Tok::ParenOpen => f.write_str("("),
            Tok::ParenClose => f.write_str(")"),
            Tok::BracketOpen => f.write_str("["),
            Tok::BracketClose => f.write_str("]"),
            Tok::BraceOpen => f.write_str("{"),
            Tok::BraceClose => f.write_str("}"),
            Tok::Tag(s) | Tok::Symbol(s) => write!(f, "{s}"),
            Tok::Binding(s) => write!(f, "'{s}"),
            Tok::Reserved(s) => write!(f, "\\{s}"),
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

fn scan(code: &str) -> Vec<(Tok, Pos)> {
    let mut tokens = vec![];
    let mut col = 1;
    let mut line = 1;
    let mut start = Pos { col, line };
    let mut i = 0;
    for (j, char) in code.chars().chain(once(' ')).enumerate() {
        match char {
            ' ' | '\n' | '(' | ')' | '[' | ']' | '{' | '}' | ',' => {
                if i < j {
                    let curr = &code[i..j];
                    if let Some(first) = curr.chars().next() {
                        if first.is_uppercase() {
                            tokens.push((Tok::Tag(curr), start));
                        } else if first == '\'' {
                            tokens.push((Tok::Binding(&curr[1..]), start));
                        } else if first == '\\' {
                            tokens.push((Tok::Reserved(&curr[1..]), start));
                        } else {
                            tokens.push((Tok::Symbol(curr), start));
                        }
                    }
                }
                i = j + 1;
                match char {
                    '\n' => {
                        tokens.push((Tok::Separator(Sep::Newline), Pos { col, line }));
                        col = 0;
                        line += 1;
                    }
                    ',' => tokens.push((Tok::Separator(Sep::Comma), Pos { col, line })),
                    '(' => tokens.push((Tok::ParenOpen, Pos { col, line })),
                    ')' => tokens.push((Tok::ParenClose, Pos { col, line })),
                    '[' => tokens.push((Tok::BracketOpen, Pos { col, line })),
                    ']' => tokens.push((Tok::BracketClose, Pos { col, line })),
                    '{' => tokens.push((Tok::BraceOpen, Pos { col, line })),
                    '}' => tokens.push((Tok::BraceClose, Pos { col, line })),
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
// PARSE
//

#[derive(Debug, Clone)]
pub struct Ast<'code>(AstEnum<'code>, Pos);

#[derive(Debug, Clone)]
enum AstEnum<'code> {
    Tag(&'code str),
    Symbol(&'code str, usize),
    Binding(&'code str),
    Call(Option<Box<Ast<'code>>>, Box<Ast<'code>>, Vec<Ast<'code>>),
    TrailingCall(Option<Box<Ast<'code>>>, Box<Ast<'code>>, Box<Ast<'code>>),
    Block(Vec<(usize, Ast<'code>)>),
    List(Vec<Ast<'code>>),
    BuiltIn(&'code str, BuiltIn),
}

#[derive(Debug, Clone, Copy)]
enum BuiltIn {
    Abs,
    Rec,
    Pop,
    If,
    Ty,
    Nil,
}

type Tokens<'a, 'code> = Peekable<Iter<'a, (Tok<'code>, Pos)>>;

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
    Ok(block)
}

fn parse_block<'code>(
    pos: Pos,
    tokens: &mut Tokens<'_, 'code>,
    stack: &mut Vec<&'code str>,
    bindings: &mut Vec<&'code str>,
) -> Result<Ast<'code>, String> {
    let stack_size_before = stack.len();
    let mut elems = vec![];
    let mut expect_separator = false;
    loop {
        match tokens.peek() {
            Some((Tok::BraceClose, _)) | None => {
                bindings.clear();
                for _ in 0..(stack.len() - stack_size_before) {
                    stack.pop();
                }
                if elems.is_empty() {
                    return Err(format!(
                        "Blocks cannot be empty, but found an empty block at {pos}"
                    ));
                }
                return Ok(Ast(AstEnum::Block(elems), pos));
            }
            Some((Tok::Separator(_), _)) => {
                tokens.next();
                expect_separator = false;
            }
            Some((t, pos)) => {
                if expect_separator {
                    return Err(format!(
                        "Expected ',' after the last expression in the block, but found '{t}' at {pos}"
                    ));
                }
                let args = bindings.len();
                if bindings.is_empty() {
                    stack.push("");
                } else {
                    stack.append(bindings);
                }
                let expr = parse_expr(tokens, stack, bindings)?;
                elems.push((args, expr));
                expect_separator = true;
            }
        }
    }
}

fn parse_expr<'code>(
    tokens: &mut Tokens<'_, 'code>,
    stack: &mut Vec<&'code str>,
    bindings: &mut Vec<&'code str>,
) -> Result<Ast<'code>, String> {
    let mut expr = parse_prefix_expr(tokens, stack, bindings)?;
    while let Some((Tok::Symbol(s), sym_pos)) = tokens.peek() {
        tokens.next();
        let f = parse_symbol(*sym_pos, s, stack)?;
        if let Some((Tok::Symbol(_), _)) = tokens.peek() {
            let next = parse_prefix_expr(tokens, stack, bindings)?;
            expr = Ast(
                AstEnum::TrailingCall(Some(Box::new(expr)), Box::new(f), Box::new(next)),
                *sym_pos,
            )
        } else {
            expr = parse_call(Some(Box::new(expr)), f, tokens, stack, bindings)?;
        }
    }
    return Ok(expr);

    fn parse_prefix_expr<'code>(
        tokens: &mut Tokens<'_, 'code>,
        stack: &mut Vec<&'code str>,
        bindings: &mut Vec<&'code str>,
    ) -> Result<Ast<'code>, String> {
        match tokens.peek() {
            Some((Tok::Symbol(s), pos)) => {
                tokens.next();
                let f = parse_symbol(*pos, s, stack)?;
                parse_call(None, f, tokens, stack, bindings)
            }
            Some((Tok::ParenOpen, pos)) => {
                tokens.next();
                let f = match tokens.peek() {
                    Some((Tok::ParenClose, _)) => {
                        tokens.next();
                        Ast(AstEnum::BuiltIn("()", BuiltIn::Nil), *pos)
                    }
                    _ => {
                        let expr = parse_expr(tokens, stack, bindings)?;
                        match tokens.next() {
                            Some((Tok::ParenClose, _)) => expr,
                            Some((t, p)) => {
                                return Err(format!(
                                    "Unmatched '(' at {pos}: Expected ')', found '{t}' at {p}"
                                ));
                            }
                            None => {
                                return Err(format!(
                                    "Unmatched '(' at {pos}: Expected ')', but the code just ended"
                                ));
                            }
                        }
                    }
                };
                parse_call(None, f, tokens, stack, bindings)
            }
            _ => parse_value_with_args(tokens, stack, bindings),
        }
    }

    fn parse_call<'code>(
        mut infix_arg: Option<Box<Ast<'code>>>,
        mut f: Ast<'code>,
        tokens: &mut Tokens<'_, 'code>,
        stack: &mut Vec<&'code str>,
        bindings: &mut Vec<&'code str>,
    ) -> Result<Ast<'code>, String> {
        let pos = f.1;
        let mut arg_bindings = vec![];
        while let Some((Tok::ParenOpen, arg_pos)) = tokens.peek() {
            tokens.next();
            let args = parse_exprs(*arg_pos, Tok::ParenClose, tokens, stack, &mut arg_bindings)?;
            f = Ast(AstEnum::Call(infix_arg.take(), Box::new(f), args), pos);
        }
        loop {
            match tokens.peek() {
                Some((
                    Tok::Symbol(_)
                    | Tok::Separator(_)
                    | Tok::ParenClose
                    | Tok::BracketClose
                    | Tok::BraceClose,
                    _,
                ))
                | None => {
                    bindings.append(&mut arg_bindings);
                    return Ok(f);
                }
                _ => {
                    let arg = parse_value_with_args(tokens, stack, &mut arg_bindings)?;
                    f = Ast(
                        AstEnum::TrailingCall(infix_arg.take(), Box::new(f), Box::new(arg)),
                        pos,
                    )
                }
            }
        }
    }

    fn parse_value_with_args<'code>(
        tokens: &mut Tokens<'_, 'code>,
        stack: &mut Vec<&'code str>,
        bindings: &mut Vec<&'code str>,
    ) -> Result<Ast<'code>, String> {
        let mut value = parse_value(tokens, stack, bindings)?;
        while let Some((Tok::ParenOpen, pos)) = tokens.peek() {
            tokens.next();
            let args = parse_exprs(*pos, Tok::ParenClose, tokens, stack, bindings)?;
            value = Ast(AstEnum::Call(None, Box::new(value), args), *pos);
        }
        Ok(value)
    }

    fn parse_value<'code>(
        tokens: &mut Tokens<'_, 'code>,
        stack: &mut Vec<&'code str>,
        bindings: &mut Vec<&'code str>,
    ) -> Result<Ast<'code>, String> {
        let Some((t, pos)) = tokens.next() else {
            return Err("Expected a value, but the code just ended".to_string());
        };
        let pos = *pos;
        match t {
            Tok::Tag(t) => Ok(Ast(AstEnum::Tag(t), pos)),
            Tok::Binding(b) => {
                bindings.push(b);
                Ok(Ast(AstEnum::Binding(b), pos))
            }
            Tok::BracketOpen => {
                let list = parse_exprs(pos, Tok::BracketClose, tokens, stack, bindings)?;
                Ok(Ast(AstEnum::List(list), pos))
            }
            Tok::BraceOpen => {
                let block = parse_block(pos, tokens, stack, bindings)?;
                match tokens.next() {
                    Some(_) => Ok(block),
                    None => Err(format!(
                        "Expected the block starting at {pos} to be closed with '}}', but the code just ended"
                    )),
                }
            }
            t => Err(format!("Expected a value, found '{t}' at {pos}")),
        }
    }

    fn parse_exprs<'code>(
        pos: Pos,
        expected: Tok,
        tokens: &mut Tokens<'_, 'code>,
        stack: &mut Vec<&'code str>,
        bindings: &mut Vec<&'code str>,
    ) -> Result<Vec<Ast<'code>>, String> {
        let mut exprs = vec![];
        let mut expect_separator = false;
        loop {
            match tokens.peek() {
                Some((t, _)) if *t == expected => {
                    tokens.next();
                    break Ok(exprs);
                }
                Some((Tok::Separator(_), _)) => {
                    tokens.next();
                    expect_separator = false;
                }
                Some((t, pos)) => {
                    if expect_separator {
                        return Err(format!(
                            "Expected ',' after the last expression, but found '{t}' at {pos}"
                        ));
                    }
                    exprs.push(parse_expr(tokens, stack, bindings)?);
                    expect_separator = true;
                }
                None => {
                    return Err(format!(
                        "Expected the expressions starting at {pos} to be closed with '{expected}', but the code just ended"
                    ));
                }
            }
        }
    }

    fn parse_symbol<'code>(
        pos: Pos,
        s: &'code str,
        stack: &[&'code str],
    ) -> Result<Ast<'code>, String> {
        if let Some((i, _)) = stack.iter().rev().enumerate().find(|(_, var)| &s == *var) {
            Ok(Ast(AstEnum::Symbol(s, i), pos))
        } else if s == "fn" {
            Ok(Ast(AstEnum::BuiltIn(s, BuiltIn::Abs), pos))
        } else if s == "rec" {
            Ok(Ast(AstEnum::BuiltIn(s, BuiltIn::Rec), pos))
        } else if s == "pop" {
            Ok(Ast(AstEnum::BuiltIn(s, BuiltIn::Pop), pos))
        } else if s == "if" {
            Ok(Ast(AstEnum::BuiltIn(s, BuiltIn::If), pos))
        } else if s == "type" {
            Ok(Ast(AstEnum::BuiltIn(s, BuiltIn::Ty), pos))
        } else {
            return Err(format!("Could not find binding for '{s}' at {pos}"));
        }
    }
}

//
// DESUGAR
//

#[derive(Debug, Clone)]
struct Expr(ExprEnum, Id);

#[derive(Debug, Clone)]
enum ExprEnum {
    Var(usize),
    Tag(usize),
    Wrap(Box<Expr>),
    Abs(Box<Expr>),
    Rec(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Pop(Box<Expr>, Box<Expr>, Box<Expr>),
    If((Box<Expr>, Box<Expr>), Box<Expr>, Box<Expr>),
    Ty(Box<Expr>),
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

    fn builtin_ty(id: Id, v: Expr) -> Self {
        Self(ExprEnum::Ty(Box::new(v)), id)
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
    tags: HashMap<&'code str, usize>,
    nodes: HashMap<Id, Pos>,
}

// Make sure to change the `core_tags` variable in `next_interned` whenever a variant is added!
enum CoreTag {
    Nil,
    List,
    Fn,
    Tag,
    Value,
    Block,
    Binding,
    Compound,
}

impl<'code> Pool<'code> {
    fn new() -> Self {
        Self {
            tags: HashMap::from_iter(vec![
                // Nil and List are not directly represented as tags and so don't appear here
                ("Fn", CoreTag::Fn as usize),
                ("Tag", CoreTag::Tag as usize),
                ("Value", CoreTag::Value as usize),
                ("Block", CoreTag::Block as usize),
                ("Binding", CoreTag::Binding as usize),
                ("Compound", CoreTag::Compound as usize),
            ]),
            nodes: HashMap::new(),
        }
    }

    fn next_interned(&self) -> usize {
        let core_tags = 8; // the number of variants of the `CoreTag` enum
        core_tags + self.tags.len()
    }

    fn intern_tag(&mut self, s: &'code str) -> usize {
        self.tags.get(s).copied().unwrap_or_else(|| {
            let id = self.next_interned();
            self.tags.insert(s, id);
            id
        })
    }

    fn new_id(&mut self, pos: Pos) -> Id {
        let id = Id(self.nodes.len() as u32 + 1);
        self.nodes.insert(id, pos);
        id
    }
}

impl<'code> Ast<'code> {
    fn to_wrapped_expr(&self, pool: &mut Pool<'code>) -> Expr {
        #[derive(PartialEq, Eq)]
        enum Arg {
            Value,
            Block,
            Pending,
        }
        fn reflect<'code>(expr: &Ast<'code>) -> Arg {
            match &expr.0 {
                AstEnum::Tag(_) | AstEnum::Symbol(_, _) | AstEnum::BuiltIn(_, _) => Arg::Value,
                AstEnum::Binding(_) => Arg::Pending,
                AstEnum::Block(_) => Arg::Block,
                AstEnum::Call(arg, f, args) => {
                    let mut arg = arg.as_ref().map(|arg| reflect(arg)).unwrap_or(Arg::Value);
                    if arg == Arg::Block {
                        return Arg::Value;
                    };
                    for a in args {
                        match reflect(a) {
                            Arg::Value => {}
                            Arg::Block => return Arg::Block,
                            Arg::Pending => arg = Arg::Pending,
                        }
                    }
                    match reflect(f) {
                        Arg::Value => arg,
                        Arg::Block => arg,
                        Arg::Pending => Arg::Pending,
                    }
                }
                AstEnum::TrailingCall(a, f, b) => {
                    let mut arg = a.as_ref().map(|arg| reflect(arg)).unwrap_or(Arg::Value);
                    if arg == Arg::Block {
                        return Arg::Value;
                    };
                    match reflect(b) {
                        Arg::Value => {}
                        Arg::Block => return Arg::Block,
                        Arg::Pending => arg = Arg::Pending,
                    }
                    match reflect(f) {
                        Arg::Value => arg,
                        Arg::Block => arg,
                        Arg::Pending => Arg::Pending,
                    }
                }
                AstEnum::List(elems) => {
                    for elem in elems {
                        if let Arg::Pending = reflect(elem) {
                            return Arg::Pending;
                        }
                    }
                    return Arg::Value;
                }
            }
        }
        let pos = self.1;
        match &self.0 {
            AstEnum::Binding(_) => Expr::app(
                pool.new_id(pos),
                Expr::tag(pool.new_id(pos), CoreTag::Binding as usize),
                self.to_expr(pool),
            ),
            AstEnum::Block(_) => Expr::app(
                pool.new_id(pos),
                Expr::tag(pool.new_id(pos), CoreTag::Block as usize),
                self.to_expr(pool),
            ),
            AstEnum::Call(infix_arg, f, args) if reflect(self) != Arg::Value => {
                let mut has_block_arg = false;
                let mut wrapped_args = vec![];
                if let Some(infix_arg) = infix_arg {
                    if reflect(infix_arg) == Arg::Block {
                        has_block_arg = true;
                    }
                    wrapped_args.push(infix_arg.to_wrapped_expr(pool));
                }
                for arg in args {
                    if reflect(arg) == Arg::Block {
                        has_block_arg = true;
                    }
                    wrapped_args.push(arg.to_wrapped_expr(pool));
                }
                if wrapped_args.is_empty() {
                    wrapped_args.push(Expr::app(
                        pool.new_id(pos),
                        Expr::tag(pool.new_id(pos), CoreTag::Value as usize),
                        Expr::tag(pool.new_id(pos), CoreTag::Nil as usize),
                    ));
                }
                let mut expr = if has_block_arg {
                    f.to_expr(pool)
                } else {
                    f.to_wrapped_expr(pool)
                };
                for arg in wrapped_args {
                    expr = Expr::app(pool.new_id(pos), expr, arg);
                }
                if !has_block_arg {
                    expr = Expr::app(
                        pool.new_id(pos),
                        Expr::tag(pool.new_id(pos), CoreTag::Compound as usize),
                        expr,
                    )
                }
                expr
            }
            AstEnum::TrailingCall(infix_arg, f, arg) if reflect(self) != Arg::Value => {
                let mut has_block_arg = false;
                let mut wrapped_args = vec![];
                if let Some(infix_arg) = infix_arg {
                    if reflect(infix_arg) == Arg::Block {
                        has_block_arg = true;
                    }
                    wrapped_args.push(infix_arg.to_wrapped_expr(pool));
                }
                if reflect(arg) == Arg::Block {
                    has_block_arg = true;
                }
                wrapped_args.push(arg.to_wrapped_expr(pool));
                let mut expr = if has_block_arg {
                    f.to_expr(pool)
                } else {
                    f.to_wrapped_expr(pool)
                };
                for arg in wrapped_args {
                    expr = Expr::app(pool.new_id(pos), expr, arg);
                }
                if !has_block_arg {
                    expr = Expr::app(
                        pool.new_id(pos),
                        Expr::tag(pool.new_id(pos), CoreTag::Compound as usize),
                        expr,
                    )
                }
                expr
            }
            AstEnum::List(elems) => {
                let mut expr = Expr::tag(pool.new_id(pos), CoreTag::List as usize);
                for elem in elems {
                    expr = Expr::app(pool.new_id(pos), expr, elem.to_wrapped_expr(pool));
                }
                Expr::app(
                    pool.new_id(pos),
                    Expr::tag(pool.new_id(pos), CoreTag::Compound as usize),
                    expr,
                )
            }
            _ => Expr::app(
                pool.new_id(pos),
                Expr::tag(pool.new_id(pos), CoreTag::Value as usize),
                self.to_expr(pool),
            ),
        }
    }
    // are to_wrapped_expr and to_expr really different fns? aren't all values wrapped?
    fn to_expr(&self, pool: &mut Pool<'code>) -> Expr {
        let pos = self.1;
        match &self.0 {
            AstEnum::Tag(t) | AstEnum::Binding(t) => {
                Expr::tag(pool.new_id(pos), pool.intern_tag(t))
            }
            AstEnum::Symbol(_, i) => Expr::var(pool.new_id(pos), *i),
            AstEnum::Call(infix_arg, f, args) => {
                let mut expr = f.to_expr(pool);
                if args.is_empty() {
                    if let Some(infix_arg) = infix_arg {
                        expr = Expr::app(pool.new_id(pos), expr, infix_arg.to_expr(pool));
                    }
                    let nil = Expr::tag(pool.new_id(pos), CoreTag::Nil as usize);
                    Expr::app(pool.new_id(pos), expr, nil)
                } else {
                    let args = infix_arg
                        .as_deref()
                        .iter()
                        .copied()
                        .chain(args.iter())
                        .collect::<Vec<_>>();
                    for arg in args {
                        expr = Expr::app(pool.new_id(pos), expr, arg.to_expr(pool));
                    }
                    expr
                }
            }
            AstEnum::TrailingCall(infix_arg, f, arg) => {
                let f = match infix_arg {
                    Some(arg) => Expr::app(pool.new_id(pos), f.to_expr(pool), arg.to_expr(pool)),
                    None => f.to_expr(pool),
                };
                Expr::app(pool.new_id(pos), f, arg.to_expr(pool))
            }
            AstEnum::Block(elems) => {
                // `{ a('x), b('y), c }`
                // desugars to `a('x, { b('y), c })`
                // desugars to `a('x, { b('y, { c }) })`
                let mut next: Option<(Expr, usize)> = None;
                for (args, expr) in elems.iter().rev() {
                    let mut expr = expr.to_expr(pool);
                    if let Some((block, args_next)) = next {
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
                    for _ in 0..max(1, *args) {
                        expr = Expr::abs(pool.new_id(pos), expr);
                    }
                    next = Some((expr, *args))
                }
                next.unwrap().0
            }
            AstEnum::List(elems) => {
                let mut expr = Expr::tag(pool.new_id(pos), CoreTag::List as usize);
                for elem in elems {
                    expr = Expr::app(pool.new_id(pos), expr, elem.to_expr(pool));
                }
                expr
            }
            AstEnum::BuiltIn(_, BuiltIn::Abs) => Expr::abs(
                pool.new_id(pos),
                Expr::abs(pool.new_id(pos), Expr::var(pool.new_id(pos), 0)),
            ),
            AstEnum::BuiltIn(_, BuiltIn::Rec) => Expr::abs(
                pool.new_id(pos),
                Expr::rec(pool.new_id(pos), Expr::var(pool.new_id(pos), 0)),
            ),
            AstEnum::BuiltIn(_, BuiltIn::Pop) => Expr::abs(
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
                                Expr::tag(pool.new_id(pos), CoreTag::Nil as usize),
                            ),
                        ),
                    ),
                ),
            ),
            AstEnum::BuiltIn(_, BuiltIn::If) => Expr::abs(
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
                                    Expr::tag(pool.new_id(pos), CoreTag::Nil as usize),
                                ),
                                Expr::app(
                                    pool.new_id(pos),
                                    Expr::var(pool.new_id(pos), 0),
                                    Expr::tag(pool.new_id(pos), CoreTag::Nil as usize),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
            AstEnum::BuiltIn(_, BuiltIn::Ty) => Expr::abs(
                pool.new_id(pos),
                Expr::builtin_ty(pool.new_id(pos), Expr::var(pool.new_id(pos), 0)),
            ),
            AstEnum::BuiltIn(_, BuiltIn::Nil) => Expr::tag(pool.new_id(pos), CoreTag::Nil as usize),
        }
    }
}

//
// PRINT
//

impl std::fmt::Display for Ast<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = String::new();
        match &self.0 {
            AstEnum::Block(elems) => {
                for (i, (_, expr)) in elems.iter().enumerate() {
                    if i != 0 {
                        buf.push_str(", ");
                    }
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
        self.size() < 10
    }

    fn size(&self) -> usize {
        match &self.0 {
            AstEnum::Tag(_)
            | AstEnum::Symbol(_, _)
            | AstEnum::Binding(_)
            | AstEnum::BuiltIn(_, _) => 1,
            AstEnum::Call(infix_arg, f, args) => {
                f.size()
                    + infix_arg.iter().map(|arg| arg.size()).sum::<usize>()
                    + args.iter().map(|arg| arg.size()).sum::<usize>()
            }
            AstEnum::TrailingCall(infix_arg, f, arg) => {
                f.size() + infix_arg.iter().map(|arg| arg.size()).sum::<usize>() + arg.size()
            }
            AstEnum::Block(elems) => 1 + elems.iter().map(|(_, arg)| arg.size()).sum::<usize>(),
            AstEnum::List(elems) => 1 + elems.iter().map(|elem| elem.size()).sum::<usize>(),
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
            AstEnum::Call(infix_arg, f, args) => {
                if let Some(arg) = infix_arg {
                    arg.pretty(buf, _wrap, lvl);
                    buf.push(' ');
                }
                f.pretty(buf, true, lvl);
                buf.push('(');
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
                buf.push(')');
            }
            AstEnum::TrailingCall(infix_arg, f, arg) => {
                if let Some(arg) = infix_arg {
                    arg.pretty(buf, _wrap, lvl);
                    buf.push(' ');
                }
                f.pretty(buf, _wrap, lvl);
                buf.push(' ');
                arg.pretty(buf, _wrap, lvl);
            }
            AstEnum::Block(elems) => {
                buf.push_str("{ ");
                for (i, (_, expr)) in elems.iter().enumerate() {
                    if i != 0 {
                        buf.push_str(", ");
                    }
                    expr.pretty(buf, _wrap, lvl);
                }
                buf.push_str(" }");
            }
            AstEnum::List(elems) => {
                buf.push('[');
                for (i, elem) in elems.iter().enumerate() {
                    if i != 0 {
                        buf.push_str(", ");
                    }
                    elem.pretty(buf, _wrap, lvl);
                }
                buf.push(']');
            }
            AstEnum::BuiltIn(s, _) => buf.push_str(s),
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            ExprEnum::Var(s) => write!(f, "{s}"),
            ExprEnum::Tag(t) => write!(f, "\"{t}\""),
            ExprEnum::Abs(expr) => {
                f.write_str("(=> ")?;
                expr.fmt(f)?;
                f.write_str(")")
            }
            ExprEnum::Wrap(value) => {
                f.write_str("(wrap ")?;
                value.fmt(f)?;
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
            ExprEnum::Ty(value) => {
                f.write_str("(type ")?;
                value.fmt(f)?;
                f.write_str(")")
            }
        }
    }
}

//
// EVAL
//

const PRELUDE: &[(&str, &str)] = &[
    ("=", "fn ['binding, 'val, 'block] { block(val) }"),
    (
        "if-eq",
        "fn(['x, 'y, 'then, 'else]) {
            pop(x, fn(['xx, 'x], {
                pop(y, fn(['yy, 'y], {
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
    Val(Val),
    Wrap(Box<Value>),
    RecVal(Expr, Arc<Env<Value>>, Id),
}

#[derive(Debug, Clone)]
enum Val {
    Fn(Expr, Arc<Env<Value>>, Id),
    Tag(usize, Vec<Value>, Id),
}

impl Value {
    fn to_string(&self, buf: &mut String, pool: &Pool) {
        match self {
            Value::Wrap(v) => v.to_string(buf, pool),
            Value::RecVal(body, _, _) => buf.push_str(&format!("(~> {})", body.to_string())),
            Value::Val(Val::Fn(body, _, _)) => buf.push_str(&format!("(=> {})", body.to_string())),
            Value::Val(Val::Tag(t, vals, _)) => {
                if *t == CoreTag::List as usize {
                    buf.push('[');
                    for (i, val) in vals.iter().enumerate() {
                        if i != 0 {
                            buf.push_str(", ");
                        }
                        val.to_string(buf, pool);
                    }
                    return buf.push(']');
                } else if *t == CoreTag::Nil as usize {
                    buf.push_str("()");
                } else if let Some((tag, _)) = pool.tags.iter().find(|(_, tag)| *tag == t) {
                    buf.push_str(tag);
                } else {
                    buf.push_str(&format!("\\{t}"))
                }
                if !vals.is_empty() {
                    buf.push('(');
                    for (i, val) in vals.iter().enumerate() {
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
                    Value::Wrap(value) => v = *value,
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
            ExprEnum::Wrap(v) => Ok(Value::Wrap(Box::new(v.eval(env)?))),
            ExprEnum::Abs(body) => Ok(Value::Val(Val::Fn(*body, Arc::clone(env), self.1))),
            ExprEnum::Rec(body) => match body.eval(env)? {
                Value::Val(Val::Fn(body, fn_env, id)) => Ok(Value::RecVal(body, fn_env, id)),
                v => Ok(v),
            },
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
                        then_expr.eval(env)
                    }
                    _ => else_expr.eval(env),
                }
            }
            ExprEnum::Ty(v) => {
                let id = v.1;
                let mut v = v.eval(env)?;
                loop {
                    match v {
                        Value::Wrap(_) => {
                            return Ok(Value::Val(Val::Tag(CoreTag::Value as usize, vec![], id)));
                        }
                        Value::Val(Val::Fn(_, _, _)) => {
                            return Ok(Value::Val(Val::Tag(CoreTag::Fn as usize, vec![], id)));
                        }
                        Value::Val(Val::Tag(_, _, _)) => {
                            return Ok(Value::Val(Val::Tag(CoreTag::Tag as usize, vec![], id)));
                        }
                        Value::RecVal(body, env, id) => {
                            let env = env.push(Value::RecVal(body.clone(), Arc::clone(&env), id));
                            v = body.eval(&env)?;
                        }
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
        let code = "{ Foo }(Nil)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "Foo");
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
        let code = "if-eq(Foo, Foo) { True } { False }";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "True");
    }

    #[test]
    fn eval_rec() {
        let code =
            "'r = rec(fn('r, { Cons(Foo, r) })), pop(r, fn(Pair('xs, 'x), { xs }), { Error })";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "Cons(Foo)");
    }

    #[test]
    fn eval_std_lib() {
        let code = "'x = Foo, if-eq(x, Foo) { True } { False }";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "True");
    }

    #[test]
    fn eval_list() {
        let code = "[Foo(Bar), [Baz, Qux]]";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "[Foo(Bar), [Baz, Qux]]");
    }

    #[test]
    fn eval_nil() {
        let code = "()(Bar, Baz())";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert_eq!(parsed.eval().unwrap(), "()(Bar, Baz(()))");
    }

    #[test]
    fn eval_match() {
        let code = include_str!("../examples/eq.vo");
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.eval().unwrap(), "[True, False, True, False, True]");
    }
}
