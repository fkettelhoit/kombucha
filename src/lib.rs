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
pub struct Ast<'code>(AstEnum<'code>, Pos);

#[derive(Debug, Clone)]
enum AstEnum<'code> {
    Tag(&'code str),
    Symbol(&'code str, usize),
    Binding(&'code str),
    Call(Box<Ast<'code>>, Vec<Ast<'code>>),
    Block(
        (usize, Box<Ast<'code>>, usize),
        Vec<(usize, Ast<'code>, usize)>,
    ),
    BuiltInLet(&'code str, Box<Ast<'code>>, Box<Ast<'code>>),
    BuiltInRec(&'code str, Box<Ast<'code>>),
    BuiltInIf {
        val: Box<Ast<'code>>,
        tag: &'code str,
        vars: Vec<&'code str>,
        then_expr: Box<Ast<'code>>,
        else_expr: Box<Ast<'code>>,
    },
    BuiltInLet2(&'code str, Box<Ast<'code>>),
}

type Tokens<'a, 'code> = Peekable<Iter<'a, (Token<'code>, Pos)>>;

pub fn parse(code: &str) -> Result<Ast<'_>, String> {
    let tokens = scan(code);
    let mut tokens = tokens.iter().peekable();
    let mut stack = vec![];
    let pos = Pos { line: 1, col: 1 };
    let block = parse_block(pos, &mut tokens, &mut stack, &mut vec![])?;
    if let Some((token, pos)) = tokens.next() {
        return Err(format!(
            "Expected the code to end, but found {token} at {pos}"
        ));
    }
    return Ok(block);
    fn parse_block<'code>(
        pos: Pos,
        tokens: &mut Tokens<'_, 'code>,
        stack: &mut Vec<&'code str>,
        bindings: &mut Vec<&'code str>,
    ) -> Result<Ast<'code>, String> {
        let mut args = bindings.len();
        if args == 0 {
            args += 1;
            stack.push("");
        } else {
            stack.append(bindings);
        }
        let expr = parse_expr(tokens, stack, bindings)?;
        let first = (args, Box::new(expr), bindings.len());
        let mut rest = vec![];
        loop {
            match tokens.peek() {
                Some((Token::Separator, _)) => {
                    tokens.next();
                    let mut args = bindings.len();
                    if args == 0 {
                        args += 1;
                        stack.push("");
                    } else {
                        stack.append(bindings);
                    }
                    let expr = parse_expr(tokens, stack, bindings)?;
                    rest.push((args, expr, bindings.len()));
                }
                Some((Token::BraceClose, _)) | None => {
                    bindings.clear();
                    return Ok(Ast(AstEnum::Block(first, rest), pos));
                }
                Some((t, pos)) => {
                    return Err(format!(
                        "Expected block to continue with ',' or '}}', but found {t} at {pos}"
                    ));
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
        fn expect<'code>(
            tokens: &mut Tokens<'_, 'code>,
            msg: &str,
            exp: Token<'code>,
        ) -> Result<Pos, String> {
            match tokens.next() {
                Some((t, pos)) if t == &exp => Ok(*pos),
                Some((t, pos)) => Err(format!("{msg}, expected '{exp}', found '{t}' at {pos}")),
                None => Err(format!("{msg}, but the code just ended")),
            }
        }
        let pos = *pos;
        let expr = match t {
            Token::Tag(t) => Ast(AstEnum::Tag(t), pos),
            Token::Binding(b) => {
                bindings.push(b);
                Ast(AstEnum::Binding(b), pos)
            }
            Token::BraceOpen => {
                let block = parse_block(pos, tokens, stack, bindings)?;
                match tokens.next() {
                    Some(_) => block,
                    None => {
                        return Err(format!(
                            "Expected the block starting at {pos} to be closed with '}}', but the code just ended"
                        ));
                    }
                }
            }
            Token::Symbol(s) => {
                if let Some((i, _)) = stack.iter().rev().enumerate().find(|(_, var)| s == *var) {
                    Ast(AstEnum::Symbol(s, i), pos)
                } else if *s == "let" {
                    // let('var, val, { body })
                    let msg = "The built-in 'let' must be called as let('var, val, { body })";
                    expect(tokens, msg, Token::ParenOpen)?;
                    let var = match tokens.next() {
                        Some((Token::Binding(b), _)) => b,
                        Some((t, pos)) => {
                            return Err(format!("{msg}, expected a binding, found '{t}' at {pos}"));
                        }
                        None => return Err(format!("{msg}, but the code just ended")),
                    };
                    bindings.push(var);
                    expect(tokens, msg, Token::Separator)?;
                    let val = parse_expr(tokens, stack, bindings)?;
                    if let Some((Token::ParenClose, _)) = tokens.peek() {
                        tokens.next();
                        Ast(AstEnum::BuiltInLet2(var, Box::new(val)), pos)
                    } else {
                        expect(tokens, msg, Token::Separator)?;
                        let block_pos = expect(tokens, msg, Token::BraceOpen)?;
                        let body = parse_block(block_pos, tokens, stack, bindings)?;
                        expect(tokens, msg, Token::BraceClose)?;
                        expect(tokens, msg, Token::ParenClose)?;
                        Ast(AstEnum::BuiltInLet(var, Box::new(val), Box::new(body)), pos)
                    }
                } else if *s == "rec" {
                    // rec('var, { body })
                    let msg = "The built-in 'rec' must be called as rec('var, { body })";
                    expect(tokens, msg, Token::ParenOpen)?;
                    let var = match tokens.next() {
                        Some((Token::Binding(b), _)) => b,
                        Some((t, pos)) => {
                            return Err(format!("{msg}, expected a binding, found '{t}' at {pos}"));
                        }
                        None => return Err(format!("{msg}, but the code just ended")),
                    };
                    bindings.push(var);
                    expect(tokens, msg, Token::Separator)?;
                    let block_pos = expect(tokens, msg, Token::BraceOpen)?;
                    let body = parse_block(block_pos, tokens, stack, bindings)?;
                    expect(tokens, msg, Token::BraceClose)?;
                    expect(tokens, msg, Token::ParenClose)?;
                    Ast(AstEnum::BuiltInRec(var, Box::new(body)), pos)
                } else if *s == "if" {
                    // if(val, Tag('x, 'y, ...), { then }, { else })
                    let msg = "The built-in 'if' must be called as if(val, Tag('x, 'y, ...), { then }, { else })";
                    expect(tokens, msg, Token::ParenOpen)?;
                    let val = parse_expr(tokens, stack, bindings)?;
                    expect(tokens, msg, Token::Separator)?;
                    let tag = match tokens.next() {
                        Some((Token::Tag(t), _)) => t,
                        Some((t, pos)) => {
                            return Err(format!("{msg}, expected a tag, found '{t}' at {pos}"));
                        }
                        None => return Err(format!("{msg}, but the code just ended")),
                    };
                    let mut vars = vec![];
                    match tokens.next() {
                        Some((Token::Separator, _)) => {}
                        Some((Token::ParenOpen, _)) => loop {
                            let var = match tokens.next() {
                                Some((Token::Binding(b), _)) => b,
                                Some((t, pos)) => {
                                    return Err(format!(
                                        "{msg}, expected a binding, found '{t}' at {pos}"
                                    ));
                                }
                                None => return Err(format!("{msg}, but the code just ended")),
                            };
                            if vars.contains(var) {
                                return Err(format!(
                                    "All variables in a pattern must be unique, but '{var} is used again at {pos}"
                                ));
                            }
                            vars.push(*var);
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
                    let block_pos = expect(tokens, msg, Token::BraceOpen)?;
                    bindings.extend(vars.iter());
                    let then_expr = parse_block(block_pos, tokens, stack, bindings)?;
                    expect(tokens, msg, Token::BraceClose)?;
                    expect(tokens, msg, Token::Separator)?;
                    let block_pos = expect(tokens, msg, Token::BraceOpen)?;
                    let else_expr = parse_block(block_pos, tokens, stack, bindings)?;
                    expect(tokens, msg, Token::BraceClose)?;
                    expect(tokens, msg, Token::ParenClose)?;
                    Ast(
                        AstEnum::BuiltInIf {
                            val: Box::new(val),
                            tag,
                            vars,
                            then_expr: Box::new(then_expr),
                            else_expr: Box::new(else_expr),
                        },
                        pos,
                    )
                } else {
                    return Err(format!("Could not find binding for '{s}' at {pos}"));
                }
            }
            t => return Err(format!("Unexpected token '{t}' at {pos}")),
        };
        match tokens.peek() {
            Some((Token::ParenOpen, _)) => {
                tokens.next();
                let mut args = vec![];
                loop {
                    args.push(parse_expr(tokens, stack, bindings)?);
                    match tokens.peek() {
                        Some((Token::ParenClose, _)) => {
                            tokens.next();
                            return Ok(Ast(AstEnum::Call(Box::new(expr), args), pos));
                        }
                        Some((Token::Separator, _)) => {
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
            _ => return Ok(expr),
        }
    }
}

impl<'code> Ast<'code> {
    fn to_expr(&self, pool: &mut Pool<'code>) -> _Expr {
        let pos = self.1;
        match &self.0 {
            AstEnum::Tag(t) => _Expr(ExprEnum::Tag(pool.intern_tag(t)), pool.register(pos)),
            AstEnum::Symbol(_, i) => _Expr(ExprEnum::Var(*i), pool.register(pos)),
            AstEnum::Binding(b) => _Expr(ExprEnum::Tag(pool.intern_binding(b)), pool.register(pos)),
            AstEnum::Call(f, args) => {
                let mut expr = f.to_expr(pool);
                for arg in args {
                    expr = _Expr(
                        ExprEnum::App(Box::new(expr), Box::new(arg.to_expr(pool))),
                        pool.register(pos),
                    );
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
                            expr = _Expr(
                                ExprEnum::App(Box::new(block), Box::new(expr)),
                                pool.register(pos),
                            )
                        } else {
                            // { let('x, Foo), ... }
                            // <==> let('x, Foo)(x => ...)
                            expr = _Expr(
                                ExprEnum::App(Box::new(expr), Box::new(block)),
                                pool.register(pos),
                            )
                        }
                    }
                    for _ in 0..args {
                        expr = _Expr(ExprEnum::Abs(Box::new(expr)), pool.register(pos));
                    }
                    next = Some(expr)
                }
                next.unwrap()
            }
            AstEnum::BuiltInLet(_, val, body) => _Expr(
                ExprEnum::App(Box::new(body.to_expr(pool)), Box::new(val.to_expr(pool))),
                pool.register(pos),
            ),
            AstEnum::BuiltInLet2(_, val) => _Expr(
                ExprEnum::Abs(Box::new(_Expr(
                    ExprEnum::App(
                        Box::new(_Expr(ExprEnum::Var(0), pool.register(pos))),
                        Box::new(val.to_expr(pool)),
                    ),
                    pool.register(pos),
                ))),
                pool.register(pos),
            ),
            AstEnum::BuiltInRec(_, body) => _Expr(
                ExprEnum::Rec(Box::new(body.to_expr(pool))),
                pool.register(pos),
            ),
            AstEnum::BuiltInIf {
                val,
                tag,
                vars,
                then_expr,
                else_expr,
            } => {
                let tag_id = pool.intern_tag(tag);
                let val = val.to_expr(pool);
                let mut then_expr = then_expr.to_expr(pool);
                for i in (0..vars.len()).rev() {
                    then_expr = _Expr(
                        ExprEnum::App(
                            Box::new(then_expr),
                            Box::new(_Expr(ExprEnum::Var(i), pool.register(pos))),
                        ),
                        pool.register(pos),
                    );
                }
                if vars.is_empty() {
                    then_expr = _Expr(
                        ExprEnum::App(
                            Box::new(then_expr),
                            Box::new(_Expr(ExprEnum::Tag(Pool::nil()), pool.register(pos))),
                        ),
                        pool.register(pos),
                    );
                }
                let else_expr = _Expr(
                    ExprEnum::App(
                        Box::new(else_expr.to_expr(pool)),
                        Box::new(_Expr(ExprEnum::Tag(Pool::nil()), pool.register(pos))),
                    ),
                    pool.register(pos),
                );
                _Expr(
                    ExprEnum::Cnd {
                        value: Box::new(val),
                        pattern: (tag_id, vars.len()),
                        then_expr: Box::new(then_expr),
                        else_expr: Box::new(else_expr),
                    },
                    pool.register(pos),
                )
            }
        }
    }
}

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

#[derive(Debug, Clone)]
struct Pool<'code> {
    tags: HashMap<&'code str, usize>,
    bindings: HashMap<&'code str, usize>,
    nodes: HashMap<Id, Pos>,
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

    fn register(&mut self, pos: Pos) -> Id {
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
            AstEnum::Block((_, first, _), rest) => {
                1 + first.size() + rest.iter().map(|(_, arg, _)| arg.size()).sum::<usize>()
            }
            AstEnum::BuiltInLet(_, val, body) => 2 + val.size() + body.size(),
            AstEnum::BuiltInLet2(_, val) => 2 + val.size(),
            AstEnum::BuiltInRec(_, body) => 1 + body.size(),
            AstEnum::BuiltInIf {
                val,
                tag: _,
                vars,
                then_expr,
                else_expr,
            } => 1 + val.size() + 1 + vars.len() + then_expr.size() + else_expr.size(),
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
            AstEnum::Block((_, first, _), rest) => {
                buf.push_str("{ ");
                first.pretty(buf, _wrap, lvl);
                for (_, expr, _) in rest {
                    buf.push_str(", ");
                    expr.pretty(buf, _wrap, lvl);
                }
                buf.push_str(" }");
            }
            AstEnum::BuiltInLet(var, val, body) => {
                buf.push_str("let('");
                buf.push_str(var);
                buf.push_str(", ");
                val.pretty(buf, _wrap, lvl);
                buf.push_str(", ");
                body.pretty(buf, _wrap, lvl);
                buf.push(')');
            }
            AstEnum::BuiltInLet2(var, val) => {
                buf.push_str("let('");
                buf.push_str(var);
                buf.push_str(", ");
                val.pretty(buf, _wrap, lvl);
                buf.push(')');
            }
            AstEnum::BuiltInRec(var, body) => {
                buf.push_str("rec('");
                buf.push_str(var);
                buf.push_str(", ");
                body.pretty(buf, _wrap, lvl);
                buf.push(')');
            }
            AstEnum::BuiltInIf {
                val,
                tag,
                vars,
                then_expr,
                else_expr,
            } => {
                buf.push_str("if(");
                val.pretty(buf, false, lvl);
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
                then_expr.pretty(buf, false, lvl);
                buf.push_str(", ");
                else_expr.pretty(buf, false, lvl);
                buf.push(')');
            }
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

impl Ast<'_> {
    pub fn eval(self) -> Result<String, String> {
        let mut pool = Pool::new();
        let mut expr = self.to_expr(&mut pool);
        if let _Expr(ExprEnum::Abs(block), _) = expr {
            expr = *block;
        } else {
            unreachable!("Expected the AST to be parsed as a block.");
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

impl _Value {
    fn to_string(&self, buf: &mut String, pool: &Pool) {
        match self {
            _Value::Fn(_, _, id) => buf.push_str(&format!("<fn:{id}>")),
            _Value::Rec(_, _, id) => buf.push_str(&format!("<rec:{id}>")),
            _Value::Tag(t, vals, _) => {
                match pool.tags.iter().find(|(_, id)| *id == t) {
                    Some((tag, _)) => buf.push_str(tag),
                    None => buf.push_str(&format!("<tag:{t}>")),
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
        assert_eq!(parsed.eval().unwrap(), "Foo");
    }

    #[test]
    fn eval_block() {
        let code = "{ Foo }";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
        assert!(parsed.eval().unwrap().starts_with("<fn:"));
    }

    #[test]
    fn eval_let_in_block() {
        let code = "let('bar, Bar), Foo(bar)";
        let parsed = parse(code).unwrap();
        assert_eq!(code, parsed.to_string());
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
        assert!(parsed.eval().unwrap().starts_with("<rec:"));
    }
}
