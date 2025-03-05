use std::{
    collections::{HashMap, HashSet},
    iter::{Peekable, once},
    slice::Iter,
    sync::Arc,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Comma,
    ParenOpenApp,
    ParenOpenGroup,
    ParenClose,
    Symbol(String),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Comma => f.write_str(","),
            Token::ParenOpenApp | Token::ParenOpenGroup => f.write_str("("),
            Token::ParenClose => f.write_str(")"),
            Token::Symbol(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos {
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
    let mut could_open_app = false;
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
                    '(' if could_open_app => tokens.push((Token::ParenOpenApp, Pos { col, line })),
                    '(' => tokens.push((Token::ParenOpenGroup, Pos { col, line })),
                    ')' => tokens.push((Token::ParenClose, Pos { col, line })),

                    _ => {}
                }
                col += 1;
                start = Pos { col, line };
                could_open_app = char == ')';
            }
            c => {
                col += 1;
                curr.push(c);
                could_open_app = true;
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

#[derive(Clone)]
pub struct InternedExpr(Expr, InternPool);

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
    Eff(usize),
    Abs(Box<Expr>),
    Rec(Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Cnd {
        value: Box<Expr>,
        pattern: (usize, usize),
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Try {
        expr: Box<Expr>,
        handlers: Vec<(usize, usize, Expr)>,
    },
}

#[derive(Debug, Clone)]
pub struct InternPool {
    tags: HashMap<String, usize>,
    effects: (HashMap<String, usize>, HashMap<usize, String>),
}

impl InternPool {
    fn new() -> Self {
        Self {
            tags: HashMap::from_iter(vec![("".to_string(), 0)]),
            effects: (HashMap::new(), HashMap::new()),
        }
    }

    fn resolve_var(&mut self, vars: &[String], s: &str) -> ExprEnum {
        for (i, t) in vars.iter().rev().enumerate() {
            if &s == t {
                return ExprEnum::Var(i);
            }
        }
        ExprEnum::Eff(self.resolve_eff(s))
    }

    fn resolve_eff(&mut self, e: &str) -> usize {
        match self.effects.0.get(e) {
            Some(e) => *e,
            None => {
                let n = self.effects.0.len();
                self.effects.0.insert(e.to_string(), n);
                self.effects.1.insert(n, e.to_string());
                n
            }
        }
    }

    fn resolve_tag(&mut self, t: &str) -> usize {
        match self.tags.get(t) {
            Some(t) => *t,
            None => {
                let n = self.tags.len();
                self.tags.insert(t.to_string(), n);
                n
            }
        }
    }
}

pub fn parse(code: &str) -> Result<InternedExpr, String> {
    fn parse_expr(
        tokens: &mut Peekable<Iter<(Token, Pos)>>,
        vars: &mut Vec<String>,
        pool: &mut InternPool,
    ) -> Result<Expr, String> {
        let Some((token, pos)) = tokens.next() else {
            return Err("Expected an expression, but found nothing".into());
        };
        let mut expr = match token {
            Token::Symbol(s) if is_tag(s) => {
                Expr::new(ExprEnum::Tag(pool.resolve_tag(s)), Some(s.clone()), *pos)
            }
            Token::Symbol(s) if s.as_str() == "try" => {
                let expr = parse_expr(tokens, vars, pool)?;
                let mut aliases = vec![];
                let mut handlers = vec![];
                loop {
                    match tokens.peek() {
                        Some((Token::Symbol(s), _)) if s.as_str() == "catch" => {
                            tokens.next();
                            let (h, name) = match tokens.next() {
                                Some((Token::Symbol(h), _)) if !is_tag(h) => {
                                    (pool.resolve_eff(h), h)
                                }
                                Some((t, pos)) => {
                                    return Err(format!(
                                        "Expected the handler to start with a name, but found {t} at {pos}"
                                    ));
                                }
                                None => return Err("Expected a handler, but found nothing".into()),
                            };
                            expect(tokens, Token::ParenOpenApp)?;
                            let mut bindings = 0;
                            let mut bound_vars = HashSet::new();
                            let mut alias = format!("{name}(");
                            if let Some((Token::ParenClose, _)) = tokens.peek() {
                                // `catch handle()` is treated as `catch handle(_)`
                                tokens.next();
                                alias += ")";
                                vars.push("".to_string());
                                bindings = 1;
                            } else {
                                loop {
                                    let (binding, pos) = expect_var(tokens)?;
                                    if bound_vars.contains(&binding) {
                                        return Err(format!(
                                            "All variables in a handler must be unique, but '{binding} is used twice at {pos}"
                                        ));
                                    }
                                    alias += &binding;
                                    bound_vars.insert(binding.clone());
                                    vars.push(binding.clone());
                                    bindings += 1;
                                    match tokens.next() {
                                        Some((Token::ParenClose, _)) => {
                                            alias += ")";
                                            break;
                                        }
                                        Some((Token::Comma, _)) => alias += ", ",
                                        Some((t, pos)) => {
                                            return Err(format!(
                                                "Expected ',' or ')', but found '{t}' at {pos}"
                                            ));
                                        }
                                        None => {
                                            return Err("Expected a ')', but found nothing".into());
                                        }
                                    }
                                }
                            }
                            pool.resolve_eff(name);
                            expect(tokens, Token::Symbol("as".into()))?;
                            let (resumable, _) = expect_var(tokens)?;
                            alias += &format!(" as {resumable}");
                            vars.push(resumable);
                            let handler = parse_expr(tokens, vars, pool)?;
                            for _ in 0..bindings + 1 {
                                vars.pop();
                            }
                            aliases.push(alias);
                            handlers.push((h, bindings, handler))
                        }
                        _ => {
                            break;
                        }
                    }
                }
                let try_expr = ExprEnum::Try {
                    expr: Box::new(expr),
                    handlers,
                };
                Expr::new(try_expr, Some(aliases.join("\n")), *pos)
            }
            Token::Symbol(s) if s.as_str() == "if" => {
                let v = parse_expr(tokens, vars, pool)?;
                expect(tokens, Token::Symbol("is".into()))?;
                let (tag, mut alias) = match tokens.next() {
                    Some((Token::Symbol(t), _)) if is_tag(t) => (pool.resolve_tag(t), t.clone()),
                    Some((t, pos)) => {
                        return Err(format!(
                            "Expected the pattern to start with a tag, but found {t} at {pos}"
                        ));
                    }
                    None => return Err("Expected a pattern, but found nothing".into()),
                };
                let mut bindings = 0;
                if let Some((Token::ParenOpenApp, _)) = tokens.peek() {
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
                            Some((Token::ParenClose, _)) => {
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
                let then_expr = parse_expr(tokens, vars, pool)?;
                expect(tokens, Token::Symbol("else".into()))?;
                for _ in 0..bindings {
                    vars.pop();
                }
                let else_expr = parse_expr(tokens, vars, pool)?;
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
                let binding = parse_expr(tokens, vars, pool)?;
                vars.push(name.clone());
                let body = parse_expr(tokens, vars, pool)?;
                vars.pop();
                let abs = Expr::new(ExprEnum::Abs(Box::new(body)), Some(name.clone()), *pos);
                Expr::new(ExprEnum::App(Box::new(abs), Box::new(binding)), None, *pos)
            }
            Token::Symbol(s) if s.as_str() == "loop" => {
                let (name, _) = expect_var(tokens)?;
                expect(tokens, Token::Symbol("=".into()))?;
                vars.push(name.clone());
                let binding = parse_expr(tokens, vars, pool)?;
                let body = parse_expr(tokens, vars, pool)?;
                vars.pop();
                let abs = Expr::new(ExprEnum::Abs(Box::new(body)), Some(name.clone()), *pos);
                let rec = Expr::new(ExprEnum::Rec(Box::new(binding)), Some(name.clone()), *pos);
                Expr::new(ExprEnum::App(Box::new(abs), Box::new(rec)), None, *pos)
            }
            Token::Symbol(s) => match tokens.peek() {
                Some((Token::Symbol(t), pos)) if t.as_str() == "=>" => {
                    tokens.next();
                    vars.push(s.clone());
                    let body = parse_expr(tokens, vars, pool)?;
                    vars.pop();
                    Expr::new(ExprEnum::Abs(Box::new(body)), Some(s.clone()), *pos)
                }
                Some((Token::Symbol(t), _)) if t.as_str() == "~>" => {
                    tokens.next();
                    vars.push(s.clone());
                    let body = parse_expr(tokens, vars, pool)?;
                    vars.pop();
                    Expr::new(ExprEnum::Rec(Box::new(body)), Some(s.clone()), *pos)
                }
                _ => Expr::new(pool.resolve_var(vars, s), Some(s.clone()), *pos),
            },
            Token::ParenOpenGroup => {
                let expr = parse_expr(tokens, vars, pool)?;
                expect(tokens, Token::ParenClose)?;
                expr
            }
            t => return Err(format!("Unexpected token '{t}' at {pos}",)),
        };
        let expr = match tokens.peek() {
            Some((Token::ParenOpenApp, _)) => {
                tokens.next();
                if let Some((Token::ParenClose, _)) = tokens.peek() {
                    // `f()` is treated as `f(())`, where `()` is the tag with an empty name
                    tokens.next();
                    let arg = Expr::new(ExprEnum::Tag(0), Some("()".to_string()), *pos);
                    return Ok(Expr::new(
                        ExprEnum::App(Box::new(expr), Box::new(arg)),
                        None,
                        *pos,
                    ));
                }
                loop {
                    let arg = parse_expr(tokens, vars, pool)?;
                    expr = Expr::new(ExprEnum::App(Box::new(expr), Box::new(arg)), None, *pos);
                    if let Some((Token::ParenClose, _)) = tokens.peek() {
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
    fn is_tag(s: &str) -> bool {
        s.chars().next().unwrap_or_default().is_uppercase()
    }
    let tokens = scan(code);
    let mut tokens = tokens.iter().peekable();
    let mut pool = InternPool::new();
    let expr = parse_expr(&mut tokens, &mut vec![], &mut pool)?;
    if let Some((token, pos)) = tokens.next() {
        Err(format!("Unexpected token {token} at {pos}"))
    } else {
        Ok(InternedExpr(expr, pool))
    }
}

impl std::fmt::Display for InternedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
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
            ExprEnum::Var(_) | ExprEnum::Tag(_) | ExprEnum::Eff(_) => true,
            ExprEnum::Cnd { .. } | ExprEnum::Try { .. } => false,
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
            ExprEnum::Var(s) | ExprEnum::Tag(s) | ExprEnum::Eff(s) => match &self.1.alias {
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
                if args.len() == 1
                    && args
                        .iter()
                        .all(|arg| matches!(arg.as_ref(), Expr(ExprEnum::Tag(0), _)))
                {
                    return f.write_str("()");
                }
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
                if group {
                    f.write_str("(")?;
                }
                f.write_str("if ")?;
                value.pretty(f, false, lvl)?;
                f.write_str(" is ")?;
                if let Some(alias) = &self.1.alias {
                    f.write_str(alias)?;
                } else {
                    let (tag, bindings) = pattern;
                    write!(f, "'{tag} {bindings}")?;
                }
                f.write_str("\n")?;
                indent(f, lvl + 1)?;
                then_expr.pretty(f, false, lvl + 1)?;
                f.write_str("\n")?;
                indent(f, lvl)?;
                if let Expr(ExprEnum::Cnd { .. }, _) = else_expr.as_ref() {
                    f.write_str("else ")?;
                    else_expr.pretty(f, false, lvl)?;
                } else {
                    f.write_str("else\n")?;
                    indent(f, lvl + 1)?;
                    else_expr.pretty(f, false, lvl + 1)?;
                }
                if group {
                    f.write_str("\n")?;
                    indent(f, lvl)?;
                    f.write_str(")")?;
                }
                Ok(())
            }
            ExprEnum::Try { expr, handlers } => {
                if group {
                    f.write_str("(")?;
                }
                f.write_str("try\n")?;
                indent(f, lvl + 1)?;
                expr.pretty(f, false, lvl + 1)?;
                let aliases = match &self.1.alias {
                    Some(alias) => alias.split("\n").collect::<Vec<_>>(),
                    None => vec![""; handlers.len()],
                };
                for ((tag, bindings, handler), alias) in handlers.iter().zip(aliases) {
                    f.write_str("\n")?;
                    indent(f, lvl)?;
                    f.write_str("catch ")?;
                    if alias.is_empty() {
                        write!(f, "'{tag} {bindings}")?;
                    } else {
                        f.write_str(alias)?;
                    }
                    f.write_str("\n")?;
                    indent(f, lvl + 1)?;
                    handler.pretty(f, false, lvl + 1)?;
                }
                if group {
                    f.write_str("\n")?;
                    indent(f, lvl)?;
                    f.write_str(")")?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Val {
    Fn(Expr, Arc<Env<Val>>, Meta),
    Rec(Expr, Arc<Env<Val>>, Meta),
    Tag(usize, Vec<Val>, Meta),
    Eff(usize, Vec<Val>, Meta),
    Res(Cont, Arc<Env<Val>>),
}

impl TryFrom<Val> for Expr {
    type Error = String;

    fn try_from(value: Val) -> Result<Self, Self::Error> {
        match value {
            Val::Fn(body, _, meta) => Ok(Expr(ExprEnum::Abs(Box::new(body)), meta)),
            Val::Rec(body, _, meta) => Ok(Expr(ExprEnum::Rec(Box::new(body)), meta)),
            Val::Tag(t, vals, meta) => {
                let mut expr = Expr(ExprEnum::Tag(t), meta.clone());
                for v in vals {
                    let app = ExprEnum::App(Box::new(expr), Box::new(Expr::try_from(v)?));
                    expr = Expr(app, meta.clone());
                }
                Ok(expr)
            }
            Val::Eff(e, vals, meta) => {
                let mut expr = Expr(ExprEnum::Eff(e), meta.clone());
                for v in vals {
                    let app = ExprEnum::App(Box::new(expr), Box::new(Expr::try_from(v)?));
                    expr = Expr(app, meta.clone());
                }
                Ok(expr)
            }
            Val::Res(_, _) => Err("Cannot convert resumable value to expr".to_string()),
        }
    }
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match Expr::try_from(self.clone()) {
            Ok(expr) => expr.fmt(f),
            Err(_) => f.write_str("<RESUMABLE>"),
        }
    }
}

#[derive(Debug)]
pub enum Env<T> {
    Empty(Arc<InternPool>),
    Some(T, Arc<Env<T>>),
    Tag(String, usize, Arc<Env<T>>),
}

impl<T> Env<T> {
    fn get(&self, var: usize) -> Option<&T> {
        match self {
            Env::Empty { .. } => None,
            Env::Some(expr, _) if var == 0 => Some(expr),
            Env::Some(_, env) => env.get(var - 1),
            Env::Tag(_, _, env) => env.get(var),
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

    fn root(self: &Arc<Self>) -> &Arc<InternPool> {
        match self.as_ref() {
            Env::Empty(pool) => pool,
            Env::Some(_, env) | Env::Tag(_, _, env) => env.root(),
        }
    }

    fn tag(self: &Arc<Self>, tag: &str) -> Option<usize> {
        match self.as_ref() {
            Env::Empty(pool) => pool.tags.get(tag).copied(),
            Env::Tag(t, id, _) if t == tag => Some(*id),
            Env::Tag(_, _, env) => env.tag(tag),
            Env::Some(_, env) => env.tag(tag),
        }
    }

    fn intern(self: &Arc<Self>, tag: String) -> (usize, Arc<Self>) {
        let t = self.root().tags.len();
        (t, Arc::new(Env::Tag(tag, t, Arc::clone(self))))
    }
}

#[derive(Clone)]
enum V {
    Fn(Expr, Arc<Env<V>>, Meta),
    Tag(usize, Vec<V>, Meta),
    Any,
}

#[derive(Debug, Clone)]
pub enum EvalResult {
    Val(Val),
    Pending(Resumable, Handler),
}

#[derive(Debug, Clone)]
pub struct Resumable(Cont, Arc<Env<Val>>);

#[derive(Debug, Clone)]
pub struct Handler {
    name: usize,
    args: Vec<Val>,
    m: Meta,
}

#[derive(Debug, Clone)]
pub enum Cont {
    Effect(Pos),
    App(Box<Cont>, Box<Expr>),
    AppFn(Expr, Arc<Env<Val>>, Meta, Box<Cont>),
    AppTag(usize, Vec<Val>, Meta, Box<Cont>),
    AppEff(usize, Vec<Val>, Meta, Box<Cont>),
    AppRes(Box<Cont>, Arc<Env<Val>>, Box<Cont>),
    Cnd {
        value: Box<Cont>,
        pattern: (usize, usize),
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Try {
        expr: Box<Cont>,
        handlers: Vec<(usize, usize, Expr)>,
    },
}

impl InternedExpr {
    pub fn eval(self) -> Result<Val, String> {
        match self.eval_with_handlers(&Arc::new(HashMap::new()))? {
            EvalResult::Val(val) => Ok(val),
            EvalResult::Pending(_, _) => Err("Evaluation requires a handler".to_string()),
        }
    }

    pub fn eval_with_handlers(
        self,
        handlers: &Arc<HashMap<String, usize>>,
    ) -> Result<EvalResult, String> {
        let InternedExpr(expr, mut pool) = self;
        let mut resolved_handlers = vec![];
        for (name, arity) in handlers.iter() {
            resolved_handlers.push((pool.resolve_eff(name), *arity));
        }
        let pool = Arc::new(pool);
        expr.check(&Arc::new(Env::Empty(Arc::clone(&pool))), &[])?;
        let result = expr._eval(&Arc::new(Env::Empty(Arc::clone(&pool))));
        eval_with_handlers(result, resolved_handlers)
    }
}

fn eval_with_handlers(
    mut result: EvalResult,
    handlers: Vec<(usize, usize)>,
) -> Result<EvalResult, String> {
    loop {
        match result {
            EvalResult::Val(val) => return Ok(EvalResult::Val(val)),
            EvalResult::Pending(Resumable(cont, env), Handler { name, args, m }) => {
                if let Some((h, arity)) = handlers.iter().find(|(h, _)| *h == name) {
                    if *arity > args.len() {
                        result = cont.resume(Val::Eff(*h, args, m), &env);
                    } else {
                        return Ok(EvalResult::Pending(
                            Resumable(cont, env),
                            Handler { name, args, m },
                        ));
                    }
                } else {
                    return Err(format!("No handler for {name}"));
                }
            }
        }
    }
}

impl Expr {
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
            ExprEnum::Eff(_) => Ok(vec![V::Any]),
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
            ExprEnum::Try { expr, handlers } => {
                let mut results = expr.check(env, stack)?;
                for (_, vars, handler) in handlers {
                    results.extend(handler.check(&env.extend(vec![V::Any; vars + 1]), stack)?);
                }
                Ok(results)
            }
        }
    }

    fn _eval(self, env: &Arc<Env<Val>>) -> EvalResult {
        let pos = self.1.pos;
        match self.0 {
            ExprEnum::Var(var) => EvalResult::Val(
                env.get(var)
                    .cloned()
                    .unwrap_or_else(|| panic!("No binding for {self} at {pos}")),
            ),
            ExprEnum::Tag(t) => EvalResult::Val(Val::Tag(t, vec![], self.1.clone())),
            ExprEnum::Eff(e) => EvalResult::Val(Val::Eff(e, vec![], self.1.clone())),
            ExprEnum::Abs(body) => EvalResult::Val(Val::Fn(*body, Arc::clone(env), self.1.clone())),
            ExprEnum::Rec(body) => {
                EvalResult::Val(Val::Rec(*body, Arc::clone(env), self.1.clone()))
            }
            ExprEnum::App(f, arg) => eval_app(f._eval(env), arg, env),
            ExprEnum::Cnd {
                value,
                pattern,
                then_expr,
                else_expr,
            } => eval_cnd(value._eval(env), pattern, then_expr, else_expr, env),
            ExprEnum::Try { expr, handlers } => eval_try(expr._eval(env), handlers, env),
        }
    }
}

impl Resumable {
    pub fn resume(
        self,
        value: Val,
        handlers: &Arc<HashMap<String, usize>>,
    ) -> Result<EvalResult, String> {
        let Resumable(cont, env) = self;
        let result = cont.resume(value, &env);
        let pool = env.root();
        let mut resolved_handlers = vec![];
        for (name, arity) in handlers.iter() {
            match pool.effects.0.get(name) {
                Some(h) => resolved_handlers.push((*h, *arity)),
                None => return Err(format!("Unresolved handler: '{name}'")),
            }
        }
        eval_with_handlers(result, resolved_handlers)
    }

    pub fn intern(&mut self, tag: String) -> Val {
        let t = match self.1.tag(&tag) {
            Some(t) => t,
            None => {
                let (t, env) = self.1.intern(tag.clone());
                self.1 = env;
                t
            }
        };
        let meta = Meta {
            alias: Some(tag),
            pos: self.0.pos(),
        };
        Val::Tag(t, vec![], meta)
    }

    pub fn unit(&self) -> Val {
        let meta = Meta {
            alias: Some("()".to_string()),
            pos: self.0.pos(),
        };
        Val::Tag(0, vec![], meta)
    }
}

impl Cont {
    fn pos(&self) -> Pos {
        match self {
            Cont::Effect(pos) => *pos,
            Cont::App(cont, _)
            | Cont::AppFn(_, _, _, cont)
            | Cont::AppTag(_, _, _, cont)
            | Cont::AppEff(_, _, _, cont)
            | Cont::AppRes(_, _, cont)
            | Cont::Cnd { value: cont, .. }
            | Cont::Try { expr: cont, .. } => cont.pos(),
        }
    }

    fn resume(self, value: Val, env: &Arc<Env<Val>>) -> EvalResult {
        match self {
            Cont::Effect(_) => EvalResult::Val(value),
            Cont::App(cont, expr) => match cont.resume(value, env) {
                EvalResult::Val(f) => eval_app(EvalResult::Val(f), expr, env),
                EvalResult::Pending(Resumable(cont, _), args) => EvalResult::Pending(
                    Resumable(Cont::App(Box::new(cont), expr), Arc::clone(&env)),
                    args,
                ),
            },
            Cont::AppFn(body, fn_env, m, cont) => {
                eval_app_fn(body, fn_env, m, cont.resume(value, env))
            }
            Cont::AppTag(t, args, m, cont) => eval_app_tag(t, args, m, cont.resume(value, env)),
            Cont::AppEff(e, args, m, cont) => {
                eval_app_eff(e, args, m, cont.resume(value, env), env)
            }
            Cont::AppRes(res, res_env, cont_arg) => {
                eval_res(res, res_env, cont_arg.resume(value, env))
            }
            Cont::Cnd {
                value: cont,
                pattern,
                then_expr,
                else_expr,
            } => match cont.resume(value, env) {
                EvalResult::Val(v) => {
                    eval_cnd(EvalResult::Val(v), pattern, then_expr, else_expr, env)
                }
                EvalResult::Pending(Resumable(cont, _), args) => EvalResult::Pending(
                    Resumable(
                        Cont::Cnd {
                            value: Box::new(cont),
                            pattern,
                            then_expr,
                            else_expr,
                        },
                        Arc::clone(&env),
                    ),
                    args,
                ),
            },
            Cont::Try {
                expr: cont,
                handlers,
            } => match cont.resume(value, env) {
                EvalResult::Val(v) => eval_try(EvalResult::Val(v), handlers, env),
                EvalResult::Pending(Resumable(cont, _), args) => EvalResult::Pending(
                    Resumable(
                        Cont::Try {
                            expr: Box::new(cont),
                            handlers,
                        },
                        Arc::clone(&env),
                    ),
                    args,
                ),
            },
        }
    }
}

fn eval_app(mut f: EvalResult, arg: Box<Expr>, env: &Arc<Env<Val>>) -> EvalResult {
    loop {
        match f {
            EvalResult::Val(Val::Rec(body, env, m)) => {
                let env = env.push(Val::Rec(body.clone(), Arc::clone(&env), m));
                f = body._eval(&env);
            }
            EvalResult::Val(Val::Fn(body, fn_env, m)) => {
                break eval_app_fn(body, fn_env, m, arg._eval(env));
            }
            EvalResult::Val(Val::Tag(t, args, m)) => {
                break eval_app_tag(t, args, m, arg._eval(env));
            }
            EvalResult::Val(Val::Eff(e, args, m)) => {
                break eval_app_eff(e, args, m, arg._eval(env), env);
            }
            EvalResult::Val(Val::Res(cont, cont_env)) => {
                break eval_res(Box::new(cont), cont_env, arg._eval(env));
            }
            EvalResult::Pending(Resumable(cont, _), args) => {
                break EvalResult::Pending(
                    Resumable(Cont::App(Box::new(cont), arg), Arc::clone(&env)),
                    args,
                );
            }
        }
    }
}

fn eval_app_fn(body: Expr, fn_env: Arc<Env<Val>>, m: Meta, arg: EvalResult) -> EvalResult {
    match arg {
        EvalResult::Val(arg) => body._eval(&fn_env.push(arg)),
        EvalResult::Pending(Resumable(cont, env), args) => EvalResult::Pending(
            Resumable(Cont::AppFn(body, fn_env, m, Box::new(cont)), env),
            args,
        ),
    }
}

fn eval_app_tag(t: usize, mut args: Vec<Val>, m: Meta, arg: EvalResult) -> EvalResult {
    match arg {
        EvalResult::Val(arg) => {
            args.push(arg);
            EvalResult::Val(Val::Tag(t, args, m))
        }
        EvalResult::Pending(Resumable(cont, env), eff_args) => EvalResult::Pending(
            Resumable(Cont::AppTag(t, args, m, Box::new(cont)), env),
            eff_args,
        ),
    }
}

fn eval_app_eff(
    e: usize,
    mut args: Vec<Val>,
    m: Meta,
    arg: EvalResult,
    env: &Arc<Env<Val>>,
) -> EvalResult {
    match arg {
        EvalResult::Val(arg) => {
            args.push(arg);
            EvalResult::Pending(
                Resumable(Cont::Effect(m.pos), Arc::clone(env)),
                Handler { name: e, args, m },
            )
        }
        EvalResult::Pending(Resumable(cont, env), eff_args) => EvalResult::Pending(
            Resumable(Cont::AppEff(e, args, m, Box::new(cont)), env),
            eff_args,
        ),
    }
}

fn eval_res(cont: Box<Cont>, cont_env: Arc<Env<Val>>, arg: EvalResult) -> EvalResult {
    match arg {
        EvalResult::Val(arg) => cont.resume(arg, &cont_env),
        EvalResult::Pending(Resumable(cont_arg, env), args) => EvalResult::Pending(
            Resumable(Cont::AppRes(cont, cont_env, Box::new(cont_arg)), env),
            args,
        ),
    }
}

fn eval_cnd(
    mut value: EvalResult,
    (tag, vars): (usize, usize),
    then_expr: Box<Expr>,
    else_expr: Box<Expr>,
    env: &Arc<Env<Val>>,
) -> EvalResult {
    loop {
        match value {
            EvalResult::Val(Val::Rec(body, env, m)) => {
                let env = env.push(Val::Rec(body.clone(), Arc::clone(&env), m));
                value = body._eval(&env);
            }
            EvalResult::Val(Val::Tag(t, vals, _)) if t == tag && vals.len() == vars => {
                break then_expr._eval(&env.extend(vals));
            }
            EvalResult::Val(
                Val::Tag(_, _, _) | Val::Fn(_, _, _) | Val::Eff { .. } | Val::Res(_, _),
            ) => {
                break else_expr._eval(env);
            }
            EvalResult::Pending(Resumable(cont, _), args) => {
                break EvalResult::Pending(
                    Resumable(
                        Cont::Cnd {
                            value: Box::new(cont),
                            pattern: (tag, vars),
                            then_expr,
                            else_expr,
                        },
                        Arc::clone(&env),
                    ),
                    args,
                );
            }
        }
    }
}

fn eval_try(
    mut expr: EvalResult,
    handlers: Vec<(usize, usize, Expr)>,
    env: &Arc<Env<Val>>,
) -> EvalResult {
    loop {
        match expr {
            EvalResult::Val(Val::Rec(body, env, m)) => {
                let env = env.push(Val::Rec(body.clone(), Arc::clone(&env), m));
                expr = body._eval(&env);
            }
            EvalResult::Val(v) => return EvalResult::Val(v),
            EvalResult::Pending(Resumable(cont, cont_env), Handler { name, args, m }) => {
                if let Some((h, arity, handler)) = handlers.iter().find(|(h, _, _)| *h == name) {
                    if *arity > args.len() {
                        expr = cont.resume(Val::Eff(*h, args, m), &cont_env);
                    } else {
                        expr = handler
                            .clone()
                            ._eval(&env.extend(args).push(Val::Res(cont, cont_env)));
                    }
                } else {
                    return EvalResult::Pending(
                        Resumable(
                            Cont::Try {
                                expr: Box::new(cont),
                                handlers,
                            },
                            cont_env,
                        ),
                        Handler { name, args, m },
                    );
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
    fn eval_if_else_if_else() {
        let code = "let f = x =>
  if x is Foo(x)
    x
  else if x is Bar(x)
    x
  else
    Error

f(Foo(X))";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "X");
    }

    #[test]
    fn eval_rec_fn_app() {
        let code = "loop map = f => xs =>
  if xs is Cons(x, xs)
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
    fn eval_tag_followed_by_group() {
        let code = "let x = Foo

if x is Foo
  (if x is Foo
    x => x
  else
    Nil
  )(x)
else
  Bar";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "Foo");
    }

    #[test]
    fn eval_rec_value_cond() {
        let code = "loop r = Cons(Foo, r)

let head = l =>
  if l is Cons(x, xs)
    x
  else
    Nil

let tail = l =>
  if l is Cons(x, xs)
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
  if x is Suc(x)
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

if Foo(twice2) is Foo(f)
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
  if x is Suc(x)
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
  if x is Suc(x)
    add(x, Suc(y))
  else
    y

if add(Suc(Zero), Zero) is Zero
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
  if x is Suc(x)
    add(x, Suc(y))
  else
    y

if add(Zero, Zero) is Zero
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

    #[test]
    fn eval_print_effect() {
        let code = "print(Hello)";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let handlers = Arc::new(HashMap::from_iter(vec![("print".to_string(), 1)]));
        let mut result = parsed.eval_with_handlers(&handlers).unwrap();
        let mut printed = vec![];
        loop {
            match result {
                EvalResult::Pending(mut cont, mut handler) => {
                    let v = handler.args.pop().unwrap();
                    printed.push(v.to_string());
                    let none = cont.intern("None".to_string());
                    result = cont.resume(none, &handlers).unwrap();
                }
                EvalResult::Val(evaled) => {
                    assert_eq!(format!("{evaled}"), "None");
                    break;
                }
            }
        }
        assert_eq!(printed, vec!["Hello"]);
    }

    #[test]
    fn eval_log_effect() {
        let code = "List(log(Debug, Foo), log(Error, Bar), log(Debug, Baz))";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let handlers = Arc::new(HashMap::from_iter(vec![("log".to_string(), 2)]));
        let mut result = parsed.eval_with_handlers(&handlers).unwrap();
        let mut debug = vec![];
        let mut error = vec![];
        loop {
            match result {
                EvalResult::Pending(cont, mut handler) => {
                    let msg = handler.args.pop().unwrap();
                    let lvl = handler.args.pop().unwrap();
                    if lvl.to_string() == "Debug" {
                        debug.push(msg.to_string());
                    } else if lvl.to_string() == "Error" {
                        error.push(msg.to_string());
                    } else {
                        panic!("Unexpected log level: '{lvl}'")
                    }
                    let unit = cont.unit();
                    result = cont.resume(unit, &handlers).unwrap();
                }
                EvalResult::Val(evaled) => {
                    assert_eq!(format!("{evaled}"), "List((), (), ())");
                    break;
                }
            }
        }
    }

    #[test]
    fn eval_effect_handler() {
        let code = "let foo = x => y => z => Foo(eff(x, y, z))

try
  foo(Bar, Baz, Qux)
catch eff(x, y, z) as resume
  resume(Triple(x, y, z))";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "Foo(Triple(Bar, Baz, Qux))");
    }

    #[test]
    fn eval_handler_with_multiple_resumes() {
        let code = "try
  Foo(eff())
catch eff() as resume
  Pair(resume(Bar), resume(Baz))";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "Pair(Foo(Bar), Foo(Baz))");
    }

    #[test]
    fn eval_effect_handlers() {
        let code = "let baz = x => eff(x)

let f = x =>
  if x is True
    try
      Bar(baz(Qux), baz(Qux))
    catch eff(x) as resume
      resume(Baz(x))
  else
    try
      Bar(baz(Qux), baz(Qux))
    catch eff(x) as resume
      Nil

Pair(f(True), f(False))";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "Pair(Bar(Baz(Qux), Baz(Qux)), Nil)");
    }

    #[test]
    fn eval_effect_handler_toggle() {
        let code = "loop with-toggle = val => resume =>
  try
    resume(val)
  catch toggle() as resume
    if val is True
      with-toggle(False, resume)
    else
      with-toggle(True, resume)

try
  List(toggle(), toggle(), toggle(), toggle())
catch toggle() as resume
  with-toggle(True, resume)";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "List(True, False, True, False)");
    }

    #[test]
    fn eval_effect_handler_get_set() {
        let code = "loop with-state = val => f =>
  try
    f()
  catch get() as resume
    resume(val)
  catch set(x) as resume
    with-state(x, _ => resume())

with-state(None, _ => List(get(), set(Foo), get(), set(Bar), set(Baz), get()))";
        let parsed = parse(code).unwrap();
        assert_eq!(parsed.to_string(), code);
        let evaled = parsed.eval().unwrap();
        assert_eq!(format!("{evaled}"), "List(None, (), Foo, (), (), Baz)");
    }
}
