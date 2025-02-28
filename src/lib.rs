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
}

#[derive(Debug, Clone)]
pub struct InternPool {
    tags: HashMap<String, usize>,
    effects: (HashMap<String, usize>, HashMap<usize, String>),
}

impl InternPool {
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

    fn resolve_var(&mut self, vars: &[String], s: &str) -> ExprEnum {
        for (i, t) in vars.iter().rev().enumerate() {
            if &s == t {
                return ExprEnum::Var(i);
            }
        }
        match self.effects.0.get(s) {
            Some(s) => ExprEnum::Eff(*s),
            None => {
                let n = self.effects.0.len();
                self.effects.0.insert(s.to_string(), n);
                self.effects.1.insert(n, s.to_string());
                ExprEnum::Eff(n)
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
    let mut pool = InternPool {
        tags: HashMap::new(),
        effects: (HashMap::new(), HashMap::new()),
    };
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
                f.write_str("else\n")?;
                indent(f, lvl + 1)?;
                else_expr.pretty(f, false, lvl + 1)?;
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
    Eff(usize, usize, Vec<Val>, Meta),
}

impl From<Val> for Expr {
    fn from(value: Val) -> Self {
        match value {
            Val::Fn(body, _, meta) => Expr(ExprEnum::Abs(Box::new(body)), meta),
            Val::Rec(body, _, meta) => Expr(ExprEnum::Rec(Box::new(body)), meta),
            Val::Tag(t, vals, meta) => {
                let mut expr = Expr(ExprEnum::Tag(t), meta.clone());
                for v in vals {
                    let app = ExprEnum::App(Box::new(expr), Box::new(Expr::from(v)));
                    expr = Expr(app, meta.clone());
                }
                expr
            }
            Val::Eff(e, _, vals, meta) => {
                let mut expr = Expr(ExprEnum::Eff(e), meta.clone());
                for v in vals {
                    let app = ExprEnum::App(Box::new(expr), Box::new(Expr::from(v)));
                    expr = Expr(app, meta.clone());
                }
                expr
            }
        }
    }
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Fn(body, _, meta) => match &meta.alias {
                Some(alias) => write!(f, "{alias} => {body}"),
                None => write!(f, "=> {body}"),
            },
            Val::Rec(body, _, meta) => match &meta.alias {
                Some(alias) => write!(f, "{alias} ~> {body}"),
                None => write!(f, "~> {body}"),
            },
            Val::Tag(s, values, meta) | Val::Eff(s, _, values, meta) => {
                match &meta.alias {
                    Some(alias) => f.write_str(alias)?,
                    None => write!(f, "'{s}")?,
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

#[derive(Debug)]
pub enum Env<T> {
    Empty {
        handlers: Arc<HashMap<String, usize>>,
        pool: Arc<InternPool>,
    },
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

    fn root(self: &Arc<Self>) -> (&Arc<HashMap<String, usize>>, &Arc<InternPool>) {
        match self.as_ref() {
            Env::Empty { handlers, pool } => (handlers, pool),
            Env::Some(_, env) | Env::Tag(_, _, env) => env.root(),
        }
    }

    fn arity(self: &Arc<Self>, handler: usize) -> Option<usize> {
        let (handlers, pool) = self.root();
        match pool.effects.1.get(&handler) {
            Some(alias) => handlers.get(alias).copied(),
            None => None,
        }
    }

    fn tag(self: &Arc<Self>, tag: &str) -> Option<usize> {
        match self.as_ref() {
            Env::Empty { pool, .. } => pool.tags.get(tag).copied(),
            Env::Tag(t, id, _) if t == tag => Some(*id),
            Env::Tag(_, _, env) => env.tag(tag),
            Env::Some(_, env) => env.tag(tag),
        }
    }

    fn intern(self: &Arc<Self>, tag: String) -> (usize, Arc<Self>) {
        let (_, pool) = self.root();
        let t = pool.tags.len();
        (t, Arc::new(Env::Tag(tag, t, Arc::clone(self))))
    }
}

#[derive(Clone)]
enum V {
    Fn(Expr, Arc<Env<V>>, Meta),
    Tag(usize, Vec<V>, Meta),
    Any,
}

#[derive(Clone)]
pub enum EvalResult {
    Val(Val),
    Pending(Resumable, Vec<Val>),
}

#[derive(Clone)]
pub struct Resumable(Cont, Arc<Env<Val>>);

#[derive(Clone)]
enum Cont {
    Effect(Pos),
    AppFn(Box<Cont>, Box<Expr>),
    AppArg(Box<Val>, Box<Cont>),
    Cnd {
        value: Box<Cont>,
        pattern: (usize, usize),
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
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
        self.0.eval_with_handlers(handlers, &Arc::new(self.1))
    }
}

impl Expr {
    fn eval_with_handlers(
        self,
        handlers: &Arc<HashMap<String, usize>>,
        pool: &Arc<InternPool>,
    ) -> Result<EvalResult, String> {
        self.check(
            &Arc::new(Env::Empty {
                handlers: Arc::clone(handlers),
                pool: Arc::clone(&pool),
            }),
            &[],
        )?;
        Ok(self._eval(&Arc::new(Env::Empty {
            handlers: Arc::clone(handlers),
            pool: Arc::clone(&pool),
        })))
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
            ExprEnum::Eff(e) => {
                let arity = env
                    .arity(e)
                    .unwrap_or_else(|| panic!("No handler for {self} at {pos}"));
                EvalResult::Val(Val::Eff(e, arity, vec![], self.1.clone()))
            }
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
        }
    }
}

impl Resumable {
    pub fn resume(self, value: Val) -> EvalResult {
        let Resumable(cont, env) = self;
        cont.resume(value, &env)
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
}

impl Cont {
    fn pos(&self) -> Pos {
        match self {
            Cont::Effect(pos) => *pos,
            Cont::AppFn(cont, _) | Cont::AppArg(_, cont) | Cont::Cnd { value: cont, .. } => {
                cont.pos()
            }
        }
    }

    fn resume(self, value: Val, env: &Arc<Env<Val>>) -> EvalResult {
        match self {
            Cont::Effect(_) => EvalResult::Val(value),
            Cont::AppFn(cont, expr) => match cont.resume(value, env) {
                EvalResult::Val(f) => match expr._eval(env) {
                    EvalResult::Val(arg) => {
                        eval_app(EvalResult::Val(f), Box::new(Expr::from(arg)), env)
                    }
                    EvalResult::Pending(Resumable(cont, env), args) => EvalResult::Pending(
                        Resumable(Cont::AppArg(Box::new(f), Box::new(cont)), env),
                        args,
                    ),
                },
                EvalResult::Pending(Resumable(cont, env), args) => {
                    EvalResult::Pending(Resumable(Cont::AppFn(Box::new(cont), expr), env), args)
                }
            },
            Cont::AppArg(f, cont) => match cont.resume(value, env) {
                EvalResult::Val(arg) => {
                    eval_app(EvalResult::Val(*f), Box::new(Expr::from(arg)), env)
                }
                EvalResult::Pending(Resumable(cont, env), args) => {
                    EvalResult::Pending(Resumable(Cont::AppArg(f, Box::new(cont)), env), args)
                }
            },
            Cont::Cnd {
                value: cont,
                pattern,
                then_expr,
                else_expr,
            } => match cont.resume(value, env) {
                EvalResult::Val(v) => {
                    eval_cnd(EvalResult::Val(v), pattern, then_expr, else_expr, env)
                }
                EvalResult::Pending(Resumable(cont, env), args) => EvalResult::Pending(
                    Resumable(
                        Cont::Cnd {
                            value: Box::new(cont),
                            pattern,
                            then_expr,
                            else_expr,
                        },
                        env,
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
            EvalResult::Val(Val::Fn(body, fn_env, m)) => match arg._eval(env) {
                EvalResult::Val(arg) => break body._eval(&fn_env.push(arg)),
                EvalResult::Pending(Resumable(cont, env), args) => {
                    let f = Box::new(Val::Fn(body, fn_env, m));
                    break EvalResult::Pending(
                        Resumable(Cont::AppArg(f, Box::new(cont)), env),
                        args,
                    );
                }
            },
            EvalResult::Val(Val::Tag(t, mut args, m)) => match arg._eval(env) {
                EvalResult::Val(arg) => {
                    args.push(arg);
                    break EvalResult::Val(Val::Tag(t, args, m));
                }
                EvalResult::Pending(Resumable(cont, env), eff_args) => {
                    let f = Box::new(Val::Tag(t, args, m));
                    break EvalResult::Pending(
                        Resumable(Cont::AppArg(f, Box::new(cont)), env),
                        eff_args,
                    );
                }
            },
            EvalResult::Val(Val::Eff(e, arity, mut args, m)) => match arg._eval(env) {
                EvalResult::Val(arg) => {
                    args.push(arg);
                    if args.len() == arity {
                        break EvalResult::Pending(
                            Resumable(Cont::Effect(m.pos), Arc::clone(env)),
                            args,
                        );
                    } else {
                        break EvalResult::Val(Val::Eff(e, arity, args, m));
                    }
                }
                EvalResult::Pending(Resumable(cont, env), eff_args) => {
                    let f = Box::new(Val::Eff(e, arity, args, m));
                    break EvalResult::Pending(
                        Resumable(Cont::AppArg(f, Box::new(cont)), env),
                        eff_args,
                    );
                }
            },
            EvalResult::Pending(Resumable(cont, env), args) => {
                break EvalResult::Pending(Resumable(Cont::AppFn(Box::new(cont), arg), env), args);
            }
        }
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
            EvalResult::Val(Val::Tag(_, _, _) | Val::Fn(_, _, _) | Val::Eff { .. }) => {
                break else_expr._eval(env);
            }
            EvalResult::Pending(Resumable(cont, env), args) => {
                break EvalResult::Pending(
                    Resumable(
                        Cont::Cnd {
                            value: Box::new(cont),
                            pattern: (tag, vars),
                            then_expr,
                            else_expr,
                        },
                        env,
                    ),
                    args,
                );
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
        loop {
            match result {
                EvalResult::Pending(mut cont, mut vals) => {
                    let v = vals.pop().unwrap();
                    println!("{v}");
                    let none = cont.intern("None".to_string());
                    result = cont.resume(none)
                }
                EvalResult::Val(evaled) => {
                    assert_eq!(format!("{evaled}"), "None");
                    break;
                }
            }
        }
    }
}
