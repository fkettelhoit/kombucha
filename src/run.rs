use crate::bytecode::{Bytecode, LIST, NULL, Op, Str};
use std::{rc::Rc, time::Instant, usize};

impl Bytecode {
    pub fn run(self) -> Result<State, usize> {
        let mut vm = Vm::default();
        vm.ip = self.start;
        vm.run(self, Profiler::default())
    }
}

#[derive(Debug, Clone)]
pub enum Val {
    String(usize),
    Effect(usize),
    Struct(usize, Rc<List>),
    Fn { code: usize, params: usize },
    Closure { code: usize, params: usize, env: Rc<Vec<Val>> },
    Resumable(usize, Rc<Vm>),
}

#[derive(Debug, Clone)]
pub enum List {
    Val(Val),
    Cons(Rc<List>, Val),
}

impl List {
    fn new(items: Vec<Val>) -> Option<List> {
        let mut items = items.into_iter();
        let Some(first) = items.next() else {
            return None;
        };
        let mut l = List::Val(first);
        for item in items {
            l = List::Cons(Rc::new(l), item);
        }
        Some(l)
    }

    fn append(self: Rc<Self>, items: Vec<Val>) -> Rc<List> {
        let mut l = self;
        for item in items {
            l = Rc::new(List::Cons(l, item))
        }
        l
    }

    pub(crate) fn to_vec(&self) -> Vec<&Val> {
        let mut vals = vec![];
        let mut list = self;
        loop {
            match list {
                List::Val(val) => {
                    vals.push(val);
                    vals.reverse();
                    return vals;
                }
                List::Cons(l, val) => {
                    vals.push(val);
                    list = l;
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum State {
    Done(Value),
    Resumable(Resumable),
}

#[derive(Debug)]
pub struct Value {
    pub bytecode: Bytecode,
    pub val: Val,
    pub profiler: Profiler,
}

#[derive(Debug)]
pub struct Resumable {
    pub effect: usize,
    pub bytecode: Bytecode,
    pub args: Vec<Val>,
    pub vm: Vm,
    pub profiler: Profiler,
}

#[derive(Debug, Clone, Default)]
pub struct Vm {
    ip: usize,
    vars: Vec<Val>,
    temps: Vec<Val>,
    frames: Vec<(usize, usize)>,
}

#[derive(Debug, Clone)]
struct Handler {
    handler: Val,
    state: (usize, usize, usize),
    ret: usize,
}

#[derive(Debug, Clone, Default)]
pub struct Profiler([Metric; 16]);

#[derive(Debug, Clone, Default)]
pub struct Metric {
    elapsed: u128,
    count: u128,
}

#[derive(Debug, Clone, Copy)]
enum Measure {
    LoadVar,
    LoadFn,
    LoadClosure,
    AppEffectHandler,
    AppEffectPause,
    AppResumable,
    AppFn,
    AppClosure,
    AppString,
    AppStruct,
    Return,
    Type,
    Unpack,
    Try,
    Unwind,
    Compare,
}

impl Profiler {
    fn clock(&mut self, time: Instant, m: Measure) {
        let elapsed = time.elapsed().as_nanos();
        let m = m as usize;
        self.0[m].elapsed += elapsed;
        self.0[m].count += 1;
    }
}

impl std::fmt::Display for Profiler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("\n### PROFILE ###\n")?;
        let measurements = [
            Measure::LoadVar,
            Measure::LoadFn,
            Measure::LoadClosure,
            Measure::AppEffectHandler,
            Measure::AppEffectPause,
            Measure::AppResumable,
            Measure::AppFn,
            Measure::AppClosure,
            Measure::AppString,
            Measure::AppStruct,
            Measure::Return,
            Measure::Type,
            Measure::Unpack,
            Measure::Try,
            Measure::Unwind,
            Measure::Compare,
        ];
        let mut total = 0;
        for m in measurements {
            let Metric { elapsed, count } = self.0[m as usize];
            if count > 0 {
                writeln!(
                    f,
                    "{:20} {:>10.3} ns (avg) * {:>10} = {:>10} ms",
                    format!("{:?}", m),
                    elapsed as f64 / count as f64,
                    count,
                    elapsed / 1000_000
                )?;
            }
            total += elapsed;
        }
        write!(f, "total: {} ms", total / 1000_000)?;
        Ok(())
    }
}

impl Vm {
    fn run(self, bytecode: Bytecode, mut profiler: Profiler) -> Result<State, usize> {
        println!("\n\n");
        let Vm { mut ip, mut vars, mut temps, mut frames } = self;
        let mut handlers: Vec<Handler> = vec![];
        loop {
            let time = Instant::now();
            let op = bytecode.ops.get(ip).copied().ok_or(ip)?;
            let i = ip;
            ip += 1;
            for v in temps.iter() {
                println!("  {}", v.pretty(&bytecode.ctx.strs));
            }
            println!("{i}: {op:?}");
            match op {
                Op::LoadVar(v) => {
                    let v: &Val = &vars[vars.len() - 1 - v];
                    temps.push(v.clone());
                    profiler.clock(time, Measure::LoadVar);
                }
                Op::LoadString(s) => temps.push(Val::String(s)),
                Op::LoadEffect(eff) => temps.push(Val::Effect(eff)),
                Op::LoadFn { code: _, params: _, fvars } if fvars > vars.len() => return Err(i),
                Op::LoadFn { code, params, fvars: 0 } => {
                    temps.push(Val::Fn { code, params });
                    profiler.clock(time, Measure::LoadFn);
                }
                Op::LoadFn { code, params, fvars } => {
                    let env = Rc::new(vars[vars.len() - fvars..].to_vec());
                    temps.push(Val::Closure { code, params, env });
                    profiler.clock(time, Measure::LoadClosure);
                }
                Op::App(params) => {
                    let f = temps.pop().ok_or(i)?;
                    if temps.len() < params {
                        return Err(i);
                    }
                    // TODO: do we need to reverse these?
                    let args: Vec<_> = temps.drain(temps.len() - params..).collect();
                    match f {
                        Val::Effect(effect) => match handlers.pop() {
                            Some(handler) => {
                                let (v, t, f) = handler.state;
                                let r_vars = vars.drain(v..).collect::<Vec<_>>();
                                let r_temps = temps.drain(t..).collect::<Vec<_>>();
                                let r_frames = frames.drain(f..).collect::<Vec<_>>();
                                let vm = Vm { ip, vars: r_vars, temps: r_temps, frames: r_frames };
                                temps.extend(args);
                                temps.push(Val::Effect(effect));
                                temps.push(Val::Resumable(v, Rc::new(vm)));
                                temps.push(handler.handler);
                                ip = handler.ret;
                                profiler.clock(time, Measure::AppEffectHandler);
                            }
                            None => {
                                profiler.clock(time, Measure::AppEffectPause);
                                let vm = Vm { ip, vars, temps, frames };
                                let r = Resumable { effect, bytecode, args, vm, profiler };
                                return Ok(State::Resumable(r));
                            }
                        },
                        Val::Resumable(v, vm) => {
                            let offset = vars.len() as i64 - v as i64;
                            frames.push((vars.len(), ip));
                            vars.extend(vm.vars.clone());
                            temps.extend(vm.temps.clone());
                            frames.extend(
                                vm.frames
                                    .iter()
                                    .map(|(v, ret)| ((*v as i64 + offset) as usize, *ret)),
                            );
                            temps.extend(args);
                            ip = vm.ip;
                            profiler.clock(time, Measure::AppResumable);
                        }
                        Val::Fn { code, params } => {
                            if params != args.len() {
                                return Err(i);
                            }
                            // TODO: if the next op is an Op::Return, we might want to do TCO
                            frames.push((vars.len(), ip));
                            vars.extend(args);
                            ip = code;
                            profiler.clock(time, Measure::AppFn);
                        }
                        Val::Closure { code, params, env } => {
                            if params != args.len() {
                                return Err(i);
                            }
                            // TODO: if the next op is an Op::Return, we might want to do TCO
                            frames.push((vars.len(), ip));
                            vars.extend(env.iter().cloned());
                            vars.extend(args);
                            ip = code;
                            profiler.clock(time, Measure::AppClosure);
                        }
                        Val::String(s) => {
                            match List::new(args) {
                                None => temps.push(Val::String(s)),
                                Some(args) => temps.push(Val::Struct(s, Rc::new(args))),
                            }
                            profiler.clock(time, Measure::AppString);
                        }
                        Val::Struct(s, items) => {
                            temps.push(Val::Struct(s, List::append(items, args)));
                            profiler.clock(time, Measure::AppStruct);
                        }
                    }
                }
                Op::Fix => todo!(),
                Op::Return if frames.is_empty() => {
                    let val = temps.pop().ok_or(ip)?;
                    return Ok(State::Done(Value { val, bytecode, profiler }));
                }
                Op::Return => {
                    let (frame, ret) = frames.pop().ok_or(i)?;
                    vars.truncate(frame);
                    ip = ret;
                    profiler.clock(time, Measure::Return);
                }
                Op::Type => {
                    match temps.pop().ok_or(i)? {
                        Val::String(s) if s == Str::Null as usize => {
                            temps.push(Val::String(Str::TyNull as usize))
                        }
                        Val::String(s) if s == Str::List as usize => {
                            temps.push(Val::String(Str::TyList as usize))
                        }
                        Val::String(_) => temps.push(Val::String(Str::TyString as usize)),
                        Val::Struct(_, _) => temps.push(Val::String(Str::TyStruct as usize)),
                        Val::Effect(_)
                        | Val::Fn { .. }
                        | Val::Closure { .. }
                        | Val::Resumable(_, _) => temps.push(Val::String(Str::TyFunction as usize)),
                    }
                    profiler.clock(time, Measure::Type);
                }
                Op::Unpack { if_true, if_false } => {
                    match temps.pop().ok_or(i)? {
                        Val::Struct(f, xs) => {
                            println!("unpack true");
                            let (rest, x) = match xs.as_ref() {
                                List::Val(x) => (Val::String(f), x),
                                List::Cons(xs, x) => (Val::Struct(f, Rc::clone(xs)), x),
                            };
                            temps.extend([rest, x.clone()]);
                            frames.push((vars.len(), ip));
                            ip = if_true;
                        }
                        _ => {
                            frames.push((vars.len(), ip));
                            ip = if_false
                        }
                    }
                    profiler.clock(time, Measure::Unpack);
                }
                Op::Try => {
                    let handler = temps.pop().ok_or(i)?;
                    let v = temps.pop().ok_or(i)?;
                    let state = (vars.len(), temps.len(), frames.len());
                    let ret = ip + 2; // skip apply + unwind
                    handlers.push(Handler { handler, state, ret });
                    temps.push(Val::String(Str::Null as usize));
                    temps.push(v);
                    profiler.clock(time, Measure::Try);
                }
                Op::Unwind => {
                    handlers.pop();
                    ip += 3;
                    profiler.clock(time, Measure::Unwind);
                }
                Op::Compare => {
                    let (f, t, b, a) = (
                        temps.pop().ok_or(i)?,
                        temps.pop().ok_or(i)?,
                        temps.pop().ok_or(i)?,
                        temps.pop().ok_or(i)?,
                    );
                    let branch = match (a, b, t, f) {
                        (Val::String(a), Val::String(b), t, _) if a == b => t,
                        (Val::Effect(a), Val::Effect(b), t, _) if a == b => t,
                        (_, _, _, f) => f,
                    };
                    temps.push(branch);
                    profiler.clock(time, Measure::Compare);
                }
            }
        }
    }
}

impl Val {
    pub fn pretty(&self, strs: &Vec<String>) -> String {
        match self {
            Val::String(s) => strs[*s].to_string(),
            Val::Effect(s) => format!("{}!", strs[*s]),
            Val::Fn { code, params } => format!("#fn/{params}@{code}"),
            Val::Closure { code, params, .. } => format!("#closure/{params}@{code}"),
            Val::Struct(s, vs) if strs[*s] == LIST => {
                let items = vs.to_vec().iter().map(|v| v.pretty(strs)).collect::<Vec<_>>();
                format!("[{}]", items.join(", "))
            }
            Val::Struct(s, vs) => match vs.as_ref() {
                List::Val(Val::String(v)) if strs[*v] == NULL => {
                    format!("{}()", Val::String(*s).pretty(strs))
                }
                _ => {
                    let items = vs.to_vec().iter().map(|v| v.pretty(strs)).collect::<Vec<_>>();
                    format!("{}({})", strs[*s].to_string(), items.join(", "))
                }
            },
            Val::Resumable(_, _) => format!("#resumable"),
        }
    }
}

impl Value {
    pub fn pretty(&self) -> String {
        self.val.pretty(&self.bytecode.ctx.strs)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.pretty())
    }
}

impl Resumable {
    pub fn effect(&self) -> &str {
        self.bytecode.ctx.strs.get(self.effect).map(|s| s.as_ref()).unwrap_or_default()
    }

    pub fn intern_atom(&mut self, s: String) -> Val {
        Val::String(intern(&mut self.bytecode.ctx.strs, s))
    }

    pub fn intern_string(&mut self, s: impl AsRef<str>) -> Val {
        Val::String(intern(&mut self.bytecode.ctx.strs, format!("\"{}\"", s.as_ref())))
    }

    pub fn args_pretty(&self) -> Vec<String> {
        self.args.iter().map(|arg| arg.pretty(&self.bytecode.ctx.strs)).collect()
    }

    pub fn resume(mut self, args: Vec<Val>) -> Result<State, usize> {
        self.vm.temps.extend(args);
        self.vm.run(self.bytecode, self.profiler)
    }

    pub fn resume_at(mut self, start: usize) -> Result<State, usize> {
        self.vm.frames.push((self.vm.vars.len(), self.vm.ip));
        self.vm.ip = start;
        self.vm.run(self.bytecode, self.profiler)
    }
}

pub(crate) fn intern(strs: &mut Vec<String>, s: String) -> usize {
    strs.iter().position(|x| *x == s).unwrap_or_else(|| {
        strs.push(s);
        strs.len() - 1
    })
}
