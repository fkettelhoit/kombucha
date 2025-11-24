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
    Closure(usize, Rc<Vec<Val>>),
    Resumable(usize, Rc<Vm>),
}

#[derive(Debug, Clone)]
pub enum List {
    Val(Val),
    Cons(Rc<List>, Val),
}

impl List {
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
    pub arg: Value,
    pub vm: Vm,
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
pub struct Profiler([Metric; 15]);

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
        let Vm { mut ip, mut vars, mut temps, mut frames } = self;
        let mut handlers: Vec<Handler> = vec![];
        loop {
            let time = Instant::now();
            let op = bytecode.ops.get(ip).copied().ok_or(ip)?;
            let i = ip;
            ip += 1;
            match op {
                Op::LoadVar(v) => {
                    let v: &Val = &vars[vars.len() - 1 - v];
                    temps.push(v.clone());
                    profiler.clock(time, Measure::LoadVar);
                }
                Op::LoadString(s) => temps.push(Val::String(s)),
                Op::LoadEffect(eff) => temps.push(Val::Effect(eff)),
                Op::LoadFn { code: _, fvars } if fvars > vars.len() => return Err(ip),
                Op::LoadFn { code, fvars } => match bytecode.ops.get(i + 1).ok_or(i)? {
                    Op::Apply => {
                        frames.push((vars.len(), ip + 1));
                        vars.push(temps.pop().ok_or(ip)?);
                        ip = code;
                        profiler.clock(time, Measure::LoadFn);
                    }
                    Op::Return => {
                        let (_, ret) = *frames.last().ok_or(ip)?;
                        match bytecode.ops.get(ret).ok_or(ip)? {
                            Op::Apply => {
                                let (frame, ret) = frames.pop().ok_or(ip)?;
                                let arg = temps.pop().ok_or(ip + 1)?;
                                frames.push((frame, ret + 1));
                                vars.push(arg);
                                ip = code;
                                profiler.clock(time, Measure::LoadFn);
                            }
                            _ => {
                                let captured = Rc::new(vars[vars.len() - fvars..].to_vec());
                                temps.push(Val::Closure(code, captured));
                                profiler.clock(time, Measure::LoadClosure);
                            }
                        }
                    }
                    _ => {
                        let captured = Rc::new(vars[vars.len() - fvars..].to_vec());
                        temps.push(Val::Closure(code, captured));
                        profiler.clock(time, Measure::LoadClosure);
                    }
                },
                Op::Apply => {
                    let (f, arg) = (temps.pop().ok_or(i)?, temps.pop().ok_or(i)?);
                    match (f, arg) {
                        (Val::Effect(effect), arg) => match handlers.pop() {
                            Some(handler) => {
                                let (v, t, f) = handler.state;
                                let r_vars = vars.drain(v..).collect::<Vec<_>>();
                                let r_temps = temps.drain(t..).collect::<Vec<_>>();
                                let r_frames = frames.drain(f..).collect::<Vec<_>>();
                                let vm = Vm { ip, vars: r_vars, temps: r_temps, frames: r_frames };
                                temps.push(arg);
                                temps.push(Val::Effect(effect));
                                temps.push(Val::Resumable(v, Rc::new(vm)));
                                temps.push(handler.handler);
                                ip = handler.ret;
                                profiler.clock(time, Measure::AppEffectHandler);
                            }
                            None => {
                                profiler.clock(time, Measure::AppEffectPause);
                                let vm = Vm { ip, vars, temps, frames };
                                let arg = Value { bytecode, val: arg, profiler };
                                let r = Resumable { effect, arg, vm };
                                return Ok(State::Resumable(r));
                            }
                        },
                        (Val::Resumable(v, vm), arg) => {
                            let offset = vars.len() as i64 - v as i64;
                            frames.push((vars.len(), ip));
                            vars.extend(vm.vars.clone());
                            temps.extend(vm.temps.clone());
                            frames.extend(
                                vm.frames
                                    .iter()
                                    .map(|(v, ret)| ((*v as i64 + offset) as usize, *ret)),
                            );
                            temps.push(arg);
                            ip = vm.ip;
                            profiler.clock(time, Measure::AppResumable);
                        }
                        (Val::Closure(c, captured), arg) => {
                            // TODO: if the next op is an Op::Return, we might want to do TCO
                            frames.push((vars.len(), ip));
                            vars.extend(captured.iter().cloned());
                            vars.push(arg);
                            ip = c;
                            profiler.clock(time, Measure::AppClosure);
                        }
                        (Val::String(s), arg) => {
                            temps.push(Val::Struct(s, Rc::new(List::Val(arg))));
                            profiler.clock(time, Measure::AppString);
                        }
                        (Val::Struct(s, items), arg) => {
                            temps.push(Val::Struct(s, Rc::new(List::Cons(items, arg))));
                            profiler.clock(time, Measure::AppStruct);
                        }
                    }
                }
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
                        Val::Effect(_) | Val::Closure(_, _) | Val::Resumable(_, _) => {
                            temps.push(Val::String(Str::TyFunction as usize))
                        }
                    }
                    profiler.clock(time, Measure::Type);
                }
                Op::Unpack => {
                    match (temps.pop().ok_or(i)?, temps.pop().ok_or(i)?, temps.pop().ok_or(i)?) {
                        (_, t, Val::Struct(f, xs)) => match xs.as_ref() {
                            List::Val(x) => {
                                temps.push(x.clone());
                                temps.push(Val::String(f));
                                temps.push(t);
                            }
                            List::Cons(xs, x) => {
                                temps.push(x.clone());
                                temps.push(Val::Struct(f, Rc::clone(xs)));
                                temps.push(t);
                            }
                        },
                        (f, _, _) => {
                            temps.push(Val::String(Str::Null as usize));
                            temps.push(f);
                            ip += 1;
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
                    temps.push(Val::String(Str::Null as usize));
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
            Val::Closure(c, _) => format!("#fn-{c}"),
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
        self.arg.bytecode.ctx.strs.get(self.effect).map(|s| s.as_ref()).unwrap_or_default()
    }

    pub fn intern_atom(&mut self, s: String) -> Val {
        Val::String(intern(&mut self.arg.bytecode.ctx.strs, s))
    }

    pub fn intern_string(&mut self, s: impl AsRef<str>) -> Val {
        Val::String(intern(&mut self.arg.bytecode.ctx.strs, format!("\"{}\"", s.as_ref())))
    }

    pub fn resume(mut self, arg: Val) -> Result<State, usize> {
        self.vm.temps.push(arg);
        self.vm.run(self.arg.bytecode, self.arg.profiler)
    }

    pub fn resume_at(mut self, start: usize) -> Result<State, usize> {
        self.vm.frames.push((self.vm.vars.len(), self.vm.ip));
        self.vm.ip = start;
        self.vm.run(self.arg.bytecode, self.arg.profiler)
    }
}

pub(crate) fn intern(strs: &mut Vec<String>, s: String) -> usize {
    strs.iter().position(|x| *x == s).unwrap_or_else(|| {
        strs.push(s);
        strs.len() - 1
    })
}
