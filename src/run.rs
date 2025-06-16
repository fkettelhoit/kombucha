use crate::bytecode::{Bytecode, NIL, Op, Syn};
use std::{rc::Rc, usize};

impl Bytecode {
    pub fn run(self) -> Result<State, usize> {
        let mut vm = Vm::default();
        vm.ip = self.start;
        vm.run(self)
    }
}

#[derive(Debug, Clone)]
pub enum Val {
    String(usize),
    Effect(usize),
    Record(usize, Vec<Rc<Val>>),
    Closure(usize, Rc<Vec<Val>>),
    Resumable(usize, Box<Vm>),
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
    handlers: Vec<Handler>,
}

#[derive(Debug, Clone)]
struct Handler {
    effect: usize,
    handler: Val,
    state: (usize, usize, usize, usize),
    ret: usize,
}

impl Vm {
    fn run(self, bytecode: Bytecode) -> Result<State, usize> {
        let Vm { mut ip, mut vars, mut temps, mut frames, mut handlers } = self;
        loop {
            let op = bytecode.ops.get(ip).copied().ok_or(ip)?;
            let i = ip;
            ip += 1;
            match op {
                Op::LoadVar(v) => {
                    let v: &Val = &vars[vars.len() - 1 - v];
                    temps.push(v.clone());
                }
                Op::LoadString(s) => temps.push(Val::String(s)),
                Op::LoadEffect(eff) => temps.push(Val::Effect(eff)),
                Op::LoadFn { code: _, fvars } if fvars > vars.len() => return Err(ip),
                Op::LoadFn { code, fvars } => {
                    let captured = Rc::new(vars[vars.len() - fvars..].to_vec());
                    temps.push(Val::Closure(code, captured))
                }
                Op::AppFnToArg | Op::AppArgToFn => {
                    let (arg, f) = match (op, temps.pop().ok_or(i)?, temps.pop().ok_or(i)?) {
                        (Op::AppFnToArg, b, a) => (b, a),
                        (_, b, a) => (a, b),
                    };
                    match (f, arg) {
                        (Val::Effect(effect), arg) => {
                            let handler = handlers.iter().rev().find(|h| h.effect == effect);
                            match handler.cloned() {
                                Some(handler) => {
                                    let (v, t, f, h) = handler.state;
                                    let res_vars = vars.drain(v..).collect::<Vec<_>>();
                                    let res_temps = temps.drain(t..).collect::<Vec<_>>();
                                    let res_frames = frames.drain(f..).collect::<Vec<_>>();
                                    let res_handlers = handlers.drain(h..).collect::<Vec<_>>();
                                    handlers.pop();
                                    let vm = Vm {
                                        ip,
                                        vars: res_vars,
                                        temps: res_temps,
                                        frames: res_frames,
                                        handlers: res_handlers,
                                    };
                                    temps.push(arg);
                                    temps.push(Val::Resumable(v, Box::new(vm)));
                                    temps.push(handler.handler);
                                    ip = handler.ret;
                                }
                                None => {
                                    let vm = Vm { ip, vars, temps, frames, handlers };
                                    let arg = Value { bytecode, val: arg };
                                    let r = Resumable { effect, arg, vm };
                                    return Ok(State::Resumable(r));
                                }
                            }
                        }
                        (Val::Resumable(v, vm), arg) => {
                            let offset = vars.len() as i64 - v as i64;
                            frames.push((vars.len(), ip));
                            vars.extend(vm.vars);
                            temps.extend(vm.temps);
                            frames.extend(
                                vm.frames
                                    .into_iter()
                                    .map(|(v, ret)| ((v as i64 + offset) as usize, ret)),
                            );
                            handlers.extend(vm.handlers);
                            temps.push(arg);
                            ip = vm.ip;
                        }
                        (Val::Closure(c, captured), arg) => {
                            // TODO: if the next op is an Op::Return, we might want to do TCO
                            frames.push((vars.len(), ip));
                            vars.extend(captured.iter().cloned());
                            vars.push(arg);
                            ip = c;
                        }
                        (Val::String(s), arg) => temps.push(Val::Record(s, vec![Rc::new(arg)])),
                        (Val::Record(s, mut items), arg) => {
                            items.push(Rc::new(arg));
                            temps.push(Val::Record(s, items))
                        }
                    }
                }
                Op::Return if frames.is_empty() => {
                    let val = temps.pop().ok_or(ip)?;
                    return Ok(State::Done(Value { val, bytecode }));
                }
                Op::Return => {
                    let (frame, ret) = frames.pop().ok_or(i)?;
                    vars.truncate(frame);
                    ip = ret
                }
                Op::Unpack => {
                    match (temps.pop().ok_or(i)?, temps.pop().ok_or(i)?, temps.pop().ok_or(i)?) {
                        (_, t, Val::Record(f, mut xs)) => {
                            let x = xs.pop().ok_or(i)?;
                            temps.push(x.as_ref().clone());
                            temps.push(if xs.is_empty() {
                                Val::String(f)
                            } else {
                                Val::Record(f, xs)
                            });
                            temps.push(t);
                        }
                        (f, _, _) => {
                            temps.push(Val::String(Syn::Nil as usize));
                            temps.push(f);
                            ip += 1;
                        }
                    }
                }
                Op::Try => {
                    let handler = temps.pop().ok_or(i)?;
                    let eff = temps.pop().ok_or(i)?;
                    let v = temps.pop().ok_or(i)?;
                    match eff {
                        Val::Effect(effect) => {
                            let state = (vars.len(), temps.len(), frames.len(), handlers.len() + 1);
                            let ret = ip + 2; // skip apply + unwind
                            handlers.push(Handler { effect, handler, state, ret });
                            temps.push(Val::String(Syn::Nil as usize));
                            temps.push(v);
                        }
                        _ => {
                            temps.push(v);
                            ip += 4; // skip apply, unwind, apply, apply
                        }
                    }
                }
                Op::Unwind => {
                    handlers.pop();
                    ip += 2;
                }
                Op::Cmp => {
                    let (f, t, b, a) = (
                        temps.pop().ok_or(i)?,
                        temps.pop().ok_or(i)?,
                        temps.pop().ok_or(i)?,
                        temps.pop().ok_or(i)?,
                    );
                    let branch = match (a, b, t, f) {
                        (Val::String(a), Val::String(b), t, _) if a == b => t,
                        (_, _, _, f) => f,
                    };
                    temps.push(Val::String(Syn::Nil as usize));
                    temps.push(branch);
                }
            }
        }
    }
}

fn pretty(v: &Val, strs: &Vec<String>) -> String {
    match v {
        Val::String(s) if strs[*s] == NIL => "[]".to_string(),
        Val::String(s) => strs[*s].to_string(),
        Val::Effect(s) => format!("{}!", strs[*s]),
        Val::Closure(c, _) => format!("#fn-{c}"),
        Val::Record(s, vs) if strs[*s] == NIL => {
            let items = vs.iter().rev().map(|v| pretty(v, strs)).collect::<Vec<_>>();
            format!("[{}]", items.join(", "))
        }
        Val::Record(s, vs) => {
            if vs.len() == 1 {
                match *vs[0] {
                    Val::String(v) if strs[v] == NIL => {
                        return format!("{}()", pretty(&Val::String(*s), strs));
                    }
                    _ => {}
                }
            }
            let items = vs.iter().map(|v| pretty(v, strs)).collect::<Vec<_>>();
            format!("{}({})", strs[*s].to_string(), items.join(", "))
        }
        Val::Resumable(_, _) => format!("#resumable"),
    }
}

impl Value {
    pub fn pretty(&self) -> String {
        pretty(&self.val, &self.bytecode.ctx.strs)
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
        self.vm.run(self.arg.bytecode)
    }

    pub fn resume_at(mut self, start: usize) -> Result<State, usize> {
        self.vm.frames.push((self.vm.vars.len(), self.vm.ip));
        self.vm.ip = start;
        self.vm.run(self.arg.bytecode)
    }
}

pub(crate) fn intern_atom(strs: &mut Vec<String>, s: String) -> Val {
    Val::String(intern(strs, s))
}

pub(crate) fn intern_string(strs: &mut Vec<String>, s: impl AsRef<str>) -> Val {
    Val::String(intern(strs, format!("\"{}\"", s.as_ref())))
}

pub(crate) fn intern(strs: &mut Vec<String>, s: String) -> usize {
    strs.iter().position(|x| *x == s).unwrap_or_else(|| {
        strs.push(s);
        strs.len() - 1
    })
}
