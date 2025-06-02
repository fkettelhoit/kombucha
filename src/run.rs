use crate::bytecode::{Bytecode, NIL, Op, Reflect};
use std::{borrow::Cow, rc::Rc};

#[derive(Debug, Clone)]
pub enum V {
    Fn(usize, usize, usize),
    String(usize),
    Effect(usize),
    Record(usize, Vec<Rc<V>>),
    Closure(usize, Rc<Vec<V>>),
    Resumable(usize, Box<Vm>),
}

#[derive(Debug, Clone, Default)]
pub struct Vm {
    ip: usize,
    vars: Vec<V>,
    temps: Vec<V>,
    frames: Vec<(usize, usize)>,
    handlers: Vec<Handler>,
}

#[derive(Debug, Clone)]
struct Handler {
    effect: usize,
    handler: V,
    state: (usize, usize, usize, usize),
    ret: usize,
}

#[derive(Debug)]
pub enum VmState {
    Done(V, Vec<Cow<'static, str>>),
    Resumable(Resumable, V),
}

#[derive(Debug)]
pub struct Resumable(Bytecode, Vm, usize);

impl Resumable {
    pub fn strings(&self) -> &Vec<Cow<'static, str>> {
        &self.0.strings
    }

    pub fn get_effect_name(&self) -> Option<&str> {
        let Resumable(code, _, eff) = self;
        code.strings.get(*eff).map(|s| s.as_ref())
    }

    pub fn intern_string(&mut self, s: impl Into<Cow<'static, str>>) -> V {
        let s = s.into();
        let Resumable(bc, _, _) = self;
        let s = bc.strings.iter().position(|x| *x == s).unwrap_or_else(|| {
            bc.strings.push(s);
            bc.strings.len() - 1
        });
        V::String(s)
    }

    pub fn run(self, arg: V) -> Result<VmState, usize> {
        let Resumable(bc, mut vm, _) = self;
        vm.temps.push(arg);
        vm.run(bc)
    }
}

impl Bytecode {
    pub fn nil() -> V {
        V::String(Reflect::Nil as usize)
    }

    pub fn run(self) -> Result<VmState, usize> {
        let mut vm = Vm::default();
        vm.ip = self.start;
        vm.run(self)
    }
}

impl Vm {
    fn run(self, code: Bytecode) -> Result<VmState, usize> {
        let Vm {
            mut ip,
            mut vars,
            mut temps,
            mut frames,
            mut handlers,
        } = self;
        while let Some(op) = code.ops.get(ip).copied() {
            let i = ip;
            ip += 1;
            match op {
                Op::LoadVar(v) => {
                    let v: &V = &vars[vars.len() - 1 - v];
                    temps.push(v.clone());
                }
                Op::LoadString(s) => temps.push(V::String(s)),
                Op::LoadEffect(eff) => temps.push(V::Effect(eff)),
                Op::LoadFn { code, fvars } => temps.push(V::Fn(code, fvars, frames.len())),
                Op::ApplyFnToArg | Op::ApplyArgToFn => {
                    let (mut arg, f) = match (op, temps.pop().ok_or(i)?, temps.pop().ok_or(i)?) {
                        (Op::ApplyFnToArg, b, a) => (b, a),
                        (_, b, a) => (a, b),
                    };
                    if let V::Fn(c, v, _) = arg {
                        arg = V::Closure(c, Rc::new(vars[vars.len() - v..].to_vec()));
                    }
                    match (f, arg) {
                        (V::Effect(eff), arg) => {
                            let handler = handlers.iter().rev().find(|h| h.effect == eff);
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
                                    temps.push(V::Resumable(v, Box::new(vm)));
                                    temps.push(handler.handler);
                                    ip = handler.ret;
                                }
                                None => {
                                    let vm = Vm {
                                        ip,
                                        vars,
                                        temps,
                                        frames,
                                        handlers,
                                    };
                                    return Ok(VmState::Resumable(Resumable(code, vm, eff), arg));
                                }
                            }
                        }
                        (V::Resumable(v, vm), arg) => {
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
                        (V::Closure(c, captured), arg) => {
                            // TODO: if the next op is an Op::Return, we might want to do TCO
                            frames.push((vars.len(), ip));
                            vars.extend(captured.iter().cloned());
                            vars.push(arg);
                            ip = c;
                        }
                        (V::Fn(c, _, _), arg) => {
                            frames.push((vars.len(), ip));
                            vars.push(arg);
                            ip = c;
                        }
                        (V::String(s), arg) => temps.push(V::Record(s, vec![Rc::new(arg)])),
                        (V::Record(s, mut items), arg) => {
                            items.push(Rc::new(arg));
                            temps.push(V::Record(s, items))
                        }
                    }
                }
                Op::Return => {
                    let (frame, ret) = frames.pop().ok_or(i)?;
                    match (temps.last().cloned(), code.ops.get(ret)) {
                        (Some(V::Fn(c, v, f)), _) if f == frames.len() + 1 => {
                            let _f = temps.pop();
                            temps.push(V::Closure(c, Rc::new(vars[vars.len() - v..].to_vec())));
                            vars.truncate(frame);
                            ip = ret
                        }
                        _ => {
                            vars.truncate(frame);
                            ip = ret
                        }
                    }
                }
                Op::Unpack => match (
                    temps.pop().ok_or(i)?,
                    temps.pop().ok_or(i)?,
                    temps.pop().ok_or(i)?,
                ) {
                    (_, t, V::Record(f, mut xs)) => {
                        let x = xs.pop().ok_or(i)?;
                        temps.push(x.as_ref().clone());
                        temps.push(if xs.is_empty() {
                            V::String(f)
                        } else {
                            V::Record(f, xs)
                        });
                        temps.push(t);
                    }
                    (f, _, _) => {
                        temps.push(V::String(Reflect::Nil as usize));
                        temps.push(f);
                        ip += 1;
                    }
                },
                Op::Try => {
                    let mut handler = temps.pop().ok_or(i)?;
                    let eff = temps.pop().ok_or(i)?;
                    let v = temps.pop().ok_or(i)?;
                    if let V::Fn(c, v, _) = handler {
                        handler = V::Closure(c, Rc::new(vars[vars.len() - v..].to_vec()));
                    }
                    match eff {
                        V::Effect(effect) => {
                            let state = (vars.len(), temps.len(), frames.len(), handlers.len() + 1);
                            let ret = ip + 2; // skip apply + unwind
                            handlers.push(Handler {
                                effect,
                                handler,
                                state,
                                ret,
                            });
                            temps.push(V::String(Reflect::Nil as usize));
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
                        (V::String(a), V::String(b), t, _) if a == b => t,
                        (_, _, _, f) => f,
                    };
                    temps.push(branch);
                    temps.push(V::String(Reflect::Nil as usize));
                }
            }
        }
        Ok(VmState::Done(temps.pop().ok_or(ip)?, code.strings))
    }
}

pub fn pretty(v: &V, strs: &Vec<Cow<'static, str>>) -> String {
    match v {
        V::String(s) if strs[*s] == NIL => "[]".to_string(),
        V::String(s) => strs[*s].to_string(),
        V::Effect(eff) => format!("{}!", strs[*eff]),
        V::Fn(c, v, frame) => format!("{c}(captured:{v},frame:{frame})"),
        V::Closure(c, vs) => {
            let closed = vs.iter().map(|v| pretty(v, strs)).collect::<Vec<_>>();
            format!("{c} [{}]", closed.join(", "))
        }
        V::Record(s, vs) if strs[*s] == NIL => {
            let items = vs.iter().rev().map(|v| pretty(v, strs)).collect::<Vec<_>>();
            format!("[{}]", items.join(", "))
        }
        V::Record(s, vs) => {
            if vs.len() == 1 {
                match *vs[0] {
                    V::String(v) if strs[v] == NIL => {
                        return format!("{}()", pretty(&V::String(*s), strs));
                    }
                    _ => {}
                }
            }
            let items = vs.iter().map(|v| pretty(v, strs)).collect::<Vec<_>>();
            format!("{}({})", strs[*s].to_string(), items.join(", "))
        }
        V::Resumable(_, _) => {
            format!("resumable")
        }
    }
}
