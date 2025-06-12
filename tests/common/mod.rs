#![allow(dead_code)]

use vorpal::{
    bytecode::NIL,
    run::{Resumable, Val, Value},
};

fn pretty(v: &Val, strs: &Vec<String>) -> String {
    match v {
        Val::String(s) if strs[*s] == NIL => "[]".to_string(),
        Val::String(s) => strs[*s].to_string(),
        Val::Effect(eff) => format!("{}!", strs[*eff]),
        Val::Closure(c, vs) => {
            let closed = vs.iter().map(|v| pretty(v, strs)).collect::<Vec<_>>();
            format!("{c} [{}]", closed.join(", "))
        }
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
        Val::Resumable(_, _) => {
            format!("resumable")
        }
    }
}

pub fn pretty_v(v: &Value) -> String {
    pretty(&v.val, &v.bytecode.ctx.strs)
}

pub fn pretty_res(r: &Resumable) -> String {
    pretty(&r.arg, &r.bytecode.ctx.strs)
}
