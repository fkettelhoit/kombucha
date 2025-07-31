use std::rc::Rc;

use serde::{Serialize, de::DeserializeOwned};
use serde_json::Value;

use crate::{
    bytecode::{Bytecode, Str},
    run::{Resumable, Val},
};

pub mod de;
pub mod ser;

impl Bytecode {
    pub fn serialize<T: Serialize>(&mut self, value: &T) -> Result<Val, ser::Error> {
        fn json_to_kombucha(bytecode: &mut Bytecode, value: Value) -> Val {
            match value {
                Value::Null => Val::String(Str::Nil as usize),
                Value::Bool(b) => {
                    let s = if b { "True" } else { "False" };
                    Val::String(intern(&mut bytecode.ctx.strs, s.to_string()))
                }
                Value::Number(_) => todo!(),
                Value::String(s) => Val::String(intern(&mut bytecode.ctx.strs, format!("\"{s}\""))),
                Value::Array(values) => Val::Struct(
                    Str::Nil as usize,
                    values
                        .into_iter()
                        .map(|v| Rc::new(json_to_kombucha(bytecode, v)))
                        .collect::<Vec<_>>(),
                ),
                Value::Object(map) => Val::Struct(
                    Str::Nil as usize,
                    map.into_iter()
                        .map(|(k, v)| {
                            Rc::new(Val::Struct(
                                Str::Nil as usize,
                                vec![
                                    Rc::new(Val::String(intern(
                                        &mut bytecode.ctx.strs,
                                        format!("\"{k}\""),
                                    ))),
                                    Rc::new(json_to_kombucha(bytecode, v)),
                                ],
                            ))
                        })
                        .collect(),
                ),
            }
        }
        Ok(json_to_kombucha(self, serde_json::to_value(value)?))
    }

    pub fn deserialize<T: DeserializeOwned>(&self, v: &Val) -> Result<T, de::Error> {
        fn kombucha_to_json(bytecode: &Bytecode, v: &Val) -> Value {
            match v {
                Val::String(s) if bytecode.ctx.strs[*s] == "True" => true.into(),
                Val::String(s) if bytecode.ctx.strs[*s] == "False" => false.into(),
                Val::String(s) | Val::Effect(s) => bytecode.ctx.strs[*s].to_string().into(),
                Val::Struct(_, vals) => vals
                    .into_iter()
                    .map(|v| kombucha_to_json(bytecode, v))
                    .collect::<Vec<_>>()
                    .into(),
                Val::Closure(_, _) | Val::Resumable(_, _) => {
                    panic!("Can't deserialize closures or resumables")
                }
            }
        }
        let value = kombucha_to_json(self, v);
        Ok(serde_json::from_value(value)?)
    }
}

pub(crate) fn intern(strs: &mut Vec<String>, s: String) -> usize {
    strs.iter().position(|x| *x == s).unwrap_or_else(|| {
        strs.push(s);
        strs.len() - 1
    })
}

impl crate::run::Value {
    pub fn deserialize<T: DeserializeOwned>(&self) -> Result<T, de::Error> {
        self.bytecode.deserialize(&self.val)
    }
}

impl Resumable {
    pub fn serialize<T: Serialize>(&mut self, value: &T) -> Result<Val, ser::Error> {
        self.arg.bytecode.serialize(value)
    }
}
