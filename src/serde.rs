use std::rc::Rc;

use serde::{Deserialize, Serialize};

use crate::{
    bytecode::{Bytecode, Str},
    run::{Resumable, Val, Value},
};

pub mod de;
pub mod ser;

impl Bytecode {
    pub fn serialize<T: Serialize>(&mut self, value: &T) -> Result<Val, ser::Error> {
        fn json_to_kombucha(bytecode: &mut Bytecode, value: serde_json::Value) -> Val {
            match value {
                serde_json::Value::Null => Val::String(Str::Nil as usize),
                serde_json::Value::Bool(b) => {
                    let s = if b { "True" } else { "False" };
                    Val::String(intern(&mut bytecode.ctx.strs, s.to_string()))
                }
                serde_json::Value::Number(_) => todo!(),
                serde_json::Value::String(s) => {
                    Val::String(intern(&mut bytecode.ctx.strs, format!("\"{s}\"")))
                }
                serde_json::Value::Array(values) => Val::Struct(
                    Str::Nil as usize,
                    values
                        .into_iter()
                        .map(|v| Rc::new(json_to_kombucha(bytecode, v)))
                        .collect::<Vec<_>>(),
                ),
                serde_json::Value::Object(map) => Val::Struct(
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

    pub fn deserialize<'a, T: Deserialize<'a>>(&'a self, v: &'a Val) -> Result<T, de::Error> {
        let mut deserializer = de::Deserializer::new(&self.ctx.strs, v);
        T::deserialize(&mut deserializer)
    }
}

pub(crate) fn intern(strs: &mut Vec<String>, s: String) -> usize {
    strs.iter().position(|x| *x == s).unwrap_or_else(|| {
        strs.push(s);
        strs.len() - 1
    })
}

impl Value {
    pub fn deserialize<'a, T: Deserialize<'a>>(&'a self) -> Result<T, de::Error> {
        self.bytecode.deserialize(&self.val)
    }
}

impl Resumable {
    pub fn serialize<T: Serialize>(&mut self, value: &T) -> Result<Val, ser::Error> {
        self.arg.bytecode.serialize(value)
    }
}
