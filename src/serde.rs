use std::rc::Rc;

use serde::{
    Serialize,
    de::{DeserializeOwned, Error},
};
use serde_json::{Map, Value};

use crate::{
    bytecode::{Bytecode, Str},
    run::{List, Resumable, Val},
};

impl Bytecode {
    pub fn serialize<T: Serialize>(&mut self, value: &T) -> Result<Val, serde_json::Error> {
        fn json_to_kb(bc: &mut Bytecode, value: Value) -> Val {
            match value {
                Value::Null => Val::String(Str::Null as usize),
                Value::Bool(b) => Val::String(intern(bc, if b { "True" } else { "False" })),
                Value::Number(_) => todo!(),
                Value::String(s) => Val::String(intern(bc, format!("\"{s}\""))),
                Value::Array(values) => list(values.into_iter().map(|v| json_to_kb(bc, v))),
                Value::Object(map) => list(map.into_iter().map(|(k, v)| {
                    Val::Struct(
                        Str::List as usize,
                        Rc::new(List::Cons(
                            Rc::new(List::Val(Val::String(intern(bc, format!("\"{k}\""))))),
                            json_to_kb(bc, v),
                        )),
                    )
                })),
            }
        }
        Ok(json_to_kb(self, serde_json::to_value(value)?))
    }

    pub fn deserialize<T: DeserializeOwned>(&self, v: &Val) -> Result<T, serde_json::Error> {
        fn kb_to_json(bc: &Bytecode, v: &Val) -> Result<Value, serde_json::Error> {
            match v {
                Val::String(s) if *s == Str::Null as usize => Ok(Value::Null),
                Val::String(s) if *s == Str::List as usize => Ok(Value::Array(vec![])),
                Val::String(s) if bc.ctx.strs[*s] == "None" => Ok(Value::Null),
                Val::String(s) if bc.ctx.strs[*s] == "True" => Ok(true.into()),
                Val::String(s) if bc.ctx.strs[*s] == "False" => Ok(false.into()),
                Val::String(s) | Val::Effect(s) => {
                    let s = &bc.ctx.strs[*s];
                    if s.starts_with('"') && s.ends_with('"') {
                        Ok(s[1..s.len() - 1].into())
                    } else {
                        Ok(s.as_str().into())
                    }
                }
                Val::Struct(s, vals) if *s == Str::List as usize => {
                    fn as_pair<'a>(bc: &'a Bytecode, val: &'a Val) -> Option<(String, &'a Val)> {
                        if let Val::Struct(s, vals) = val {
                            if *s == Str::List as usize {
                                if let List::Cons(vals, v) = vals.as_ref() {
                                    if let List::Val(Val::String(k)) = vals.as_ref() {
                                        let k = &bc.ctx.strs[*k];
                                        if k.starts_with('"') && k.ends_with('"') {
                                            let k = k[1..k.len() - 1].to_string();
                                            return Some((k, v));
                                        }
                                    }
                                }
                            }
                        }
                        None
                    }
                    let vals = vals.to_vec();
                    let mut map = Map::new();
                    for val in vals.iter() {
                        if let Some((k, v)) = as_pair(bc, val) {
                            map.insert(k, kb_to_json(bc, v)?);
                        } else {
                            return Ok(vals
                                .into_iter()
                                .map(|v| kb_to_json(bc, v))
                                .collect::<Result<Vec<_>, _>>()?
                                .into());
                        }
                    }
                    Ok(Value::Object(map))
                }
                Val::Struct(s, vals) => match vals.as_ref() {
                    List::Val(val) if bc.ctx.strs[*s] == "Some" => kb_to_json(bc, val),
                    List::Val(val) => {
                        let mut map = Map::new();
                        map.insert(bc.ctx.strs[*s].to_string(), kb_to_json(bc, val)?);
                        Ok(Value::Object(map))
                    }
                    vals => {
                        let vals = vals
                            .to_vec()
                            .into_iter()
                            .map(|v| kb_to_json(bc, v))
                            .collect::<Result<Vec<_>, _>>()?
                            .into();
                        let mut map = Map::new();
                        map.insert(bc.ctx.strs[*s].to_string(), vals);
                        Ok(Value::Object(map))
                    }
                },
                Val::Closure(_, _) | Val::Resumable(_, _) => {
                    Err(serde_json::Error::custom("Can't deserialize closures or resumables"))
                }
            }
        }
        Ok(serde_json::from_value(kb_to_json(self, v)?)?)
    }
}

fn intern(bc: &mut Bytecode, s: impl Into<String>) -> usize {
    let s = s.into();
    bc.ctx.strs.iter().position(|x| *x == s).unwrap_or_else(|| {
        bc.ctx.strs.push(s);
        bc.ctx.strs.len() - 1
    })
}

fn list(mut vals: impl Iterator<Item = Val>) -> Val {
    match vals.next() {
        None => Val::String(Str::List as usize),
        Some(v) => {
            let mut list = List::Val(v);
            while let Some(v) = vals.next() {
                list = List::Cons(Rc::new(list), v);
            }
            Val::Struct(Str::List as usize, Rc::new(list))
        }
    }
}

impl crate::run::Value {
    pub fn deserialize<T: DeserializeOwned>(&self) -> Result<T, serde_json::Error> {
        self.bytecode.deserialize(&self.val)
    }
}

impl Resumable {
    pub fn serialize<T: Serialize>(&mut self, value: &T) -> Result<Val, serde_json::Error> {
        self.arg.bytecode.serialize(value)
    }
}
