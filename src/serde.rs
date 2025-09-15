use std::rc::Rc;

use serde::{
    Serialize,
    de::{DeserializeOwned, Error},
};
use serde_json::{Map, Value};

use crate::{
    bytecode::{Bytecode, Str},
    run::{Resumable, Val},
};

impl Bytecode {
    pub fn serialize<T: Serialize>(&mut self, value: &T) -> Result<Val, serde_json::Error> {
        fn json_to_kombucha(bytecode: &mut Bytecode, value: Value) -> Val {
            match value {
                Value::Null => Val::String(Str::Null as usize),
                Value::Bool(b) => {
                    let s = if b { "True" } else { "False" };
                    Val::String(intern(&mut bytecode.ctx.strs, s.to_string()))
                }
                Value::Number(_) => todo!(),
                Value::String(s) => Val::String(intern(&mut bytecode.ctx.strs, format!("\"{s}\""))),
                Value::Array(values) => Val::Struct(
                    Str::List as usize,
                    values
                        .into_iter()
                        .map(|v| Rc::new(json_to_kombucha(bytecode, v)))
                        .collect::<Vec<_>>(),
                ),
                Value::Object(map) => Val::Struct(
                    Str::List as usize,
                    map.into_iter()
                        .map(|(k, v)| {
                            Rc::new(Val::Struct(
                                Str::List as usize,
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

    pub fn deserialize<T: DeserializeOwned>(&self, v: &Val) -> Result<T, serde_json::Error> {
        fn kombucha_to_json(bytecode: &Bytecode, v: &Val) -> Result<Value, serde_json::Error> {
            match v {
                Val::String(s) if *s == Str::Null as usize => Ok(Value::Null),
                Val::String(s) if *s == Str::List as usize => Ok(Value::Array(vec![])),
                Val::String(s) if bytecode.ctx.strs[*s] == "None" => Ok(Value::Null),
                Val::String(s) if bytecode.ctx.strs[*s] == "True" => Ok(true.into()),
                Val::String(s) if bytecode.ctx.strs[*s] == "False" => Ok(false.into()),
                Val::String(s) | Val::Effect(s) => {
                    let s = &bytecode.ctx.strs[*s];
                    if s.starts_with('"') && s.ends_with('"') {
                        Ok(s[1..s.len() - 1].into())
                    } else {
                        Ok(s.as_str().into())
                    }
                }
                Val::Struct(s, vals) if *s == Str::List as usize => {
                    fn is_pair(bytecode: &Bytecode, val: &Val) -> bool {
                        match val {
                            Val::Struct(s, vals) if *s == Str::List as usize && vals.len() == 2 => {
                                match vals.first().unwrap().as_ref() {
                                    Val::String(s) => {
                                        let s = &bytecode.ctx.strs[*s];
                                        s.starts_with('"') && s.ends_with('"')
                                    }
                                    _ => false,
                                }
                            }
                            _ => false,
                        }
                    }
                    let is_map = vals.iter().all(|v| is_pair(bytecode, v.as_ref()));
                    if is_map {
                        let mut map = Map::new();
                        for v in vals {
                            let Val::Struct(_, vals) = v.as_ref() else {
                                unreachable!("this should have been a pair");
                            };
                            let k = &vals[0];
                            let v = &vals[1];
                            let Val::String(k) = k.as_ref() else {
                                unreachable!("this should have been a string key");
                            };
                            let k = &bytecode.ctx.strs[*k];
                            let k = k[1..k.len() - 1].to_string();
                            let v = kombucha_to_json(bytecode, v)?;
                            map.insert(k, v);
                        }
                        Ok(Value::Object(map))
                    } else {
                        Ok(vals
                            .into_iter()
                            .map(|v| kombucha_to_json(bytecode, v))
                            .collect::<Result<Vec<_>, _>>()?
                            .into())
                    }
                }
                Val::Struct(s, vals) if bytecode.ctx.strs[*s] == "Some" && vals.len() == 1 => {
                    kombucha_to_json(bytecode, vals.into_iter().next().unwrap())
                }
                Val::Struct(s, vals) if vals.len() == 1 => {
                    let val = kombucha_to_json(bytecode, vals.into_iter().next().unwrap())?;
                    let mut map = Map::new();
                    map.insert(bytecode.ctx.strs[*s].to_string(), val);
                    Ok(Value::Object(map))
                }
                Val::Struct(s, vals) => {
                    let vals = vals
                        .into_iter()
                        .map(|v| kombucha_to_json(bytecode, v))
                        .collect::<Result<Vec<_>, _>>()?
                        .into();
                    let mut map = Map::new();
                    map.insert(bytecode.ctx.strs[*s].to_string(), vals);
                    Ok(Value::Object(map))
                }
                Val::Closure(_, _) | Val::Resumable(_, _) => {
                    Err(serde_json::Error::custom("Can't deserialize closures or resumables"))
                }
            }
        }
        Ok(serde_json::from_value(kombucha_to_json(self, v)?)?)
    }
}

pub(crate) fn intern(strs: &mut Vec<String>, s: String) -> usize {
    strs.iter().position(|x| *x == s).unwrap_or_else(|| {
        strs.push(s);
        strs.len() - 1
    })
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
