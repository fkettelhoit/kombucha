use std::{fmt::Display, rc::Rc};

use serde::de::{self, IntoDeserializer};

use crate::{
    bytecode::{NIL, Str},
    run::{Val, pretty},
};

#[derive(Debug, Clone)]
pub struct Error<'de> {
    cause: E,
    value: Option<Value<'de>>,
}

#[derive(Clone)]
struct Value<'de>(Val, &'de Vec<String>);

impl std::fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&pretty(&self.0, &self.1))
    }
}

impl<'de> Error<'de> {
    fn new(e: E, val: Val, strs: &'de Vec<String>) -> Self {
        Self { cause: e, value: Some(Value(val, strs)) }
    }
}

impl<'de> Display for Error<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Some(Value(v, strs)) => write!(f, "{}: {}", self.cause, pretty(v, strs)),
            None => write!(f, "{}", self.cause),
        }
    }
}

impl<'de> std::error::Error for Error<'de> {}

impl<'de> serde::de::Error for Error<'de> {
    fn custom<T: Display>(msg: T) -> Self {
        Error { cause: E::Custom(msg.to_string()), value: None }
    }
}

#[derive(Debug, Clone)]
pub enum E {
    NumTypeNotSupported,
    BytesNotSupported,
    InvalidBool,
    InvalidStr,
    InvalidChar,
    InvalidUnit,
    InvalidSeq,
    InvalidUnitStruct,
    InvalidTupleStruct,
    InvalidStruct,
    MapNotSupported,
    InvalidEnum,
    UnknownVariant,
    InvalidIdentifier,
    FoundClosure,
    FoundResumable,
    Custom(String),
}

impl std::error::Error for E {}

impl Display for E {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            E::NumTypeNotSupported => write!(f, "Numeric types are not supported"),
            E::BytesNotSupported => write!(f, "Byte arrays are not supported"),
            E::InvalidBool => write!(f, "Invalid boolean value"),
            E::InvalidStr => write!(f, "Invalid string value"),
            E::InvalidChar => write!(f, "Invalid character value"),
            E::InvalidUnit => write!(f, "Invalid unit value"),
            E::InvalidSeq => write!(f, "Invalid sequence value"),
            E::InvalidUnitStruct => write!(f, "Invalid unit struct"),
            E::InvalidTupleStruct => write!(f, "Invalid tuple struct"),
            E::InvalidStruct => write!(f, "Invalid struct"),
            E::MapNotSupported => write!(f, "Maps are not supported"),
            E::InvalidEnum => write!(f, "Invalid enum value"),
            E::UnknownVariant => write!(f, "Unknown enum variant"),
            E::InvalidIdentifier => write!(f, "Invalid identifier"),
            E::FoundClosure => write!(f, "Cannot deserialize closure"),
            E::FoundResumable => write!(f, "Cannot deserialize resumable"),
            E::Custom(msg) => write!(f, "{}", msg),
        }
    }
}

impl serde::de::Error for E {
    fn custom<T: Display>(msg: T) -> Self {
        E::Custom(msg.to_string())
    }
}

type InternedStrs = Vec<String>;

type Result<'de, T> = std::result::Result<T, Error<'de>>;

pub struct Deserializer<'de> {
    strs: &'de InternedStrs,
    context_stack: Vec<&'de Val>,
}

impl<'de> Deserializer<'de> {
    pub fn new(strs: &'de InternedStrs, input: &'de Val) -> Self {
        Self { strs, context_stack: vec![input] }
    }

    fn current_input(&self) -> &'de Val {
        self.context_stack.last().copied().unwrap()
    }

    fn push_context(&mut self, input: &'de Val) {
        self.context_stack.push(input);
    }

    fn pop_context(&mut self) {
        self.context_stack.pop();
    }

    fn err(&self, e: E) -> Error<'de> {
        Error::new(e, self.current_input().clone(), self.strs)
    }
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error<'de>;

    fn deserialize_any<V: de::Visitor<'de>>(self, v: V) -> Result<'de, V::Value> {
        match self.current_input() {
            Val::String(s) => {
                let s = self.strs.get(*s).ok_or_else(|| self.err(E::InvalidStr))?.as_str();
                if s == "True" || s == "False" {
                    self.deserialize_bool(v)
                } else if s.len() == 1 || (s.starts_with('"') && s.ends_with('"') && s.len() == 3) {
                    self.deserialize_char(v)
                } else if s == NIL {
                    self.deserialize_unit(v)
                } else {
                    self.deserialize_str(v)
                }
            }
            Val::Effect(_) => self.deserialize_str(v),
            Val::Struct(_, _items) => self.deserialize_seq(v),
            Val::Closure(_, _) => Err(self.err(E::FoundClosure)),
            Val::Resumable(_, _) => Err(self.err(E::FoundResumable)),
        }
    }

    fn deserialize_bool<V: de::Visitor<'de>>(self, v: V) -> Result<'de, V::Value> {
        match self.current_input() {
            Val::String(s) => match self.strs.get(*s).map(|s| s.as_ref()) {
                Some("True") => v.visit_bool(true),
                Some("False") => v.visit_bool(false),
                _ => Err(self.err(E::InvalidBool)),
            },
            _ => Err(self.err(E::InvalidBool)),
        }
    }

    fn deserialize_i8<V: de::Visitor<'de>>(self, _v: V) -> Result<'de, V::Value> {
        Err(self.err(E::NumTypeNotSupported))
    }

    fn deserialize_i16<V: de::Visitor<'de>>(self, _v: V) -> Result<'de, V::Value> {
        Err(self.err(E::NumTypeNotSupported))
    }

    fn deserialize_i32<V: de::Visitor<'de>>(self, _v: V) -> Result<'de, V::Value> {
        Err(self.err(E::NumTypeNotSupported))
    }

    fn deserialize_i64<V: de::Visitor<'de>>(self, _v: V) -> Result<'de, V::Value> {
        Err(self.err(E::NumTypeNotSupported))
    }

    fn deserialize_u8<V: de::Visitor<'de>>(self, _v: V) -> Result<'de, V::Value> {
        Err(self.err(E::NumTypeNotSupported))
    }

    fn deserialize_u16<V: de::Visitor<'de>>(self, _v: V) -> Result<'de, V::Value> {
        Err(self.err(E::NumTypeNotSupported))
    }

    fn deserialize_u32<V: de::Visitor<'de>>(self, _v: V) -> Result<'de, V::Value> {
        Err(self.err(E::NumTypeNotSupported))
    }

    fn deserialize_u64<V: de::Visitor<'de>>(self, _v: V) -> Result<'de, V::Value> {
        Err(self.err(E::NumTypeNotSupported))
    }

    fn deserialize_f32<V: de::Visitor<'de>>(self, _v: V) -> Result<'de, V::Value> {
        Err(self.err(E::NumTypeNotSupported))
    }

    fn deserialize_f64<V: de::Visitor<'de>>(self, _v: V) -> Result<'de, V::Value> {
        Err(self.err(E::NumTypeNotSupported))
    }

    fn deserialize_char<V: de::Visitor<'de>>(self, v: V) -> Result<'de, V::Value> {
        match self.current_input() {
            Val::String(s) => {
                let s = self.strs.get(*s).ok_or_else(|| self.err(E::InvalidChar))?;
                let len = s.chars().count();
                if s.starts_with("'") && len == 2 {
                    v.visit_char(s.chars().skip(1).next().ok_or_else(|| self.err(E::InvalidChar))?)
                } else if s.starts_with('"') && s.ends_with('"') && len == 3 {
                    v.visit_char(s.chars().skip(1).next().ok_or_else(|| self.err(E::InvalidChar))?)
                } else if len == 1 {
                    v.visit_char(s.chars().next().ok_or_else(|| self.err(E::InvalidChar))?)
                } else {
                    Err(self.err(E::InvalidChar))
                }
            }
            _ => Err(self.err(E::InvalidChar)),
        }
    }

    fn deserialize_str<V: de::Visitor<'de>>(self, v: V) -> Result<'de, V::Value> {
        match self.current_input() {
            Val::String(s) => {
                let s = self.strs.get(*s).ok_or_else(|| self.err(E::InvalidStr))?;
                if s.starts_with("'") {
                    v.visit_str(&s[1..])
                } else if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
                    v.visit_str(&s[1..s.len() - 1])
                } else {
                    v.visit_str(s)
                }
            }
            Val::Effect(eff) => {
                v.visit_str(self.strs.get(*eff).ok_or_else(|| self.err(E::InvalidStr))?)
            }
            _ => Err(self.err(E::InvalidStr)),
        }
    }

    fn deserialize_string<V: de::Visitor<'de>>(self, v: V) -> Result<'de, V::Value> {
        self.deserialize_str(v)
    }

    fn deserialize_bytes<V: de::Visitor<'de>>(self, _v: V) -> Result<'de, V::Value> {
        Err(self.err(E::BytesNotSupported))
    }

    fn deserialize_byte_buf<V: de::Visitor<'de>>(self, _v: V) -> Result<'de, V::Value> {
        Err(self.err(E::BytesNotSupported))
    }

    fn deserialize_option<V: de::Visitor<'de>>(self, v: V) -> Result<'de, V::Value> {
        match self.current_input() {
            Val::String(s) if *s == Str::Nil as usize => v.visit_none(),
            _ => v.visit_some(self),
        }
    }

    fn deserialize_unit<V: de::Visitor<'de>>(self, v: V) -> Result<'de, V::Value> {
        match self.current_input() {
            Val::String(s) if *s == Str::Nil as usize => v.visit_unit(),
            _ => Err(self.err(E::InvalidUnit)),
        }
    }

    fn deserialize_unit_struct<V: de::Visitor<'de>>(
        self,
        name: &'static str,
        v: V,
    ) -> Result<'de, V::Value> {
        match self.current_input() {
            Val::String(s) => {
                let s = self.strs.get(*s).ok_or_else(|| self.err(E::InvalidUnitStruct))?;
                if s == name { v.visit_unit() } else { Err(self.err(E::InvalidUnitStruct)) }
            }
            _ => Err(self.err(E::InvalidUnitStruct)),
        }
    }

    fn deserialize_newtype_struct<V: de::Visitor<'de>>(
        self,
        _name: &'static str,
        v: V,
    ) -> Result<'de, V::Value> {
        v.visit_newtype_struct(self)
    }

    fn deserialize_seq<V: de::Visitor<'de>>(self, v: V) -> Result<'de, V::Value> {
        match self.current_input() {
            Val::Struct(_, items) => {
                let seq = SeqDeserializer::new(self, items);
                v.visit_seq(seq)
            }
            _ => Err(self.err(E::InvalidSeq)),
        }
    }

    fn deserialize_tuple<V: de::Visitor<'de>>(self, _len: usize, v: V) -> Result<'de, V::Value> {
        self.deserialize_seq(v)
    }

    fn deserialize_tuple_struct<V: de::Visitor<'de>>(
        self,
        name: &'static str,
        len: usize,
        v: V,
    ) -> Result<'de, V::Value> {
        match self.current_input() {
            Val::Struct(s, items) if items.len() == len => {
                let record_name =
                    self.strs.get(*s).ok_or_else(|| self.err(E::InvalidTupleStruct))?;
                if record_name == name {
                    v.visit_seq(SeqDeserializer::new(self, items))
                } else {
                    Err(self.err(E::InvalidTupleStruct))
                }
            }
            _ => Err(self.err(E::InvalidTupleStruct)),
        }
    }

    fn deserialize_map<V: de::Visitor<'de>>(self, _v: V) -> Result<'de, V::Value> {
        Err(self.err(E::MapNotSupported))
    }

    fn deserialize_struct<V: de::Visitor<'de>>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        v: V,
    ) -> Result<'de, V::Value> {
        match self.current_input() {
            Val::Struct(s, items) if items.len() == fields.len() => {
                let record_name = self.strs.get(*s).ok_or_else(|| self.err(E::InvalidStruct))?;
                if record_name == name {
                    v.visit_map(StructDeserializer::new(self, items, fields))
                } else {
                    Err(self.err(E::InvalidStruct))
                }
            }
            _ => Err(self.err(E::InvalidStruct)),
        }
    }

    fn deserialize_enum<V: de::Visitor<'de>>(
        self,
        _name: &'static str,
        variants: &'static [&'static str],
        v: V,
    ) -> Result<'de, V::Value> {
        let enum_de = EnumDeserializer::new(self, variants);
        v.visit_enum(enum_de)
    }

    fn deserialize_identifier<V: de::Visitor<'de>>(self, v: V) -> Result<'de, V::Value> {
        match self.current_input() {
            Val::String(s) => {
                v.visit_str(self.strs.get(*s).ok_or_else(|| self.err(E::InvalidIdentifier))?)
            }
            _ => Err(self.err(E::InvalidIdentifier)),
        }
    }

    fn deserialize_ignored_any<V: de::Visitor<'de>>(self, v: V) -> Result<'de, V::Value> {
        v.visit_unit()
    }
}

struct SeqDeserializer<'a, 'de> {
    deserializer: &'a mut Deserializer<'de>,
    items: &'de [Rc<Val>],
    index: usize,
}

impl<'a, 'de> SeqDeserializer<'a, 'de> {
    fn new(deserializer: &'a mut Deserializer<'de>, items: &'de [Rc<Val>]) -> Self {
        Self { deserializer, items, index: 0 }
    }
}

impl<'de, 'a> de::SeqAccess<'de> for SeqDeserializer<'a, 'de> {
    type Error = Error<'de>;

    fn next_element_seed<T>(&mut self, seed: T) -> std::result::Result<Option<T::Value>, Error<'de>>
    where
        T: de::DeserializeSeed<'de>,
    {
        match self.items.get(self.index) {
            None => Ok(None),
            Some(val) => {
                self.index += 1;
                self.deserializer.push_context(val.as_ref());
                let result = seed.deserialize(&mut *self.deserializer);
                self.deserializer.pop_context();
                result.map(Some)
            }
        }
    }
}

struct StructDeserializer<'a, 'de> {
    de: &'a mut Deserializer<'de>,
    items: &'de [Rc<Val>],
    fields: &'static [&'static str],
    index: usize,
}

impl<'a, 'de> StructDeserializer<'a, 'de> {
    fn new(
        de: &'a mut Deserializer<'de>,
        items: &'de [Rc<Val>],
        fields: &'static [&'static str],
    ) -> Self {
        Self { de, items, fields, index: 0 }
    }
}

impl<'de, 'a> de::MapAccess<'de> for StructDeserializer<'a, 'de> {
    type Error = Error<'de>;

    fn next_key_seed<K>(&mut self, seed: K) -> std::result::Result<Option<K::Value>, Error<'de>>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.index >= self.fields.len() || self.index >= self.items.len() {
            return Ok(None);
        }

        let field_name = self.fields[self.index];
        seed.deserialize(field_name.into_deserializer()).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> std::result::Result<V::Value, Error<'de>>
    where
        V: de::DeserializeSeed<'de>,
    {
        if self.index >= self.items.len() {
            return Err(self.de.err(E::InvalidStruct));
        }

        let val = &self.items[self.index];
        self.index += 1;

        self.de.push_context(val.as_ref());
        let result = seed.deserialize(&mut *self.de);
        self.de.pop_context();
        result
    }
}

struct EnumDeserializer<'a, 'de> {
    deserializer: &'a mut Deserializer<'de>,
    variants: &'static [&'static str],
}

impl<'a, 'de> EnumDeserializer<'a, 'de> {
    fn new(deserializer: &'a mut Deserializer<'de>, variants: &'static [&'static str]) -> Self {
        Self { deserializer, variants }
    }
}

impl<'de, 'a> de::EnumAccess<'de> for EnumDeserializer<'a, 'de> {
    type Error = Error<'de>;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> std::result::Result<(V::Value, Self::Variant), Error<'de>>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant_name = match self.deserializer.current_input() {
            Val::String(s) => self
                .deserializer
                .strs
                .get(*s)
                .ok_or_else(|| self.deserializer.err(E::InvalidEnum))?
                .as_ref(),
            Val::Struct(s, _) => self
                .deserializer
                .strs
                .get(*s)
                .ok_or_else(|| self.deserializer.err(E::InvalidEnum))?
                .as_ref(),
            _ => return Err(self.deserializer.err(E::InvalidEnum)),
        };
        if !self.variants.contains(&variant_name) {
            return Err(self.deserializer.err(E::UnknownVariant));
        }
        let variant = seed.deserialize(variant_name.into_deserializer())?;
        Ok((variant, self))
    }
}

impl<'de, 'a> de::VariantAccess<'de> for EnumDeserializer<'a, 'de> {
    type Error = Error<'de>;

    fn unit_variant(self) -> std::result::Result<(), Error<'de>> {
        match self.deserializer.current_input() {
            Val::String(_) => Ok(()),
            _ => Err(self.deserializer.err(E::InvalidEnum)),
        }
    }

    fn newtype_variant_seed<T>(self, seed: T) -> std::result::Result<T::Value, Error<'de>>
    where
        T: de::DeserializeSeed<'de>,
    {
        match self.deserializer.current_input() {
            Val::Struct(_s, items) if items.len() == 1 => {
                let val = &items[0];
                self.deserializer.push_context(val.as_ref());
                let result = seed.deserialize(&mut *self.deserializer);
                self.deserializer.pop_context();
                result
            }
            _ => Err(self.deserializer.err(E::InvalidEnum)),
        }
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> std::result::Result<V::Value, Error<'de>>
    where
        V: de::Visitor<'de>,
    {
        match self.deserializer.current_input() {
            Val::Struct(_s, items) if items.len() == len => {
                visitor.visit_seq(SeqDeserializer::new(self.deserializer, items))
            }
            _ => Err(self.deserializer.err(E::InvalidEnum)),
        }
    }

    fn struct_variant<V>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> std::result::Result<V::Value, Error<'de>>
    where
        V: de::Visitor<'de>,
    {
        match self.deserializer.current_input() {
            Val::Struct(_s, items) if items.len() == fields.len() => {
                visitor.visit_map(StructDeserializer::new(self.deserializer, items, fields))
            }
            _ => Err(self.deserializer.err(E::InvalidEnum)),
        }
    }
}
