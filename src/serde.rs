use std::{borrow::Cow, fmt::Display, rc::Rc};

use serde::{
    Deserialize, Serialize,
    de::{self, IntoDeserializer},
    ser,
};

use crate::{
    bytecode::{Bytecode, NIL, Reflect},
    run::{Resumable, Val, Value, intern, intern_atom, intern_string},
};

#[derive(Debug, Clone)]
pub enum Error {
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

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::NumTypeNotSupported => write!(f, "numeric types are not supported"),
            Error::BytesNotSupported => write!(f, "byte arrays are not supported"),
            Error::InvalidBool => write!(f, "invalid boolean value"),
            Error::InvalidStr => write!(f, "invalid string value"),
            Error::InvalidChar => write!(f, "invalid character value"),
            Error::InvalidUnit => write!(f, "invalid unit value"),
            Error::InvalidSeq => write!(f, "invalid sequence value"),
            Error::InvalidUnitStruct => write!(f, "invalid unit struct"),
            Error::InvalidTupleStruct => write!(f, "invalid tuple struct"),
            Error::InvalidStruct => write!(f, "invalid struct"),
            Error::MapNotSupported => write!(f, "maps are not supported"),
            Error::InvalidEnum => write!(f, "invalid enum value"),
            Error::UnknownVariant => write!(f, "unknown enum variant"),
            Error::InvalidIdentifier => write!(f, "invalid identifier"),
            Error::FoundClosure => write!(f, "cannot deserialize closure"),
            Error::FoundResumable => write!(f, "cannot deserialize resumable"),
            Error::Custom(msg) => write!(f, "{}", msg),
        }
    }
}

impl serde::ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Custom(msg.to_string())
    }
}

impl serde::de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Custom(msg.to_string())
    }
}

type Str = &'static str;
type InternedStrs = Vec<Cow<'static, str>>;

pub struct Serializer<'a> {
    strs: &'a mut InternedStrs,
    name: Str,
    key: Val,
    items: Vec<Rc<Val>>,
}

fn nil() -> Val {
    Val::String(Reflect::Nil as usize)
}

impl<'a> Serializer<'a> {
    fn new(strs: &'a mut InternedStrs) -> Self {
        Self { strs, name: "", key: nil(), items: vec![] }
    }
}

impl Bytecode {
    pub fn serialize<T: Serialize>(&mut self, value: &T) -> Result<Val> {
        let mut serializer = Serializer::new(&mut self.strings);
        value.serialize(&mut serializer)
    }

    pub fn deserialize<'a, T: Deserialize<'a>>(&'a self, value: &'a Val) -> Result<T> {
        let mut deserializer = Deserializer::new(&self.strings, value);
        T::deserialize(&mut deserializer)
    }
}

impl Value {
    pub fn deserialize<'a, T: Deserialize<'a>>(&'a self) -> Result<T> {
        self.bytecode.deserialize(&self.val)
    }
}

impl Resumable {
    pub fn deserialize<'a, T: Deserialize<'a>>(&'a self) -> Result<T> {
        self.bytecode.deserialize(&self.arg)
    }
}

type Result<T> = std::result::Result<T, Error>;

impl<'a> ser::Serializer for &'a mut Serializer<'a> {
    type Ok = Val;

    type Error = Error;

    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    fn serialize_bool(self, v: bool) -> Result<Val> {
        let v = if v { "True" } else { "False" };
        Ok(intern_atom(self.strs, v))
    }

    fn serialize_i8(self, _v: i8) -> Result<Val> {
        Err(Error::NumTypeNotSupported)
    }

    fn serialize_i16(self, _v: i16) -> Result<Val> {
        Err(Error::NumTypeNotSupported)
    }

    fn serialize_i32(self, _v: i32) -> Result<Val> {
        Err(Error::NumTypeNotSupported)
    }

    fn serialize_i64(self, _v: i64) -> Result<Val> {
        Err(Error::NumTypeNotSupported)
    }

    fn serialize_u8(self, _v: u8) -> Result<Val> {
        Err(Error::NumTypeNotSupported)
    }

    fn serialize_u16(self, _v: u16) -> Result<Val> {
        Err(Error::NumTypeNotSupported)
    }

    fn serialize_u32(self, _v: u32) -> Result<Val> {
        Err(Error::NumTypeNotSupported)
    }

    fn serialize_u64(self, _v: u64) -> Result<Val> {
        Err(Error::NumTypeNotSupported)
    }

    fn serialize_f32(self, _v: f32) -> Result<Val> {
        Err(Error::NumTypeNotSupported)
    }

    fn serialize_f64(self, _v: f64) -> Result<Val> {
        Err(Error::NumTypeNotSupported)
    }

    fn serialize_char(self, v: char) -> Result<Val> {
        Ok(intern_string(self.strs, v.to_string()))
    }

    fn serialize_str(self, v: &str) -> Result<Val> {
        Ok(intern_string(self.strs, v))
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Val> {
        Err(Error::BytesNotSupported)
    }

    fn serialize_none(self) -> Result<Val> {
        self.serialize_unit()
    }

    fn serialize_some<T: ?Sized + serde::Serialize>(self, v: &T) -> Result<Val> {
        let some = intern(self.strs, "Some");
        Ok(Val::Record(some, vec![Rc::new(v.serialize(self)?)]))
    }

    fn serialize_unit(self) -> Result<Val> {
        Ok(nil())
    }

    fn serialize_unit_struct(self, _name: Str) -> Result<Val> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(self, _: Str, _: u32, variant: Str) -> Result<Val> {
        Ok(intern_atom(self.strs, variant))
    }

    fn serialize_newtype_struct<T>(self, _name: Str, v: &T) -> Result<Val>
    where
        T: ?Sized + serde::Serialize,
    {
        v.serialize(self)
    }

    fn serialize_newtype_variant<T>(self, _: Str, _: u32, variant: Str, v: &T) -> Result<Val>
    where
        T: ?Sized + serde::Serialize,
    {
        let some = intern(self.strs, variant);
        Ok(Val::Record(some, vec![Rc::new(v.serialize(self)?)]))
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self> {
        Ok(self)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self> {
        Ok(self)
    }

    fn serialize_tuple_struct(self, name: Str, _len: usize) -> Result<Self> {
        self.name = name;
        Ok(self)
    }

    fn serialize_tuple_variant(self, _: Str, _: u32, variant: Str, _len: usize) -> Result<Self> {
        self.name = variant;
        Ok(self)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self> {
        Ok(self)
    }

    fn serialize_struct(self, name: Str, _len: usize) -> Result<Self> {
        self.name = name;
        Ok(self)
    }

    fn serialize_struct_variant(self, _: Str, _: u32, variant: Str, _len: usize) -> Result<Self> {
        self.name = variant;
        Ok(self)
    }
}

impl<'a> ser::SerializeSeq for &'a mut Serializer<'a> {
    type Ok = Val;
    type Error = Error;

    fn serialize_element<T: ?Sized + ser::Serialize>(&mut self, v: &T) -> Result<()> {
        let item = v.serialize(&mut Serializer::new(self.strs))?;
        Ok(self.items.push(Rc::new(item)))
    }

    fn end(self) -> Result<Val> {
        Ok(Val::Record(Reflect::Nil as usize, self.items.drain(..).collect()))
    }
}

impl<'a> ser::SerializeTuple for &'a mut Serializer<'a> {
    type Ok = Val;
    type Error = Error;

    fn serialize_element<T: ?Sized + ser::Serialize>(&mut self, v: &T) -> Result<()> {
        let item = v.serialize(&mut Serializer::new(self.strs))?;
        Ok(self.items.push(Rc::new(item)))
    }

    fn end(self) -> std::result::Result<Self::Ok, Self::Error> {
        Ok(Val::Record(Reflect::Nil as usize, self.items.drain(..).collect()))
    }
}

impl<'a> ser::SerializeTupleStruct for &'a mut Serializer<'a> {
    type Ok = Val;
    type Error = Error;

    fn serialize_field<T: ?Sized + ser::Serialize>(&mut self, v: &T) -> Result<()> {
        let item = v.serialize(&mut Serializer::new(self.strs))?;
        Ok(self.items.push(Rc::new(item)))
    }

    fn end(self) -> std::result::Result<Self::Ok, Self::Error> {
        let name = intern(self.strs, self.name);
        Ok(Val::Record(name, self.items.drain(..).collect()))
    }
}

impl<'a> ser::SerializeTupleVariant for &'a mut Serializer<'a> {
    type Ok = Val;
    type Error = Error;

    fn serialize_field<T: ?Sized + ser::Serialize>(&mut self, v: &T) -> Result<()> {
        let item = v.serialize(&mut Serializer::new(self.strs))?;
        Ok(self.items.push(Rc::new(item)))
    }

    fn end(self) -> std::result::Result<Self::Ok, Self::Error> {
        let name = intern(self.strs, self.name);
        Ok(Val::Record(name, self.items.drain(..).collect()))
    }
}

impl<'a> ser::SerializeMap for &'a mut Serializer<'a> {
    type Ok = Val;
    type Error = Error;

    fn serialize_key<T: ?Sized + ser::Serialize>(&mut self, k: &T) -> Result<()> {
        Ok(self.key = k.serialize(&mut Serializer::new(self.strs))?)
    }

    fn serialize_value<T: ?Sized + ser::Serialize>(&mut self, v: &T) -> Result<()> {
        let k = std::mem::replace(&mut self.key, nil());
        let v = v.serialize(&mut Serializer::new(self.strs))?;
        let item = Val::Record(Reflect::Nil as usize, vec![Rc::new(k), Rc::new(v)]);
        Ok(self.items.push(Rc::new(item)))
    }

    fn end(self) -> std::result::Result<Self::Ok, Self::Error> {
        Ok(Val::Record(Reflect::Nil as usize, self.items.drain(..).collect()))
    }
}

impl<'a> ser::SerializeStruct for &'a mut Serializer<'a> {
    type Ok = Val;
    type Error = Error;

    fn serialize_field<T: ?Sized + ser::Serialize>(&mut self, k: Str, v: &T) -> Result<()> {
        let k = k.serialize(&mut Serializer::new(self.strs))?;
        let v = v.serialize(&mut Serializer::new(self.strs))?;
        let item = Val::Record(Reflect::Nil as usize, vec![Rc::new(k), Rc::new(v)]);
        Ok(self.items.push(Rc::new(item)))
    }

    fn end(self) -> std::result::Result<Self::Ok, Self::Error> {
        let name = intern(self.strs, self.name);
        Ok(Val::Record(name, self.items.drain(..).collect()))
    }
}

impl<'a> ser::SerializeStructVariant for &'a mut Serializer<'a> {
    type Ok = Val;
    type Error = Error;

    fn serialize_field<T: ?Sized + ser::Serialize>(&mut self, k: Str, v: &T) -> Result<()> {
        let k = k.serialize(&mut Serializer::new(self.strs))?;
        let v = v.serialize(&mut Serializer::new(self.strs))?;
        let item = Val::Record(Reflect::Nil as usize, vec![Rc::new(k), Rc::new(v)]);
        Ok(self.items.push(Rc::new(item)))
    }

    fn end(self) -> std::result::Result<Self::Ok, Self::Error> {
        let name = intern(self.strs, self.name);
        Ok(Val::Record(name, self.items.drain(..).collect()))
    }
}

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
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value> {
        match self.current_input() {
            Val::String(s) => {
                let s = self.strs.get(*s).ok_or(Error::InvalidStr)?.as_ref();
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
            Val::Record(_, _items) => self.deserialize_seq(v),
            Val::Closure(_, _) => Err(Error::FoundClosure),
            Val::Resumable(_, _) => Err(Error::FoundResumable),
        }
    }

    fn deserialize_bool<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value> {
        match self.current_input() {
            Val::String(s) => match self.strs.get(*s).map(|s| s.as_ref()) {
                Some("True") => v.visit_bool(true),
                Some("False") => v.visit_bool(false),
                _ => Err(Error::InvalidBool),
            },
            _ => Err(Error::InvalidBool),
        }
    }

    fn deserialize_i8<V: de::Visitor<'de>>(self, _v: V) -> Result<V::Value> {
        Err(Error::NumTypeNotSupported)
    }

    fn deserialize_i16<V: de::Visitor<'de>>(self, _v: V) -> Result<V::Value> {
        Err(Error::NumTypeNotSupported)
    }

    fn deserialize_i32<V: de::Visitor<'de>>(self, _v: V) -> Result<V::Value> {
        Err(Error::NumTypeNotSupported)
    }

    fn deserialize_i64<V: de::Visitor<'de>>(self, _v: V) -> Result<V::Value> {
        Err(Error::NumTypeNotSupported)
    }

    fn deserialize_u8<V: de::Visitor<'de>>(self, _v: V) -> Result<V::Value> {
        Err(Error::NumTypeNotSupported)
    }

    fn deserialize_u16<V: de::Visitor<'de>>(self, _v: V) -> Result<V::Value> {
        Err(Error::NumTypeNotSupported)
    }

    fn deserialize_u32<V: de::Visitor<'de>>(self, _v: V) -> Result<V::Value> {
        Err(Error::NumTypeNotSupported)
    }

    fn deserialize_u64<V: de::Visitor<'de>>(self, _v: V) -> Result<V::Value> {
        Err(Error::NumTypeNotSupported)
    }

    fn deserialize_f32<V: de::Visitor<'de>>(self, _v: V) -> Result<V::Value> {
        Err(Error::NumTypeNotSupported)
    }

    fn deserialize_f64<V: de::Visitor<'de>>(self, _v: V) -> Result<V::Value> {
        Err(Error::NumTypeNotSupported)
    }

    fn deserialize_char<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value> {
        match self.current_input() {
            Val::String(s) => {
                let s = self.strs.get(*s).ok_or(Error::InvalidChar)?;
                let len = s.chars().count();
                if s.starts_with("'") && len == 2 {
                    v.visit_char(s.chars().skip(1).next().ok_or(Error::InvalidChar)?)
                } else if s.starts_with('"') && s.ends_with('"') && len == 3 {
                    v.visit_char(s.chars().skip(1).next().ok_or(Error::InvalidChar)?)
                } else if len == 1 {
                    v.visit_char(s.chars().next().ok_or(Error::InvalidChar)?)
                } else {
                    Err(Error::InvalidChar)
                }
            }
            _ => Err(Error::InvalidChar),
        }
    }

    fn deserialize_str<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value> {
        match self.current_input() {
            Val::String(s) => {
                let s = self.strs.get(*s).ok_or(Error::InvalidStr)?;
                if s.starts_with("'") {
                    v.visit_str(&s[1..])
                } else if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
                    v.visit_str(&s[1..s.len() - 1])
                } else {
                    v.visit_str(s)
                }
            }
            Val::Effect(eff) => v.visit_str(self.strs.get(*eff).ok_or(Error::InvalidStr)?),
            _ => Err(Error::InvalidStr),
        }
    }

    fn deserialize_string<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value> {
        self.deserialize_str(v)
    }

    fn deserialize_bytes<V: de::Visitor<'de>>(self, _v: V) -> Result<V::Value> {
        Err(Error::BytesNotSupported)
    }

    fn deserialize_byte_buf<V: de::Visitor<'de>>(self, _v: V) -> Result<V::Value> {
        Err(Error::BytesNotSupported)
    }

    fn deserialize_option<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value> {
        match self.current_input() {
            Val::String(s) if *s == Reflect::Nil as usize => v.visit_none(),
            _ => v.visit_some(self),
        }
    }

    fn deserialize_unit<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value> {
        match self.current_input() {
            Val::String(s) if *s == Reflect::Nil as usize => v.visit_unit(),
            _ => Err(Error::InvalidUnit),
        }
    }

    fn deserialize_unit_struct<V: de::Visitor<'de>>(self, name: Str, v: V) -> Result<V::Value> {
        match self.current_input() {
            Val::String(s) => {
                let s = self.strs.get(*s).ok_or(Error::InvalidUnitStruct)?;
                if s == name { v.visit_unit() } else { Err(Error::InvalidUnitStruct) }
            }
            _ => Err(Error::InvalidUnitStruct),
        }
    }

    fn deserialize_newtype_struct<V: de::Visitor<'de>>(self, _name: Str, v: V) -> Result<V::Value> {
        v.visit_newtype_struct(self)
    }

    fn deserialize_seq<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value> {
        match self.current_input() {
            Val::Record(s, items) if *s == Reflect::Nil as usize => {
                let seq = SeqDeserializer::new(self, items, true);
                v.visit_seq(seq)
            }
            Val::Record(_, items) => {
                let seq = SeqDeserializer::new(self, items, false);
                v.visit_seq(seq)
            }
            _ => Err(Error::InvalidSeq),
        }
    }

    fn deserialize_tuple<V: de::Visitor<'de>>(self, _len: usize, v: V) -> Result<V::Value> {
        self.deserialize_seq(v)
    }

    fn deserialize_tuple_struct<V: de::Visitor<'de>>(
        self,
        name: Str,
        len: usize,
        v: V,
    ) -> Result<V::Value> {
        match self.current_input() {
            Val::Record(s, items) if items.len() == len => {
                let record_name = self.strs.get(*s).ok_or(Error::InvalidTupleStruct)?;
                if record_name == name {
                    v.visit_seq(SeqDeserializer::new(self, items, false))
                } else {
                    Err(Error::InvalidTupleStruct)
                }
            }
            _ => Err(Error::InvalidTupleStruct),
        }
    }

    fn deserialize_map<V: de::Visitor<'de>>(self, _v: V) -> Result<V::Value> {
        Err(Error::MapNotSupported)
    }

    fn deserialize_struct<V: de::Visitor<'de>>(
        self,
        name: Str,
        fields: &'static [Str],
        v: V,
    ) -> Result<V::Value> {
        match self.current_input() {
            Val::Record(s, items) if items.len() == fields.len() => {
                let record_name = self.strs.get(*s).ok_or(Error::InvalidStruct)?;
                if record_name == name {
                    v.visit_map(StructDeserializer::new(self, items, fields))
                } else {
                    Err(Error::InvalidStruct)
                }
            }
            _ => Err(Error::InvalidStruct),
        }
    }

    fn deserialize_enum<V: de::Visitor<'de>>(
        self,
        _name: Str,
        variants: &'static [Str],
        v: V,
    ) -> Result<V::Value> {
        let enum_de = EnumDeserializer::new(self, variants);
        v.visit_enum(enum_de)
    }

    fn deserialize_identifier<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value> {
        match self.current_input() {
            Val::String(s) => v.visit_str(self.strs.get(*s).ok_or(Error::InvalidIdentifier)?),
            _ => Err(Error::InvalidIdentifier),
        }
    }

    fn deserialize_ignored_any<V: de::Visitor<'de>>(self, v: V) -> Result<V::Value> {
        v.visit_unit()
    }
}

struct SeqDeserializer<'a, 'de> {
    deserializer: &'a mut Deserializer<'de>,
    items: &'de [Rc<Val>],
    index: usize,
    reverse: bool,
}

impl<'a, 'de> SeqDeserializer<'a, 'de> {
    fn new(deserializer: &'a mut Deserializer<'de>, items: &'de [Rc<Val>], reverse: bool) -> Self {
        Self { deserializer, items, index: 0, reverse }
    }
}

impl<'de, 'a> de::SeqAccess<'de> for SeqDeserializer<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        let actual_index = if self.reverse {
            if self.index >= self.items.len() {
                return Ok(None);
            }
            self.items.len() - 1 - self.index
        } else {
            self.index
        };

        match self.items.get(actual_index) {
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
    fields: &'static [Str],
    index: usize,
}

impl<'a, 'de> StructDeserializer<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>, items: &'de [Rc<Val>], fields: &'static [Str]) -> Self {
        Self { de, items, fields, index: 0 }
    }
}

impl<'de, 'a> de::MapAccess<'de> for StructDeserializer<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.index >= self.fields.len() || self.index >= self.items.len() {
            return Ok(None);
        }

        let field_name = self.fields[self.index];
        seed.deserialize(field_name.into_deserializer()).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        if self.index >= self.items.len() {
            return Err(Error::InvalidStruct);
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
    variants: &'static [Str],
}

impl<'a, 'de> EnumDeserializer<'a, 'de> {
    fn new(deserializer: &'a mut Deserializer<'de>, variants: &'static [Str]) -> Self {
        Self { deserializer, variants }
    }
}

impl<'de, 'a> de::EnumAccess<'de> for EnumDeserializer<'a, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant_name = match self.deserializer.current_input() {
            Val::String(s) => self.deserializer.strs.get(*s).ok_or(Error::InvalidEnum)?.as_ref(),
            Val::Record(s, _) => self.deserializer.strs.get(*s).ok_or(Error::InvalidEnum)?.as_ref(),
            _ => return Err(Error::InvalidEnum),
        };
        if !self.variants.contains(&variant_name) {
            return Err(Error::UnknownVariant);
        }
        let variant = seed.deserialize(variant_name.into_deserializer())?;
        Ok((variant, self))
    }
}

impl<'de, 'a> de::VariantAccess<'de> for EnumDeserializer<'a, 'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        match self.deserializer.current_input() {
            Val::String(_) => Ok(()),
            _ => Err(Error::InvalidEnum),
        }
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        match self.deserializer.current_input() {
            Val::Record(_s, items) if items.len() == 1 => {
                let val = &items[0];
                self.deserializer.push_context(val.as_ref());
                let result = seed.deserialize(&mut *self.deserializer);
                self.deserializer.pop_context();
                result
            }
            _ => Err(Error::InvalidEnum),
        }
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.deserializer.current_input() {
            Val::Record(_s, items) if items.len() == len => {
                visitor.visit_seq(SeqDeserializer::new(self.deserializer, items, false))
            }
            _ => Err(Error::InvalidEnum),
        }
    }

    fn struct_variant<V>(self, fields: &'static [Str], visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.deserializer.current_input() {
            Val::Record(_s, items) if items.len() == fields.len() => {
                visitor.visit_map(StructDeserializer::new(self.deserializer, items, fields))
            }
            _ => Err(Error::InvalidEnum),
        }
    }
}
