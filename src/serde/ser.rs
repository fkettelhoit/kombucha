use std::{fmt::Display, rc::Rc};

use serde::{Serialize, ser};

use crate::{
    bytecode::Str,
    run::{Val, intern, intern_atom, intern_string},
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
            Error::NumTypeNotSupported => write!(f, "Numeric types are not supported"),
            Error::BytesNotSupported => write!(f, "Byte arrays are not supported"),
            Error::InvalidBool => write!(f, "Invalid boolean value"),
            Error::InvalidStr => write!(f, "Invalid string value"),
            Error::InvalidChar => write!(f, "Invalid character value"),
            Error::InvalidUnit => write!(f, "Invalid unit value"),
            Error::InvalidSeq => write!(f, "Invalid sequence value"),
            Error::InvalidUnitStruct => write!(f, "Invalid unit struct"),
            Error::InvalidTupleStruct => write!(f, "Invalid tuple struct"),
            Error::InvalidStruct => write!(f, "Invalid struct"),
            Error::MapNotSupported => write!(f, "Maps are not supported"),
            Error::InvalidEnum => write!(f, "Invalid enum value"),
            Error::UnknownVariant => write!(f, "Unknown enum variant"),
            Error::InvalidIdentifier => write!(f, "Invalid identifier"),
            Error::FoundClosure => write!(f, "Cannot deserialize closure"),
            Error::FoundResumable => write!(f, "Cannot deserialize resumable"),
            Error::Custom(msg) => write!(f, "{}", msg),
        }
    }
}

impl serde::ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Custom(msg.to_string())
    }
}

type InternedStrs = Vec<String>;

pub(crate) struct Serializer<'a> {
    strs: &'a mut InternedStrs,
    name: &'static str,
    key: Val,
    items: Vec<Rc<Val>>,
}

fn nil() -> Val {
    Val::String(Str::Nil as usize)
}

impl<'a> Serializer<'a> {
    pub(crate) fn new(strs: &'a mut InternedStrs) -> Self {
        Self { strs, name: "", key: nil(), items: vec![] }
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
        Ok(intern_atom(self.strs, v.to_string()))
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
        let none = intern(self.strs, "None".to_string());
        Ok(Val::String(none))
    }

    fn serialize_some<T: ?Sized + serde::Serialize>(self, v: &T) -> Result<Val> {
        let some = intern(self.strs, "Some".to_string());
        Ok(Val::Struct(some, vec![Rc::new(v.serialize(self)?)]))
    }

    fn serialize_unit(self) -> Result<Val> {
        Ok(nil())
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Val> {
        Ok(intern_atom(self.strs, name.to_string()))
    }

    fn serialize_unit_variant(self, _: &'static str, _: u32, variant: &'static str) -> Result<Val> {
        Ok(intern_atom(self.strs, variant.to_string()))
    }

    fn serialize_newtype_struct<T>(self, name: &'static str, v: &T) -> Result<Val>
    where
        T: ?Sized + serde::Serialize,
    {
        let some = intern(self.strs, name.to_string());
        Ok(Val::Struct(some, vec![Rc::new(v.serialize(self)?)]))
    }

    fn serialize_newtype_variant<T>(
        self,
        _: &'static str,
        _: u32,
        variant: &'static str,
        v: &T,
    ) -> Result<Val>
    where
        T: ?Sized + serde::Serialize,
    {
        let some = intern(self.strs, variant.to_string());
        Ok(Val::Struct(some, vec![Rc::new(v.serialize(self)?)]))
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self> {
        Ok(self)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self> {
        Ok(self)
    }

    fn serialize_tuple_struct(self, name: &'static str, _len: usize) -> Result<Self> {
        self.name = name;
        Ok(self)
    }

    fn serialize_tuple_variant(
        self,
        _: &'static str,
        _: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self> {
        self.name = variant;
        Ok(self)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self> {
        Ok(self)
    }

    fn serialize_struct(self, name: &'static str, _len: usize) -> Result<Self> {
        self.name = name;
        Ok(self)
    }

    fn serialize_struct_variant(
        self,
        _: &'static str,
        _: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self> {
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
        Ok(Val::Struct(Str::Nil as usize, self.items.drain(..).collect()))
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
        Ok(Val::Struct(Str::Nil as usize, self.items.drain(..).collect()))
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
        let name = intern(self.strs, self.name.to_string());
        Ok(Val::Struct(name, self.items.drain(..).collect()))
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
        let name = intern(self.strs, self.name.to_string());
        Ok(Val::Struct(name, self.items.drain(..).collect()))
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
        let item = Val::Struct(Str::Nil as usize, vec![Rc::new(k), Rc::new(v)]);
        Ok(self.items.push(Rc::new(item)))
    }

    fn end(self) -> std::result::Result<Self::Ok, Self::Error> {
        Ok(Val::Struct(Str::Nil as usize, self.items.drain(..).collect()))
    }
}

impl<'a> ser::SerializeStruct for &'a mut Serializer<'a> {
    type Ok = Val;
    type Error = Error;

    fn serialize_field<T: ?Sized + ser::Serialize>(
        &mut self,
        k: &'static str,
        v: &T,
    ) -> Result<()> {
        let k = k.serialize(&mut Serializer::new(self.strs))?;
        let v = v.serialize(&mut Serializer::new(self.strs))?;
        let item = Val::Struct(Str::Nil as usize, vec![Rc::new(k), Rc::new(v)]);
        Ok(self.items.push(Rc::new(item)))
    }

    fn end(self) -> std::result::Result<Self::Ok, Self::Error> {
        let name = intern(self.strs, self.name.to_string());
        let fields = Val::Struct(Str::Nil as usize, self.items.drain(..).collect());
        Ok(Val::Struct(name, vec![Rc::new(fields)]))
    }
}

impl<'a> ser::SerializeStructVariant for &'a mut Serializer<'a> {
    type Ok = Val;
    type Error = Error;

    fn serialize_field<T: ?Sized + ser::Serialize>(
        &mut self,
        k: &'static str,
        v: &T,
    ) -> Result<()> {
        let k = k.serialize(&mut Serializer::new(self.strs))?;
        let v = v.serialize(&mut Serializer::new(self.strs))?;
        let item = Val::Struct(Str::Nil as usize, vec![Rc::new(k), Rc::new(v)]);
        Ok(self.items.push(Rc::new(item)))
    }

    fn end(self) -> std::result::Result<Self::Ok, Self::Error> {
        let name = intern(self.strs, self.name.to_string());
        let fields = Val::Struct(Str::Nil as usize, self.items.drain(..).collect());
        Ok(Val::Struct(name, vec![Rc::new(fields)]))
    }
}
