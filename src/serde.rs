use serde::{Deserialize, Serialize};

use crate::{
    bytecode::Bytecode,
    run::{Resumable, Val, Value},
};

mod de;
mod ser;

impl Bytecode {
    pub fn serialize<T: Serialize>(&mut self, value: &T) -> Result<Val, ser::Error> {
        let mut serializer = ser::Serializer::new(&mut self.ctx.strs);
        value.serialize(&mut serializer)
    }

    pub fn deserialize<'a, T: Deserialize<'a>>(&'a self, v: &'a Val) -> Result<T, de::Error<'a>> {
        let mut deserializer = de::Deserializer::new(&self.ctx.strs, v);
        T::deserialize(&mut deserializer)
    }
}

impl Value {
    pub fn deserialize<'a, T: Deserialize<'a>>(&'a self) -> Result<T, de::Error<'a>> {
        self.bytecode.deserialize(&self.val)
    }
}

impl Resumable {
    pub fn serialize<T: Serialize>(&mut self, value: &T) -> Result<Val, ser::Error> {
        self.arg.bytecode.serialize(value)
    }
}
