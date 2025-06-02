use std::borrow::Cow;

pub enum Reflect {
    Nil = 0,
    Value = 1,
    Binding = 2,
    Compound = 3,
    Template = 4,
    Tuple = 5,
}

pub const NIL: &str = "";
pub const VALUE: &str = "Value";
pub const BINDING: &str = "Binding";
pub const COMPOUND: &str = "Compound";
pub const TEMPLATE: &str = "Template";
pub const TUPLE: &str = "Tuple";

#[derive(Debug, Clone, Default)]
pub struct Bytecode {
    pub strings: Vec<Cow<'static, str>>,
    pub ops: Vec<Op>,
    pub start: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    LoadVar(usize),
    LoadString(usize),
    LoadEffect(usize),
    LoadFn { code: usize, fvars: usize },
    ApplyFnToArg,
    ApplyArgToFn,
    Return,
    Cmp,
    Unpack,
    Try,
    Unwind,
}

impl Bytecode {
    pub fn new(strings: Vec<impl Into<Cow<'static, str>>>, ops: Vec<Op>, start: usize) -> Self {
        Bytecode {
            strings: strings.into_iter().map(|s| s.into()).collect(),
            ops,
            start,
        }
    }

    pub fn as_bytes(&self) -> Result<Vec<u8>, String> {
        let mut buf = vec![];
        buf.extend((self.strings.len() as u32).to_be_bytes());
        buf.extend((self.start as u32).to_be_bytes());
        for s in &self.strings {
            buf.extend((s.len() as u32).to_be_bytes());
            buf.extend(s.as_bytes());
        }
        fn check_size(op: usize, v: usize, bits: usize) -> Result<(), String> {
            if v >= (1 << bits) {
                return Err(format!("Op {op:6}: Cannot encode {v} in {bits} bits"));
            }
            Ok(())
        }
        for (i, op) in self.ops.iter().copied().enumerate() {
            match op {
                Op::LoadVar(v) => {
                    check_size(i, v, 12)?;
                    buf.push(0 << 4 | ((v >> 8 & 0x0F) as u8));
                    buf.push((v & 0xFF) as u8);
                }
                Op::LoadString(str) => {
                    check_size(i, str, 20)?;
                    buf.push(1 << 4 | ((str >> 16 & 0x0F) as u8));
                    buf.push((str >> 8 & 0xFF) as u8);
                    buf.push((str & 0xFF) as u8);
                }
                Op::LoadEffect(eff) => {
                    check_size(i, eff, 20)?;
                    buf.push(2 << 4 | ((eff >> 16 & 0x0F) as u8));
                    buf.push((eff >> 8 & 0xFF) as u8);
                    buf.push((eff & 0xFF) as u8);
                }
                Op::LoadFn { code, fvars } => {
                    check_size(i, code, 20)?;
                    check_size(i, fvars, 8)?;
                    buf.push(3 << 4 | ((code >> 16 & 0x0F) as u8));
                    buf.push((code >> 8 & 0xFF) as u8);
                    buf.push((code & 0xFF) as u8);
                    buf.push((fvars & 0xFF) as u8);
                }
                Op::ApplyFnToArg => buf.push(4 << 4),
                Op::ApplyArgToFn => buf.push(5 << 4),
                Op::Return => buf.push(6 << 4),
                Op::Cmp => buf.push(7 << 4),
                Op::Unpack => buf.push(8 << 4),
                Op::Try => buf.push(9 << 4),
                Op::Unwind => buf.push(10 << 4),
            }
        }
        Ok(buf)
    }

    pub fn parse(buf: &[u8]) -> Result<Self, String> {
        let mut i = 0;
        if buf.len() < 8 {
            return Err("Expected the bytecode to contain at least 8 bytes".into());
        }
        let str_count = u32::from_be_bytes([buf[i], buf[i + 1], buf[i + 2], buf[i + 3]]);
        i += 4;
        let start_op = u32::from_be_bytes([buf[i], buf[i + 1], buf[i + 2], buf[i + 3]]);
        i += 4;
        let mut strings = Vec::new();
        for _ in 0..str_count {
            if i + 4 > buf.len() {
                return Err(format!("Expected 4 string length bytes at offset {i}"));
            }
            let len = u32::from_be_bytes([buf[i], buf[i + 1], buf[i + 2], buf[i + 3]]) as usize;
            i += 4;
            if i + len > buf.len() {
                return Err(format!("String at {i} is shorter than length {len}"));
            }
            let string = String::from_utf8(buf[i..i + len].to_vec())
                .map_err(|_| format!("Invalid UTF8 string at {i}"))?;
            strings.push(string.into());
            i += len;
        }
        fn check_bytes(bytes: &[u8], i: usize, needed: usize, op: &str) -> Result<(), String> {
            if i + needed > bytes.len() {
                return Err(format!("{op} at {i} needs {needed} bytes"));
            }
            Ok(())
        }
        let mut ops = Vec::new();
        while i < buf.len() {
            let tag = (buf[i] >> 4) & 0x0F;
            match tag {
                0 => {
                    check_bytes(buf, i, 2, "LoadVar")?;
                    let v = ((buf[i] & 0x0F) as usize) << 8 | buf[i + 1] as usize;
                    ops.push(Op::LoadVar(v));
                    i += 1;
                }
                1 => {
                    check_bytes(buf, i, 3, "LoadString")?;
                    let str = ((buf[i] & 0x0F) as usize) << 16
                        | (buf[i + 1] as usize) << 8
                        | buf[i + 2] as usize;
                    ops.push(Op::LoadString(str));
                    i += 2;
                }
                2 => {
                    check_bytes(buf, i, 3, "LoadEffect")?;
                    let eff = ((buf[i] & 0x0F) as usize) << 16
                        | (buf[i + 1] as usize) << 8
                        | buf[i + 2] as usize;
                    ops.push(Op::LoadEffect(eff));
                    i += 2;
                }
                3 => {
                    check_bytes(buf, i, 4, "LoadFn")?;
                    let code = ((buf[i] & 0x0F) as usize) << 16
                        | (buf[i + 1] as usize) << 8
                        | buf[i + 2] as usize;
                    let fvars = buf[i + 3] as usize;
                    ops.push(Op::LoadFn { code, fvars });
                    i += 3;
                }
                4 => ops.push(Op::ApplyFnToArg),
                5 => ops.push(Op::ApplyArgToFn),
                6 => ops.push(Op::Return),
                7 => ops.push(Op::Cmp),
                8 => ops.push(Op::Unpack),
                9 => ops.push(Op::Try),
                10 => ops.push(Op::Unwind),
                _ => return Err(format!("Invalid opcode {tag} at {i}")),
            }
            i += 1;
        }
        Ok(Bytecode {
            strings,
            ops,
            start: start_op as usize,
        })
    }
}
