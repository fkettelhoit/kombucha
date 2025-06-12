// The order mirrors their position in the bytecode header:
pub enum Reflect {
    Nil = 0,
    Value = 1,
    Binding = 2,
    Compound = 3,
    List = 4,
}

pub const NIL: &str = "";
pub const VALUE: &str = "Value";
pub const BINDING: &str = "Binding";
pub const COMPOUND: &str = "Compound";
pub const LIST: &str = "List";

#[derive(Debug, Clone, Default)]
pub struct Bytecode {
    pub ctx: Ctx,
    pub ops: Vec<Op>,
    pub start: usize,
}

#[derive(Debug, Clone)]
pub struct Ctx {
    pub bindings: Vec<String>,
    pub vars: Vec<String>,
    pub strs: Vec<String>,
}

impl Default for Ctx {
    fn default() -> Self {
        let strs = [NIL, VALUE, BINDING, COMPOUND, LIST].map(|s| s.to_string()).to_vec();
        Ctx { bindings: vec![], vars: vec![], strs }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    LoadFn { code: usize, fvars: usize },
    LoadString(usize),
    LoadEffect(usize),
    LoadVar(usize),
    AppFnToArg,
    AppArgToFn,
    Return,
    Cmp,
    Unpack,
    Try,
    Unwind,
}

impl Bytecode {
    pub fn new(ctx: Ctx, ops: Vec<Op>, start: usize) -> Self {
        Bytecode { ctx, ops, start }
    }

    pub fn as_bytes(&self) -> Result<Vec<u8>, String> {
        let mut buf = vec![];
        buf.extend((self.ctx.bindings.len() as u32).to_be_bytes());
        buf.extend((self.ctx.vars.len() as u32).to_be_bytes());
        buf.extend((self.ctx.strs.len() as u32).to_be_bytes());
        buf.extend((self.start as u32).to_be_bytes());
        for section in [&self.ctx.bindings, &self.ctx.vars, &self.ctx.strs] {
            for s in section {
                buf.extend((s.len() as u32).to_be_bytes());
                buf.extend(s.as_bytes());
            }
        }
        fn check_size(op: usize, v: usize, bits: usize) -> Result<(), String> {
            if v >= (1 << bits) {
                return Err(format!("Op {op:6}: Cannot encode {v} in {bits} bits"));
            }
            Ok(())
        }
        for (i, op) in self.ops.iter().copied().enumerate() {
            let tag_shift = 4;
            match op {
                Op::LoadFn { code, fvars } => {
                    check_size(i, code, 20)?;
                    check_size(i, fvars, 8)?;
                    buf.push(1 << tag_shift | ((code >> 16 & 0x0F) as u8));
                    buf.push((code >> 8 & 0xFF) as u8);
                    buf.push((code & 0xFF) as u8);
                    buf.push((fvars & 0xFF) as u8);
                }
                Op::LoadString(str) => {
                    check_size(i, str, 20)?;
                    buf.push(2 << tag_shift | ((str >> 16 & 0x0F) as u8));
                    buf.push((str >> 8 & 0xFF) as u8);
                    buf.push((str & 0xFF) as u8);
                }
                Op::LoadEffect(eff) => {
                    check_size(i, eff, 20)?;
                    buf.push(3 << tag_shift | ((eff >> 16 & 0x0F) as u8));
                    buf.push((eff >> 8 & 0xFF) as u8);
                    buf.push((eff & 0xFF) as u8);
                }
                Op::LoadVar(v) => {
                    check_size(i, v, 12)?;
                    buf.push(4 << tag_shift | ((v >> 8 & 0x0F) as u8));
                    buf.push((v & 0xFF) as u8);
                }
                Op::AppFnToArg => buf.push(5 << tag_shift),
                Op::AppArgToFn => buf.push(6 << tag_shift),
                Op::Return => buf.push(7 << tag_shift),
                Op::Cmp => buf.push(8 << tag_shift),
                Op::Unpack => buf.push(9 << tag_shift),
                Op::Try => buf.push(10 << tag_shift),
                Op::Unwind => buf.push(11 << tag_shift),
            }
        }
        Ok(buf)
    }

    pub fn parse(buf: &[u8]) -> Result<Self, String> {
        let mut i = 0;
        if buf.len() < 16 {
            return Err("Expected the bytecode to contain at least 16 bytes".into());
        }
        let mut ctx = Ctx::default();
        let mut strs = [0u32; 4];
        for str in strs.iter_mut() {
            *str = u32::from_be_bytes([buf[i], buf[i + 1], buf[i + 2], buf[i + 3]]);
            i += 4;
        }
        let [b, v, s, start_op] = strs;
        for (strings, count) in [(&mut ctx.bindings, b), (&mut ctx.vars, v), (&mut ctx.strs, s)] {
            strings.clear();
            for _ in 0..count {
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
                1 => {
                    check_bytes(buf, i, 4, "LoadClosure")?;
                    let code = ((buf[i] & 0x0F) as usize) << 16
                        | (buf[i + 1] as usize) << 8
                        | buf[i + 2] as usize;
                    let fvars = buf[i + 3] as usize;
                    ops.push(Op::LoadFn { code, fvars });
                    i += 3;
                }
                2 => {
                    check_bytes(buf, i, 3, "LoadString")?;
                    let str = ((buf[i] & 0x0F) as usize) << 16
                        | (buf[i + 1] as usize) << 8
                        | buf[i + 2] as usize;
                    ops.push(Op::LoadString(str));
                    i += 2;
                }
                3 => {
                    check_bytes(buf, i, 3, "LoadEffect")?;
                    let eff = ((buf[i] & 0x0F) as usize) << 16
                        | (buf[i + 1] as usize) << 8
                        | buf[i + 2] as usize;
                    ops.push(Op::LoadEffect(eff));
                    i += 2;
                }
                4 => {
                    check_bytes(buf, i, 2, "LoadVar")?;
                    let v = ((buf[i] & 0x0F) as usize) << 8 | buf[i + 1] as usize;
                    ops.push(Op::LoadVar(v));
                    i += 1;
                }
                5 => ops.push(Op::AppFnToArg),
                6 => ops.push(Op::AppArgToFn),
                7 => ops.push(Op::Return),
                8 => ops.push(Op::Cmp),
                9 => ops.push(Op::Unpack),
                10 => ops.push(Op::Try),
                11 => ops.push(Op::Unwind),
                _ => return Err(format!("Invalid opcode {tag} at {i}")),
            }
            i += 1;
        }
        Ok(Bytecode { ctx, ops, start: start_op as usize })
    }

    pub fn pretty(&self) -> String {
        let mut buf = String::new();
        for (i, op) in self.ops.iter().enumerate() {
            match op {
                Op::Return => buf.push_str(&format!("{i:05}:   Return\n")),
                Op::LoadString(s) => match self.ctx.strs.get(*s) {
                    Some(s) => buf.push_str(&format!("{i:05}: PushString(\"{s}\")\n")),
                    None => buf.push_str(&format!("{i:05}: {op:05?}\n")),
                },
                Op::LoadEffect(s) => match self.ctx.strs.get(*s) {
                    Some(s) => buf.push_str(&format!("{i:05}: PushEffect(\"{s}\")\n")),
                    None => buf.push_str(&format!("{i:05}: {op:05?}\n")),
                },
                _ => buf.push_str(&format!("{i:05}: {op:05?}\n")),
            }
        }
        buf
    }
}
