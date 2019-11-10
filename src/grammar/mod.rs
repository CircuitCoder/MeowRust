#[derive(Debug)]
pub enum IntSuffix {
  U8,
  U16,
  U32,
  U64,
  U128,
  Usize,

  I8,
  I16,
  I32,
  I64,
  I128,
  Isize,
}

#[derive(Debug)]
pub enum Literal {
  Char(char),
  Int(u128, Option<IntSuffix>),
  Float(f64),
  Str(String),
  Bool(bool),
}
