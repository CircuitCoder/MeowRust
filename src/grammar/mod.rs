use std::collections::HashMap;
use std::rc::Rc;

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

#[derive(Debug)]
pub enum PathSeg<'a> {
  Super,
  SelfVal,
  SelfType,
  Ident(&'a str), // TODO: cow
}

#[derive(Default, Debug)]
pub struct GenericArgs<'a> {
  pub types: Vec<Type<'a>>,
  pub bindings: HashMap<&'a str, Type<'a>>,
}

#[derive(Debug)]
pub struct Path<'a, T> {
  pub from_root: bool,
  pub segments: Vec<(PathSeg<'a>, T)>,
}

pub type SimplePath<'a> = Path<'a, ()>;
pub type PathInExpr<'a> = Path<'a, GenericArgs<'a>>;
pub type TypePath<'a> = Path<'a, TypeSegArgs<'a>>;

#[derive(Debug)]
pub struct FnTypeSpec<'a> {
  pub args: Vec<Type<'a>>,
  pub ret: Box<Type<'a>>,
}

#[derive(Debug)]
pub enum TypeSegArgs<'a> {
  NonFnArgs(GenericArgs<'a>),
  FnArgs(FnTypeSpec<'a>),
  None,
}

#[derive(Debug)]
pub enum SolidType<'a> {
  Ident(TypePath<'a>),
  Tuple(Vec<Type<'a>>),
  Ref(bool, Box<Type<'a>>),
  Array(Box<Type<'a>>, usize),
  Slice(Box<Type<'a>>),

  BareFunc(FnTypeSpec<'a>),

  Never,
}

#[derive(Debug)]
pub enum Type<'a> {
  Solid(SolidType<'a>),
  SameAs(Rc<Type<'a>>),
  Placeholder,
}

impl<'a> From<SolidType<'a>> for Type<'a> {
  fn from(f: SolidType<'a>) -> Self {
    Self::Solid(f)
  }
}

pub const UNIT_TYPE: Type = Type::Solid(SolidType::Tuple(Vec::new()));
