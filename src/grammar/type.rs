use std::rc::Rc;

use super::*;

#[derive(Debug, Clone)]
pub enum SolidType<'a> {
  Ident(TypePath<'a>),
  Tuple(Vec<Type<'a>>),
  Ref(bool, Box<Type<'a>>),
  Array(Box<Type<'a>>, Box<Expr<'a>>),
  Slice(Box<Type<'a>>),

  BareFunc(FnTypeSpec<'a>),

  Never,
}

#[derive(Debug, Clone)]
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
