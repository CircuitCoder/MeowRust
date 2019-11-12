use std::collections::HashMap;

mod r#type;

pub use r#type::*;

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
pub enum Pat<'a> {
  Literal(Literal),
  Ident {
    name: &'a str,
    is_ref: bool,
    is_mut: bool,

    bounded: Option<Box<Pat<'a>>>,
  },
}

#[derive(Debug)]
pub enum Stmt<'a> {
  Empty,
  Let(Pat<'a>, Option<Expr<'a>>),
  Expr(Expr<'a>),
}

#[derive(Debug)]
pub enum FlowCtrl<'a> {
  Ret{ ret: Option<Box<Expr<'a>>> },
  Break{ label: Option<&'a str>, ret: Option<Box<Expr<'a>>> },
  Cont{ label: Option<&'a str> },
}

#[derive(Debug)]
pub enum Expr<'a> {
  Literal(Literal),
  Block {
    body: Vec<Stmt<'a>>, 
    ret: Option<Box<Expr<'a>>>
  },
  Tuple(Vec<Expr<'a>>),
  Array(Vec<Expr<'a>>),
  ArrayFill {
    filler: Box<Expr<'a>>,
    count: Box<Expr<'a>>,
  },
  Call {
    recv: Box<Expr<'a>>, 
    method: Option<&'a str>, 
    params: Vec<Expr<'a>>,
  },
  Field {
    owner: Box<Expr<'a>>,
    field: &'a str,
  },
  ArrayIndex {
    owner: Box<Expr<'a>>,
    idx: Box<Expr<'a>>,
  },
  TupleIndex {
    owner: Box<Expr<'a>>,
    idx: u128,
  },

  Closure {
    params: Vec<(Pat<'a>, Type<'a>)>,
    body: Box<Expr<'a>>,
  },

  FlowCtrl(FlowCtrl<'a>),

  Range {
    from: Option<Box<Expr<'a>>>,
    to: Option<Box<Expr<'a>>>,
  },
  Borrow {
    cont: Box<Expr<'a>>,
    is_mut: bool,
  },
  Deref(Box<Expr<'a>>),
  Question(Box<Expr<'a>>),
  Neg(Box<Expr<'a>>),
  Not(Box<Expr<'a>>),

  Cast {
    value: Box<Expr<'a>>,
    to: Type<'a>,
  },

  BinaryOp {
    op: BinaryOp,
    lhs: Box<Expr<'a>>,
    rhs: Box<Expr<'a>>,
  },

  CompoundAssign {
    op: Option<ArithOp>,
    lhs: Box<Expr<'a>>,
    rhs: Box<Expr<'a>>,
  },

  Path(PathInExpr<'a>),
  Loop {
    cond: LoopCond<'a>,
    label: Option<&'a str>,
    body: Box<Expr<'a>>,
  },

  If {
    cond: IfCond<'a>,
    body: Box<Expr<'a>>,
    else_arm: Option<Box<Expr<'a>>>,
  },

  Match {
    from: Box<Expr<'a>>,
    arms: Vec<(MatchArm<'a>, Expr<'a>)>,
  },
}

#[derive(Debug)]
pub enum BinaryOp {
  ArithOp(ArithOp),
  LogicalOp(LogicalOp),
}

#[derive(Debug)]
pub enum ArithOp {
  Add, Sub, Mul, Div, Mod,
  BitAnd, BitOr, BitXor, BitLS, BitRS,
}

impl ArithOp {
  pub fn from(input: &str) -> Self {
    Self::try_from(input).unwrap()
  }

  pub fn from_char(input: char) -> Self {
    // TODO: one less allocation
    Self::from(&input.to_string())
  }

  fn try_from(input: &str) -> Option<Self> {
    match input {
      "+" => Some(Self::Add),
      "-" => Some(Self::Sub),
      "*" => Some(Self::Mul),
      "/" => Some(Self::Div),
      "%" => Some(Self::Mod),

      "&" => Some(Self::BitAnd),
      "|" => Some(Self::BitOr),
      "^" => Some(Self::BitXor),
      "<<" => Some(Self::BitLS),
      ">>" => Some(Self::BitRS),
      _ => None,
    }
  }
}

#[derive(Debug)]
pub enum LogicalOp {
  And, Or,
  Eq, Ne, L, G, Le, Ge,
}

impl LogicalOp {
  pub fn from(input: &str) -> Self {
    Self::try_from(input).unwrap()
  }
  fn try_from(input: &str) -> Option<Self> {
    match input {
      "==" => Some(Self::Eq),
      "!=" => Some(Self::Ne),
      ">" => Some(Self::G),
      "<" => Some(Self::L),
      ">=" => Some(Self::Ge),
      "<=" => Some(Self::Le),

      "&&" => Some(Self::And),
      "||" => Some(Self::Or),
      _ => None,
    }
  }
}

impl Into<BinaryOp> for ArithOp {
  fn into(self) -> BinaryOp {
    BinaryOp::ArithOp(self)
  }
}

impl Into<BinaryOp> for LogicalOp {
  fn into(self) -> BinaryOp {
    BinaryOp::LogicalOp(self)
  }
}

#[derive(Debug)]
pub enum LoopCond<'a> {
  Infty,
  For(Pat<'a>, Box<Expr<'a>>),
  While(Box<Expr<'a>>),
  WhileLet(Vec<Pat<'a>>, Box<Expr<'a>>),
}

#[derive(Debug)]
pub enum IfCond<'a> {
  Let(Pat<'a>, Box<Expr<'a>>),
  Bool(Box<Expr<'a>>)
}

#[derive(Debug)]
pub struct MatchArm<'a> {
  pub pats: Vec<Pat<'a>>,
  pub guard: Option<Box<Expr<'a>>>
}
