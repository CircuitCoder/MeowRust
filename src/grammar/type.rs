use super::*;
use super::item::Lifetime;

#[derive(Debug, Clone, PartialEq)]
pub enum SolidType<'a> {
  // Primitives
  Bool,
  Int(IntSuffix),
  Float(FloatSuffix),
  Char,
  Str,

  // Composite types
  Ident(TypePath<'a>),
  Tuple(Vec<Type<'a>>),
  Ref(bool, Option<Lifetime<'a>>, Box<Type<'a>>),
  Array(Box<Type<'a>>, Option<u128>),
  Slice(Box<Type<'a>>),

  BareFunc(FnTypeSpec<'a>),

  Never,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
  Solid(SolidType<'a>),
  Placeholder,
}

#[derive(Debug, Clone, PartialEq)]
enum TypeOrd {

  /**
   * Two types diverge
   * 
   * e.g.: u16 and f32
   */
  Diverge,
  /**
   * Two types doesn't diverge, but has no clearing ordering between them.
   * Merge of the two types yield a type that is greater than both of them
   * 
   * e.g.: (a, _) and (_, b)
   */
  NoOrd,

  /**
   * Two type is equvalent in specificity
   */
  Eq,

  /**
   * The latter is more specific
   */
  Less,

  /**
   * The former is more specific
   */
  Greater,
}

impl TypeOrd {
  fn from_eq(eq: bool) -> TypeOrd {
    if eq { TypeOrd::Eq } else { TypeOrd::Diverge }
  }

  fn join(self, ano: TypeOrd) -> TypeOrd {
    match (self, ano) {
      (TypeOrd::Diverge, _) => TypeOrd::Diverge,
      (_, TypeOrd::Diverge) => TypeOrd::Diverge,

      (TypeOrd::NoOrd, _) => TypeOrd::NoOrd,
      (_, TypeOrd::NoOrd) => TypeOrd::NoOrd,

      (TypeOrd::Eq, ret) => ret,
      (ret, TypeOrd::Eq) => ret,

      (s, a) => {
        if s == a {
          s
        } else {
          TypeOrd::NoOrd
        }
      },
    }
  }
}

impl<'a> From<SolidType<'a>> for Type<'a> {
  fn from(f: SolidType<'a>) -> Self {
    Self::Solid(f)
  }
}

impl<'a> Type<'a> {
  fn merge_cmp(&self, another: &'a Type<'a>) -> (Option<Type<'a>>, TypeOrd) {
    match (self, another) {
      (&Type::Placeholder, _) => {
        let ord = if another == &Type::Placeholder { TypeOrd::Eq } else { TypeOrd::Less };
        (Some(another.clone()), ord)
      },
      (_, &Type::Placeholder) => (Some(self.clone()), TypeOrd::Greater),
      (&Type::Solid(ref s), &Type::Solid(ref a)) => {
        let (s, o) = s.merge_cmp(a);
        (s.map(Into::into), o)
      },
    }
  }
}

impl<'a> SolidType<'a> {
  fn merge_cmp(&self, another: &'a SolidType<'a>) -> (Option<Type<'a>>, TypeOrd) {
    let ret = match self {
      | &SolidType::Bool
      | &SolidType::Int(_)
      | &SolidType::Float(_)
      | &SolidType::Char
      | &SolidType::Str
      | &SolidType::Never
      => (Some(self.clone().into()), TypeOrd::from_eq(another == self)),

      &SolidType::Ident(ref p) => {
        if let &SolidType::Ident(ref anop) = another {
          // TODO: conanicalize path
          // TODO: generic
          (Some(self.clone().into()), TypeOrd::from_eq(p == anop))
        } else {
          (None, TypeOrd::Diverge)
        }
      },
      &SolidType::Tuple(ref elem) => {
        if let &SolidType::Tuple(ref anoelem) = another {
          if elem.len() != anoelem.len() {
            (None, TypeOrd::Diverge)
          } else {
            let mut ordacc = TypeOrd::Eq;
            let mut typacc = Vec::with_capacity(elem.len());

            for (e, ae) in elem.iter().zip(anoelem.iter()) {
              let (t, o) = e.merge_cmp(ae);
              ordacc = ordacc.join(o);
              if ordacc != TypeOrd::Diverge {
                typacc.push(t.unwrap());
              }
            }

            (Some(SolidType::Tuple(typacc).into()), ordacc)
          }
        } else {
          (None, TypeOrd::Diverge)
        }
      },
      &SolidType::Ref(ref is_mut, ref lifetime, ref deref) => {
        if let &SolidType::Ref(ref ano_mut, ref ano_lifetime, ref ano_deref) = another {
          if is_mut != ano_mut {
            (None, TypeOrd::Diverge)
          } else {
            use std::ops::Deref;
            deref.merge_cmp(ano_deref.deref())
          }
        } else {
          (None, TypeOrd::Diverge)
        }
      },

      &SolidType::Array(ref base, ref count) => {
        if let &SolidType::Array(ref ano_base, ref ano_count) = another {
          use std::ops::Deref;
          let (bt, bo) = base.merge_cmp(ano_base.deref());

          if bo == TypeOrd::Diverge { return (None, TypeOrd::Diverge) }
          let bti = bt.unwrap();

          let (ct, co) = match (count.clone(), ano_count.clone()) {
            (None, None) => (None, TypeOrd::Eq),
            (Some(c), None) => (Some(c), TypeOrd::Greater),
            (None, Some(c)) => (Some(c), TypeOrd::Less),
            (Some(ac), Some(bc)) if ac == bc => (Some(ac), TypeOrd::Eq),
            _ => (None, TypeOrd::Diverge),
          };

          (Some(SolidType::Array(Box::new(bti), ct).into()), bo.join(co))
        } else {
          (None, TypeOrd::Diverge)
        }
      },

      &SolidType::Slice(ref base) => {
        if let &SolidType::Slice(ref ano_base) = another {
          use std::ops::Deref;
          base.merge_cmp(ano_base.deref())
        } else {
          (None, TypeOrd::Diverge)
        }
      },

      &SolidType::BareFunc(FnTypeSpec{ ref ret, ref args }) => {
        if let &SolidType::BareFunc(FnTypeSpec{ ret: ref ano_ret, args: ref ano_args }) = another {
          if args.len() != ano_args.len() {
            (None, TypeOrd::Diverge)
          } else {
            use std::ops::Deref;
            let (rt, ro) = ret.merge_cmp(ano_ret.deref());

            if ro == TypeOrd::Diverge {
              return (None, TypeOrd::Diverge);
            }
            
            let rti = rt.unwrap();
            let mut ordacc = ro;
            let mut typacc = Vec::with_capacity(args.len());

            for (e, ae) in args.iter().zip(ano_args.iter()) {
              let (t, o) = e.merge_cmp(ae);
              ordacc = ordacc.join(o);
              if ordacc != TypeOrd::Diverge {
                typacc.push(t.unwrap());
              }
            }

            let t = SolidType::BareFunc(FnTypeSpec {
              ret: Box::new(rti),
              args: typacc,
            });
            (Some(t.into()), ordacc)
          }
        } else {
          (None, TypeOrd::Diverge)
        }
      },
    };

    if ret.1 == TypeOrd::Diverge {
      (None, TypeOrd::Diverge)
    } else {
      ret
    }
  }

  fn is_finished(&self) -> bool {
    match self {
      | &SolidType::Bool
      | &SolidType::Int(_)
      | &SolidType::Float(_)
      | &SolidType::Char
      | &SolidType::Str
      | &SolidType::Never
      => true,
      _ => false,
    }
  }
}

pub const UNIT_TYPE: Type = Type::Solid(SolidType::Tuple(Vec::new()));
