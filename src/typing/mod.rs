use im_rc::hashmap::HashMap;
use im_rc::hashset::HashSet;
use std::collections::VecDeque;

use crate::grammar::*;
use crate::grammar::item::*;
use crate::registry::*;

type TypableWalkThrough<'a> = (Type<'a>, Vec<&'a dyn Typable<'a>>);

trait Typable<'a> {
  fn walk_through(&'a self, ctx: TypingCtx<'a>, typer: &mut Typer<'a>) -> TypableWalkThrough<'a>;
  fn update(&'a self, hint: Type<'a>, ctx: TypingCtx<'a>, typer: &mut Typer<'a>) -> Type<'a>;
  // fn probe(&self) -> Vec<&'a dyn Typable<'a>>;
}

// Scoped variable/functions/...
#[derive(Clone)]
pub enum Scoped<'a> {
  Binding {
    is_mut: bool,
    from: &'a Pat<'a>,
  },
}

#[derive(Default, Clone)]
struct TypingCtx<'a> {
  labels: HashMap<&'a str, &'a Expr<'a>>,
  func: Option<&'a Func<'a>>,
  last_loop: Option<&'a Expr<'a>>,

  reg: HashMap<&'a str, RegistryNode<'a>>,
  scope: HashMap<&'a str, Scoped<'a>>
}

type TypableId<'a> = *const (dyn Typable<'a> + 'a);

struct Typer<'a> {
  resolved: HashMap<TypableId<'a>, Type<'a>>,
  ctxs: HashMap<TypableId<'a>, TypingCtx<'a>>,

  notify_map: HashMap<TypableId<'a>, HashSet<&'a Typable<'a>>>,

  queue: VecDeque<TypableId<'a>>,
  nontrivial_init: HashSet<TypableId<'a>>,
}

impl<'a> Typer<'a> {
  pub fn new() -> Self {
    Typer::<'a> {
      resolved: HashMap::new(),
      ctxs: HashMap::new(),
      notify_map: HashMap::new(),
      queue: VecDeque::new(),
      nontrivial_init: HashSet::new(),
    }
  }

  pub fn walk_through_child(&mut self, root: &'a Typable<'a>, ctx: TypingCtx<'a>) {
    self.ctxs.insert(root, ctx.clone());

    let (init_type, notifiers) = root.walk_through(ctx, self);

    if init_type != Type::Placeholder {
      self.nontrivial_init.insert(root);
    }

    self.resolved.insert(root, init_type);
  }

  pub fn update_hint(&mut self, target: &'a mut Typable<'a>, hint: Type<'a>) {
  }
}

impl<'a> Typable<'a> for Literal {
  fn resolve(&'a self, hint: Option<Type<'a>>, _: TypingCtx<'a>, typer: &mut Typer<'a>) -> TypingResult<'a> {
    let t = match self {
      &Literal::Bool(_) => SolidType::Bool.into(),
      &Literal::Char(_) => SolidType::Char.into(),
      &Literal::Str(_) => SolidType::Ref(false, Box::new(SolidType::Str.into())).into(),
      &Literal::Int(_, ref suf) => SolidType::Int(suf.clone().unwrap_or(IntSuffix::Isize)).into(),
      &Literal::Float(_, ref suf) => SolidType::Float(suf.clone().unwrap_or(FloatSuffix::F64)).into(),
    };

    if let Some(h) = hint {
      if h != t {
        return TypingResult::Conflict;
      }
    }

    TypingResult::Success(t)
  }
}

impl<'a> Typable<'a> for Expr<'a> {
  fn resolve(&'a self, hint: Option<Type<'a>>, ctx: TypingCtx<'a>, typer: &mut Typer<'a>) -> TypingResult<'a> {
    match self {
      &Expr::Literal(ref lit) => typer.resolve(lit, hint, ctx),
      &Expr::Tuple(ref elem) => {
        let mut changed = false;
        let mut sufficient = false;
        let mut result = Vec::with_capacity(elem.len());

        let h = match hint {
          // TODO: use infinity iterator
          None => vec![None; elem.len()],
          Some(Type::Solid(SolidType::Tuple(t))) => t.into_iter().map(Some).collect(),
          _ => return TypingResult::Conflict,
        };

        for (e, eh) in elem.iter().zip(h.into_iter()) {
          if let Some(s) = typing_try!(typer.resolve(e, eh, ctx.clone()), changed, sufficient) {
            result.push(s);
          }
        }

        if result.len() == elem.len() {
          assert!(sufficient);
          TypingResult::Success(SolidType::Tuple(result).into())
        } else {
          if changed {
            TypingResult::Insufficient
          } else {
            TypingResult::Unchanged
          }
        }
      },
      &Expr::Array(ref elem) => {
        let mut changed = false;
        let mut sufficient = false;

        let mut bt = match hint {
          // TODO: use infinity iterator
          None => None,
          // TODO: check for expr not able to be evaluated at compile-time
          // Both result in an len=None, but we should emit an error if it cannot be evaluated
          Some(Type::Solid(SolidType::Array(t, len))) if len.is_none() || len == Some(elem.len() as u128)
            => Some(*t),
          _ => return TypingResult::Conflict,
        };

        for e in elem.iter() {
          if let Some(nbt) = typing_try!(typer.resolve(e, bt.clone(), ctx.clone()), changed, sufficient) {
            bt = Some(nbt);
          }
        }

        if let Some(b) = bt {
          if sufficient {
            // We may actually get a sufficient=true and bt = None if we have no elment, hence the check
            return TypingResult::Success(SolidType::Array(Box::new(b), Some(elem.len() as u128)).into());
          }
        }

        if changed {
          TypingResult::Insufficient
        } else {
          TypingResult::Unchanged
        }
      },
      &Expr::ArrayFill{ ref filler, ref count } => {
        let c = if let Some(i) = count {
          *i
        } else {
          return TypingResult::Conflict // Cannot evaluate at compile time
        };

        let bt = match hint {
          // TODO: use infinity iterator
          None => None,
          Some(Type::Solid(SolidType::Array(t, len))) if len.is_none() || len == Some(c)
            => Some(*t),
          _ => return TypingResult::Conflict,
        };

        typer.resolve(filler.as_ref(), bt, ctx)
      },
      &Expr::Call{ .. } => unimplemented!(),
      &Expr::Field{ .. } => unimplemented!(),
      &Expr::ArrayIndex{ ref owner, .. } => {
        let mut changed = false;
        let mut _sufficient = false;

        let owner_type = typing_try!(typer.resolve(owner.as_ref(), None, ctx), changed, _sufficient);

        if let Some(owner_inner) = owner_type {
          if let Type::Solid(SolidType::Array(bt, _)) = owner_inner {
            // TODO: check ascriptions
            return TypingResult::Success(*bt);
          } else {
            unimplemented!("Desuger to ops overload calls");
          }
        }

        if changed {
          TypingResult::Insufficient
        } else {
          TypingResult::Unchanged
        }
      },
      &Expr::TupleIndex{ ref owner, idx } => {
        let mut changed = false;
        let mut _sufficient = false;

        let owner_type = typing_try!(typer.resolve(owner.as_ref(), None, ctx), changed, _sufficient);

        if let Some(owner_inner) = owner_type {
          if let Type::Solid(SolidType::Tuple(tt)) = owner_inner {
            // TODO: check ascriptions
            return TypingResult::Success(tt[idx as usize].clone());
          } else {
            return TypingResult::Conflict;
          }
        }

        if changed {
          TypingResult::Insufficient
        } else {
          TypingResult::Unchanged
        }
      },
      &Expr::Closure{ ref params, ref ret, ref body } => unimplemented!(),
      &Expr::FlowCtrl(ref fc) => match fc {
        &FlowCtrl::Break() => {
        },
      },
      &Expr::Block{ ref body, ref ret } => {
        let mut changed = false;
        let mut sufficient = false;

        let mut bctx = ctx;

        for stmt in body.iter() {
          match stmt {
            &Stmt::Empty => continue,
            &Stmt::Expr(ref e) => { typing_try!(typer.resolve(e, None, bctx.clone()), changed, sufficient); },
            &Stmt::Let(ref p, ref e) => {
              if let &Pat::Ident { ref name, ref is_ref, ref is_mut, .. } = p {
                let binding_mut = *is_mut && !*is_ref;
                bctx.scope.insert(name, Scoped::Binding { from: p, is_mut: binding_mut });
              }

              let pt = typing_try!(typer.resolve(p, None, bctx.clone()), changed, sufficient);

              if let Some(ei) = e {
                let et = typing_try!(typer.resolve(ei, None, bctx.clone()), changed, sufficient);

                // Try again with other's type
                if pt.is_some() {
                  typing_try!(typer.resolve(ei, pt, bctx.clone()), changed, sufficient);
                }
                if et.is_some() {
                  typing_try!(typer.resolve(p, et, bctx.clone()), changed, sufficient);
                }
              }
            },
            &Stmt::Item(_) => unimplemented!(),
          }
        }

        let self_type = if let &Some(ref e) = ret {
          typing_try!(typer.resolve(e.as_ref(), hint, bctx), changed, sufficient)
        } else {
          if hint != Some(UNIT_TYPE) {
            changed = true;
          }

          Some(UNIT_TYPE)
        };

        // FIXME: check hint

        if !sufficient {
          if !changed {
            return TypingResult::Unchanged;
          } else {
            return TypingResult::Insufficient;
          }
        } else if let Some(t) = self_type {
          return TypingResult::Success(t);
        } else {
          return TypingResult::Insufficient;
        }
      },
    }
  }
}

impl<'a> Typable<'a> for Pat<'a> {
  fn resolve(&'a self, hint: Option<Type<'a>>, ctx: TypingCtx<'a>, typer: &mut Typer<'a>) -> TypingResult<'a> {
    match self {
      &Pat::Ident { bounded: None, is_mut, is_ref, name } => {
        // TODO: type ascriptions
        match hint {
          Some(t) => TypingResult::Success(t),
          None => TypingResult::Insufficient,
        }
      },
      _ => {
        // Not universally typable
        return TypingResult::Conflict;
      }
    }
  }
}
