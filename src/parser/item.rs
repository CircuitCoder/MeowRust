use nom::{
  named, alt, map, map_opt, complete, opt, tag, separated_nonempty_list,
};

use crate::grammar::item::*;
use crate::grammar::*;
use super::attr::*;
use super::ident::*;
use super::path::*;
use super::r#type::*;
use super::pattern::*;
use super::expr::block_expr;

named!(pub item<&str, Item>, map!(
  mrws!(tuple!(complete!(opt!(outer_attr)), item_value)),
  |(a, i)| Item { attr: a, value: i }
));

// We don't have visibility qualifiers, so we're calling VisItem item_value
named!(item_value<&str, ItemValue>, alt!(
  module
  | use_decl
  | map!(func, ItemValue::Func)
  | map!(type_alias, ItemValue::TypeAlias)
));

named!(module<&str, ItemValue>, map!(
  mrws!(tuple!(
    tag!("mod"),
    ident,
    alt!(
      map!(mrws!(delimited!(
        tag!("{"),
        mrws!(many0!(item)),
        tag!("}")
      )), Some)
      | map!(tag!(";"), |_| None)
    )
  )),
  |(_, name, inner)| ItemValue::Module(name, inner)
));

// TODO: support {a, b, c} and *
named!(use_decl<&str, ItemValue>, mrws!(preceded!(
  tag!("use"),
  map!(mrws!(tuple!(
    simple_path,
    opt!(mrws!(preceded!(tag!("as"), ident)))
  )), |(p, alias)| ItemValue::UseDecl(vec![(p, alias)]))
)));

named!(type_param<&str, TypeParam>, map!(
  mrws!(tuple!(
    ident,
    opt!(mrws!(preceded!(tag!(":"), type_param_bounds))),
    opt!(mrws!(preceded!(tag!("="), r#type)))
  )),
  |(name, tb, default)| TypeParam {
    name,
    trait_bounds: tb.unwrap_or_else(Vec::new),
    default,
  }
));

named!(lifetime<&str, Lifetime>, alt!(
  map!(tag!("'static"), |_| Lifetime::Static)
  | map!(tag!("'_"), |_| Lifetime::Unnamed)
  | map!(lifetime_or_label, |name| Lifetime::Named(name))
));

named!(lifetime_bounds<&str, Vec<Lifetime>>,
  mrws!(terminated!(
    mrws!(separated_list!(tag!("+"), lifetime)),
    opt!(complete!(tag!("+")))
  ))
);

named!(lifetime_param<&str, LifetimeParam>, map!(
  mrws!(tuple!(
    lifetime_or_label,
    opt!(mrws!(preceded!(tag!(":"), lifetime_bounds)))
  )),
  |(name, lb)| LifetimeParam {
    name,
    lifetime_bounds: lb.unwrap_or_else(Vec::new),
  }
));

/**
 *  TODO: investigate: the ref says trait bounds can be empty, effectively yields
 *   where T:
 *  Also, reference says that we can have 0 items
 */
named!(where_clause<&str, Vec<(Type, Vec<TypePath>)>>, mrws!(delimited!(
  tag!("where"),
  mrws!(separated_nonempty_list!(tag!(","), where_clause_item)),
  opt!(complete!(tag!(",")))
)));

named!(where_clause_item<&str, (Type, Vec<TypePath>)>, mrws!(separated_pair!(
  r#type,
  tag!(":"),
  type_param_bounds
)));

named!(generics<&str, Vec<GenericParam>>, mrws!(delimited!(
  tag!("<"),
  map_opt!(mrws!(terminated!(
    separated_list!(tag!(","), alt!(
      map!(lifetime_param, |p| GenericParam::Lifetime(p))
      | map!(type_param, |p| GenericParam::Type(p))
    )),
    opt!(complete!(tag!(",")))
  )), |params: Vec<_>| {
    let mut found = false;
    // lifetime params must appear before type params
    for param in params.iter() {
      match param {
        GenericParam::Type(_) => found = true,
        GenericParam::Lifetime(_) => if found { return None; },
      }
    }
    Some(params)
  }),
  tag!(">")
)));

// TODO: support qualifiers?
named!(func<&str, Func>, map!(
  mrws!(tuple!(
    tag!("fn"),
    ident,
    opt!(generics),
    tag!("("),
    func_params,
    tag!(")"),
    opt!(mrws!(preceded!(tag!("->"), r#type))),
    opt!(where_clause),
    block_expr
  )),
  |(_, name, generics, _, params, _, ret, r#where, body)| Func {
    name,
    params,
    generics: generics.unwrap_or_else(Vec::new),
    r#where: r#where.unwrap_or_else(Vec::new),
    ret: ret.unwrap_or(UNIT_TYPE),
    body,
  }
));

named!(func_params<&str, Vec<(Pat, Type)>>, mrws!(terminated!(
  mrws!(separated_list!(
    tag!(","),
    mrws!(separated_pair!(
      pat,
      tag!(":"),
      r#type
    ))
  )),
  opt!(complete!(tag!(",")))
)));

named!(type_alias<&str, TypeAlias>, map!(
  mrws!(tuple!(
    tag!("type"),
    ident,
    opt!(generics),
    opt!(where_clause),
    tag!("="),
    r#type
  )),
  |(_, name, generics, r#where, _, value)| TypeAlias {
    name,
    generics: generics.unwrap_or_else(Vec::new),
    r#where: r#where.unwrap_or_else(Vec::new),
    value,
  }
));

