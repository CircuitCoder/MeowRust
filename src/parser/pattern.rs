use nom::{alt, complete, map, named, opt, tag};

use super::ident::ident;
use super::literal::literal;
use crate::grammar::Pat;

named!(pub pat<&str, Pat>, alt!(
    literal_pat
  | ident_pat
));

// TODO: add let

named!(literal_pat<&str, Pat>, map!(literal, Pat::Literal));

named!(ident_pat<&str, Pat>, map!(
  mrws!(tuple!(
    opt!(complete!(tag!("ref"))),
    opt!(complete!(tag!("mut"))),
    ident,
    opt!(complete!(mrws!(preceded!(
      tag!("@"),
      pat
    ))))
  )),
  |(r, m, i, b)| Pat::Ident {
    name: i,
    is_ref: r.is_some(),
    is_mut: m.is_some(),
    bounded: b.map(Box::new),
  }
));
