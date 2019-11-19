use nom::{
  named, tuple, alt, map, delimited, tag
};

use crate::grammar::attr::*;
use super::path::simple_path;
use super::literal::literal;

named!(pub outer_attr<&str, Attr>, mrws!(delimited!(
  tag!("#["),
  attr,
  tag!("]")
)));

named!(pub inner_attr<&str, Attr>, mrws!(delimited!(
  tag!("#!["),
  attr,
  tag!("]")
)));

named!(attr<&str, Attr>, map!(
  mrws!(tuple!(
    simple_path,
    attr_input
  )),
  |(p, v)| Attr(AttrType::parse(p), v)
));

// TODO: support token tree
named!(attr_input<&str, AttrValue>, map!(mrws!(preceded!(
  tag!("="),
  literal
)), AttrValue::Literal));
