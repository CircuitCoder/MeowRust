use nom::{alt, complete, map, named, opt, tag};

use super::expr::*;
use super::item::*;
use super::pattern::pat;
use crate::grammar::Stmt;

named!(pub stmt<&str, Stmt>, alt!(
    map!(tag!(";"), |_| Stmt::Empty)
  | map!(item, Stmt::Item)
  | let_stmt
  | expr_stmt
));

named!(let_stmt<&str, Stmt>, map!(mrws!(tuple!(
  tag!("let"),
  pat,
  opt!(complete!(mrws!(preceded!(
    tag!("="),
    expr
  )))),
  tag!(";")
)), |(_, p, e, _)| Stmt::Let(p, e)));

named!(expr_stmt<&str, Stmt>, map!(alt!(
    expr_with_block
  | mrws!(terminated!(expr_without_block, tag!(";")))
), Stmt::Expr));
