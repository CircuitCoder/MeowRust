use crate::grammar;
use nom::{call, named, IResult};

// Common

const SEPARATORS: &'static str = "+-*/%^!&|><=@_.,;:#$?([{)]{";
const WHITESPACES: &'static str = " \n\r\t";

fn till_sep(input: &str) -> IResult<&str, &str> {
  let first_char = match input.chars().next() {
    None => return Ok(("", "")), // EOF, considered as a success
    Some(c) => c,
  };

  if WHITESPACES.chars().filter(|c| *c == first_char).count() != 0 {
    let mut split = 0;
    for c in input.chars() {
      if WHITESPACES.chars().filter(|w| *w == c).count() == 0 {
        return Ok((&input[split..], &input[..split]));
      }

      split += c.len_utf8();
    }

    return Ok(("", input));
  }

  /*
  if SEPARATORS.chars().filter(|c| *c == first_char).count() != 0 {
    return Ok((input, ""));
  }
  */

  // return Err(nom::Err::Error((input, nom::error::ErrorKind::Alt)))
  Ok((input, ""))
}

macro_rules! mrws {
  ($i:expr, $($args:tt)*) => ({
    use nom::Err;
    use nom::lib::std::result::Result::*;
    use nom::sep;
    use $crate::parser::till_sep;

    match sep!($i, till_sep, $($args)*) {
      Err(e) => Err(e),
      Ok((i1, o)) => {
        match till_sep(i1) {
          Err(e) => dbg!(Err(Err::convert(e))),
          Ok((i2, _)) => Ok((i2, o)),
        }
      }
    }
  })
}

pub mod expr;
pub mod ident;
pub mod literal;
pub mod path;
pub mod pattern;
pub mod stmt;
pub mod r#type;
pub mod attr;
pub mod item;

named!(pub parse<&str, grammar::Expr>, call!(expr::expr));
