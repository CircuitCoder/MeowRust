use nom::{
  named, switch, take, char, alt, map,
  tuple, tag,
  none_of,
  many_till,
  preceded,
  fold_many_m_n,
  opt,
  one_of,
  value,
  many0,
  IResult,
};

use crate::grammar::{Literal, IntSuffix};

named!(BIN_DIGIT<&str, u32>, map!(
  one_of!("01"),
  |c: char| c.to_digit(2).unwrap()
));

named!(OCT_DIGIT<&str, u32>, map!(
  one_of!("01234567"),
  |c: char| c.to_digit(8).unwrap()
));

named!(DEC_DIGIT<&str, u32>, map!(
  one_of!("0123456789"),
  |c: char| c.to_digit(10).unwrap()
));

named!(HEX_DIGIT<&str, u32>, map!(
  one_of!("0123456789abcdefABCDEF"),
  |c: char| c.to_digit(16).unwrap()
));

named!(QUOTE_ESCAPE<&str, char>,
  switch!(take!(2),
      "\\\"" => char!('"')
    | "\\\'" => char!('\'')
  )
);

named!(pub ASCII_ESCAPE<&str, char>,
  alt!(
    switch!(take!(2),
        "\\n" => value!('\n')
      | "\\r" => value!('\r')
      | "\\t" => value!('\t')
      | "\\\\" => value!('\\')
      | "\\0" => value!('\0')
    )
    |
    map!(
      tuple!(tag!("\\x"), OCT_DIGIT, HEX_DIGIT),
      |(_, o, h)| (o * 16 + h) as u8 as char
    )
  )
);

named!(CHAR_LITERAL<&str, Literal>,
  map!(
    tuple!(
      tag!("'"),
      alt!(
          none_of!("'\\\n\r\t")
        | QUOTE_ESCAPE
        | ASCII_ESCAPE
      ),
      tag!("'")
    ),
    |(_, c, _)| Literal::Char(c)
  )
);

named!(pub STRING_LITERAL<&str, Literal>,
  map!(
    tuple!(
      tag!("\""),
      many_till!(
        alt!(
            none_of!("\\\n\"")
          | QUOTE_ESCAPE
          | ASCII_ESCAPE
        ),
        tag!("\"")
      )
    ),
    |(_, (c, _))| Literal::Str(c.into_iter().collect())
  )
);

named!(RAW_STRING_LITERAL<&str, Literal>,
  preceded!(
    tag!("r"),
    RAW_STRING_CONTENT
  )
);

fn RAW_STRING_CONTENT(input: &str) -> IResult<&str, Literal> {
  // Count front
  let mut pending = input;
  let mut count = 0;
  loop {
    match pending.chars().next() {
      Some('#') => {
        count += 1;
        pending = &pending[1..];
      },
      Some('\"') => {
        pending = &pending[1..];
        break;
      },
      _ => return Err(nom::Err::Error((input, nom::error::ErrorKind::Alt))),
    }
  }

  // println!("Pending: {}", pending);

  let mut curcount: usize = 0;
  let mut endstart: usize = 0;
  let mut endcount: usize = 0;
  let mut checking: bool = false;
  for c in pending.chars() {
    if c == '"' {
      checking = true;
      endstart = curcount;
      endcount = 0;
      // println!("Reset @ {}", curcount);
    } else if checking && c == '#' {
      endcount += 1;
      if endcount == count {
        // Doing real job here
        let mut result = Vec::new();
        let mapped = &pending[..endstart];
        let rest = &pending[(curcount+1)..];

        for r in mapped.chars() {
          if r == '\n' {
            if result.last() == Some(&'\\') {
              *result.last_mut().unwrap() = r;
            } else {
              return Err(nom::Err::Error((input, nom::error::ErrorKind::Alt)))
            }
          } else {
            result.push(r);
          }
        }

        return Ok((rest, Literal::Str(result.iter().collect())));
      }
    } else {
      checking = false
    }

    curcount += c.len_utf8();
  }

  return Err(nom::Err::Error((input, nom::error::ErrorKind::Alt)))
}

named!(BIN_LITERAL<&str, u128>,
  preceded!(
    tag!("0b"),
    fold_many_m_n!(1, 128, BIN_LITERAL,
      0, |acc, item| acc * 2 + item)
  )
);

named!(OCT_LITERAL<&str, u128>,
  preceded!(
    tag!("0b"),
    fold_many_m_n!(1, 128, OCT_LITERAL,
      0, |acc, item| acc * 2 + item)
  )
);

named!(DEC_LITERAL<&str, u128>,
  preceded!(
    tag!("0b"),
    fold_many_m_n!(1, 128, DEC_LITERAL,
      0, |acc, item| acc * 2 + item)
  )
);

named!(HEX_LITERAL<&str, u128>,
  preceded!(
    tag!("0b"),
    fold_many_m_n!(1, 128, HEX_LITERAL,
      0, |acc, item| acc * 2 + item)
  )
);

named!(INTEGER_SUFFIX<&str, IntSuffix>,
  alt!(
      map!(tag!("u8"), |_| IntSuffix::U8)
    | map!(tag!("u16"), |_| IntSuffix::U16)
    | map!(tag!("u32"), |_| IntSuffix::U32)
    | map!(tag!("u64"), |_| IntSuffix::U64)
    | map!(tag!("u128"), |_| IntSuffix::U128)
    | map!(tag!("usize"), |_| IntSuffix::Usize)

    | map!(tag!("i8"), |_| IntSuffix::I8)
    | map!(tag!("i16"), |_| IntSuffix::I16)
    | map!(tag!("i32"), |_| IntSuffix::I32)
    | map!(tag!("i64"), |_| IntSuffix::I64)
    | map!(tag!("i128"), |_| IntSuffix::I128)
    | map!(tag!("isize"), |_| IntSuffix::Isize)
  )
);

named!(INTEGER_LITERAL<&str, Literal>,
  map!(
    tuple!(
      alt!(
          BIN_LITERAL
        | OCT_LITERAL
        | DEC_LITERAL
        | HEX_LITERAL
      ),
      opt!(INTEGER_SUFFIX)
    ),
    |(l, s)| Literal::Int(l, s)
  )
);

named!(BOOLEAN_LITERAL<&str, Literal>,
  alt!(
      map!(tag!("true"), |_| Literal::Bool(true))
    | map!(tag!("false"), |_| Literal::Bool(false))
  )
);

named!(pub LITERAL<&str, Literal>,
  alt!(
      CHAR_LITERAL
    | STRING_LITERAL
    | RAW_STRING_LITERAL
    | INTEGER_LITERAL
    | BOOLEAN_LITERAL
  )
);
