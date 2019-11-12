use nom::{alt, map, named, preceded, tag, take_while, IResult};

#[allow(dead_code)]
pub mod keywords {
  pub const KW_AS: &'static str = "as";
  pub const KW_BREAK: &'static str = "break";
  pub const KW_CONST: &'static str = "pub const";
  pub const KW_CONTINUE: &'static str = "continue";
  // No supports for crate
  pub const KW_ELSE: &'static str = "else";
  pub const KW_ENUM: &'static str = "enum";
  // No supports for extern
  pub const KW_FALSE: &'static str = "false";
  pub const KW_FN: &'static str = "fn";
  pub const KW_FOR: &'static str = "for";
  pub const KW_IF: &'static str = "if";
  pub const KW_IMPL: &'static str = "impl";
  pub const KW_IN: &'static str = "in";
  pub const KW_LET: &'static str = "let";
  pub const KW_LOOP: &'static str = "loop";
  pub const KW_MATCH: &'static str = "match";
  pub const KW_MOD: &'static str = "mod";
  // No supports for move
  pub const KW_MUT: &'static str = "mut";
  // No supports for pub
  pub const KW_REF: &'static str = "ref";
  pub const KW_RETURN: &'static str = "return";
  pub const KW_SELFVALUE: &'static str = "self";
  pub const KW_SELFTYPE: &'static str = "Self";
  pub const KW_STATIC: &'static str = "static";
  pub const KW_SUPER: &'static str = "super"; // TODO: what is this?
  pub const KW_TRAIT: &'static str = "trait";
  pub const KW_TRUE: &'static str = "true";
  pub const KW_TYPE: &'static str = "type";
  pub const KW_UNSAFE: &'static str = "unsafe"; // MeowRust don't differentiate safe and unsafe context
  pub const KW_USE: &'static str = "use";
  pub const KW_WHERE: &'static str = "where";
  pub const KW_WHILE: &'static str = "while";

  pub const ALL: [&'static str; 30] = [
    "as",
    "break",
    "pub const",
    "continue",
    "else",
    "enum",
    "false",
    "fn",
    "for",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "match",
    "mod",
    "mut",
    "ref",
    "return",
    "self",
    "Self",
    "static",
    "super",
    "trait",
    "true",
    "type",
    "unsafe",
    "use",
    "where",
    "while",
  ];
}

fn valid_ident_char(c: char) -> bool {
  match c {
    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
    _ => false,
  }
}

named!(unchecked_ident<&str, &str>, take_while!(valid_ident_char));

fn ident_or_kw(input: &str) -> IResult<&str, &str> {
  if input.starts_with('_') {
    let (left, ident) = unchecked_ident(input)?;
    if ident.len() < 2 {
      return Err(nom::Err::Error((input, nom::error::ErrorKind::Char)));
    }

    return Ok((left, ident));
  }

  match input.chars().next() {
    Some('a'..='z') | Some('A'..='Z') => unchecked_ident(input),
    _ => Err(nom::Err::Error((input, nom::error::ErrorKind::Alt))),
  }
}

named!(raw_ident<&str, &str>,
  preceded!(tag!("r#"), ident_or_kw)
);

fn non_kw_ident(input: &str) -> IResult<&str, &str> {
  let (left, ident) = ident_or_kw(input)?;
  for kw in keywords::ALL.iter() {
    if ident == *kw {
      return Err(nom::Err::Error((input, nom::error::ErrorKind::Alt)));
    }
  }

  return Ok((left, ident));
}

named!(pub ident<&str, &str>,
  alt!(raw_ident | non_kw_ident)
);

named!(pub lifetime_or_label<&str, &str>,
  preceded!(tag!("'"), non_kw_ident)
);

named!(pub lifetime_token<&str, &str>,
  alt!(lifetime_or_label | map!(tag!("'_"), |_| "_"))
);
