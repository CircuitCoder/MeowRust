use super::*;

#[derive(Debug, Clone)]
pub enum AttrValue {
  /**
   * #[attr=literal]
   */
  Literal(super::Literal),

  // TODO: support token tree (\w macros probably?)
}

#[derive(Debug, Clone)]
pub enum AttrType<'a> {
  /**
   * Specify an language item
   */
  Lang,

  /**
   * Specify the path of a module source file
   */
  Path,

  /**
   * User defined items
   */
  Custom(super::SimplePath<'a>),
}

impl<'a> AttrType<'a> {
  pub fn parse(p: super::SimplePath<'a>) -> Self {
    if p.from_root || p.segments.len() > 1 {
      // Built-in attrs only have one segments, and are not prefixed by ::
      return Self::Custom(p);
    }

    match p.segments.iter().next() {
      Some((PathSeg::Ident("lang"), _)) => Self::Lang,
      Some((PathSeg::Ident("path"), _)) => Self::Path,
      _ => return Self::Custom(p),
    }
  }
}

/**
 * An attribute, outer or inner
 */
#[derive(Debug, Clone)]
pub struct Attr<'a>(pub AttrType<'a>, pub AttrValue);
