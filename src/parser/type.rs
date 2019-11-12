use nom::{
  alt, named, map, opt, tag, call, complete,
};

use crate::grammar::*;
use super::ident::*;
use super::literal::*;
use super::path::*;

named!(
  pub tuple_type<&str, Type>,
  map!(
    mrws!(
      tuple!(
        tag!("("),
        opt!(complete!(
          mrws!(
            separated_pair!(
              r#type,
              tag!(","),
              mrws!(separated_list!(
                tag!(","),
                r#type
              ))
            )
          )
        )),
        tag!(")")
      )
    ),
    |(_, r, _)| {
      match r {
        None => UNIT_TYPE,
        Some((first, mut rest)) => {
          let mut v = Vec::with_capacity(rest.len() + 1);
          v.push(first);
          v.append(&mut rest);
          SolidType::Tuple(v).into()
        },
      }
    }
  )
);

named!(ref_type<&str, Type>,
  map!(
    mrws!(tuple!(
      tag!("&"),
      opt!(tag!(keywords::KW_MUT)),
      type_no_bound
    )),
    |(_, m, t)| SolidType::Ref(m.is_some(), Box::new(t)).into()
  )
);

named!(array_type<&str, Type>,
  map!(
    mrws!(tuple!(
      tag!("["),
      r#type,
      tag!(";"),
      // TODO: EXPR
      literal,
      tag!("]")
    )),
    |(_, t, _, n, _)| SolidType::Array(Box::new(t), 0).into() // TODO: compute length
  )
);

named!(slice_type<&str, Type>,
  map!(
    mrws!(delimited!(
      tag!("["),
      r#type,
      tag!("]")
    )),
    |t| SolidType::Slice(Box::new(t)).into()
  )
);

named!(bare_func_params<&str, Vec<Type>>,
  mrws!(terminated!(
    mrws!(separated_list!(
      tag!(","),
      r#type
    )),
    opt!(tag!(","))
  ))
);

named!(bare_func_ret<&str, Type>,
  mrws!(preceded!(
    tag!("->"),
    r#type
  ))
);

named!(bare_func<&str, Type>,
  map!(
    mrws!(tuple!(
      tag!("fn"),
      tag!("("),
      bare_func_params,
      tag!(")"),
      opt!(complete!(bare_func_ret))
    )),
    |(_, _, p, _, r)| {
      SolidType::BareFunc(FnTypeSpec {
        args: p,
        ret: Box::new(r.unwrap_or_else(|| UNIT_TYPE)),
      }).into()
    }
  )
);

named!(
  type_no_bound<&str, Type>,
  alt!(
    tuple_type
    | ref_type
    | array_type
    | slice_type
    | map!(tag!("_"), |_| Type::Placeholder)
    | map!(tag!("!"), |_| SolidType::Never.into())
    | bare_func
    | mrws!(delimited!(tag!("("), r#type, tag!(")")))
    | map!(type_path, |p| SolidType::Ident(p).into())
  )
);

named!(pub r#type<&str, Type>, call!(type_no_bound));
