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
        None => Type::Tuple(Vec::new()),
        Some((first, mut rest)) => {
          let mut v = Vec::with_capacity(rest.len() + 1);
          v.push(first);
          v.append(&mut rest);
          Type::Tuple(v)
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
    |(_, m, t)| Type::Ref(m.is_some(), Box::new(t))
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
    |(_, t, _, n, _)| Type::Array(Box::new(t), 0)
  )
);

named!(slice_type<&str, Type>,
  map!(
    mrws!(delimited!(
      tag!("["),
      r#type,
      tag!("]")
    )),
    |t| Type::Slice(Box::new(t))
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
      Type::BareFunc(FnTypeSpec {
        args: p,
        ret: Box::new(r.unwrap_or_else(|| UNIT_TYPE)),
      })
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
    | map!(tag!("!"), |_| Type::Never)
    | bare_func
    | mrws!(delimited!(tag!("("), r#type, tag!(")")))
    | map!(type_path, Type::Ident)
  )
);

named!(pub r#type<&str, Type>, call!(type_no_bound));
