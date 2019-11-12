use nom::{alt, complete, map, named, opt, separated_nonempty_list, tag, IResult};
use std::collections::HashMap;

use super::ident::*;
use super::r#type::r#type;
use crate::grammar::*;

named!(
  simple_path_seg<&str, PathSeg>,
  alt!(
      map!(ident, PathSeg::Ident)
    | map!(tag!(keywords::KW_SUPER), |_| PathSeg::Super)
    | map!(tag!(keywords::KW_SELFVALUE), |_| PathSeg::SelfVal)
  )
);

named!(
  path_ident_seg<&str, PathSeg>,
  alt!(
      map!(ident, PathSeg::Ident)
    | map!(tag!(keywords::KW_SUPER), |_| PathSeg::Super)
    | map!(tag!(keywords::KW_SELFVALUE), |_| PathSeg::SelfVal)
    | map!(tag!(keywords::KW_SELFTYPE), |_| PathSeg::SelfType)
  )
);

named!(
  pub simple_path<&str, SimplePath>,
  map!(
    mrws!(
      tuple!(
        opt!(complete!(tag!("::"))),
        mrws!(separated_nonempty_list!(
          tag!("::"), simple_path_seg
        ))
      )
    ),
    |(r, v)| SimplePath {
      from_root: r.is_some(),
      segments: v.into_iter().map(|v| (v, ())).collect(),
    }
  )
);

named!(
  generic_args<&str, GenericArgs>,
  map!(
    mrws!(delimited!(
      tag!("<"),
      opt!(generic_args_inner),
      tag!(">")
    )),
    |i| i.unwrap_or_else(Default::default)
  )
);

named!(
  generic_args_inner<&str, GenericArgs>,
  mrws!(terminated!(
    alt!(
        map!(generic_args_types, |t| GenericArgs { types: t, bindings: HashMap::new() })
      | map!(generic_args_bindings, |b| GenericArgs { types: Vec::new(), bindings: b })
      | map!(
        mrws!(separated_pair!(
          generic_args_types,
          tag!(","),
          generic_args_bindings
        )),
        |(t, b)| GenericArgs { types: t, bindings: b }
      )
    ),
    opt!(tag!(","))
  ))
);

named!(
  generic_args_types<&str, Vec<Type>>,
  mrws!(separated_nonempty_list!(
    tag!(","),
    r#type
  ))
);

named!(
  generic_args_bindings<&str, HashMap<&str, Type>>,
  map!(
    mrws!(separated_nonempty_list!(
      tag!(","),
      mrws!(separated_pair!(
        ident,
        tag!("="),
        r#type
      ))
    )),
    |v| {
      let mut r = HashMap::with_capacity(v.len());
      for (i, t) in v.into_iter() {
        // TODO: error if dup?
        r.insert(i, t);
      }

      r
    }
  )
);

named!(
  pub path_in_expr<&str, PathInExpr>,
  map!(
    mrws!(
      tuple!(
        opt!(complete!(tag!("::"))),
        mrws!(separated_nonempty_list!(
          tag!("::"), path_in_expr_seg
        ))
      )
    ),
    |(r, v)| PathInExpr {
      from_root: r.is_some(),
      segments: v,
    }
  )
);

named!(
  path_in_expr_seg<&str, (PathSeg, GenericArgs)>,
  map!(
    mrws!(tuple!(
      path_ident_seg,
      opt!(complete!(mrws!(preceded!(
        tag!("::"),
        generic_args
      ))))
    )),
    |(s, a)| (s, a.unwrap_or_else(|| Default::default()))
  )
);

named!(
  pub type_path<&str, TypePath>,
  map!(
    mrws!(
      tuple!(
        opt!(tag!("::")),
        mrws!(separated_nonempty_list!(
          complete!(tag!("::")), type_path_seg
        ))
      )
    ),
    |(r, v)| TypePath {
      from_root: r.is_some(),
      segments: v,
    }
  )
);

named!(
  type_path_seg<&str, (PathSeg, TypeSegArgs)>,
  map!(
    mrws!(tuple!(
      path_ident_seg,
      opt!(complete!(mrws!(preceded!(
        opt!(tag!("::")),
        alt!(
          map!(generic_args, TypeSegArgs::NonFnArgs)
          | map!(type_path_fn, TypeSegArgs::FnArgs)
        )
      ))))
    )),
    |(s, a)| (s, a.unwrap_or(TypeSegArgs::None))
  )
);

named!(
  type_path_fn<&str, FnTypeSpec>,
  map!(
    mrws!(tuple!(
      mrws!(delimited!(
        tag!("("),
        separated_list!(tag!(","), r#type),
        mrws!(tuple!(opt!(tag!(",")), tag!(")")))
      )),
      opt!(complete!(
        mrws!(preceded!(
          tag!("->"),
          r#type
        ))
      ))
    )),
    |(p, r)| FnTypeSpec { args: p, ret: Box::new(r.unwrap_or_else(|| UNIT_TYPE)) }
  )
);

// TODO: qualified
