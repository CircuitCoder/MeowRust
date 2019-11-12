use nom::{
  named, map, alt, complete, tag, opt, many0, terminated, separated_nonempty_list, value, one_of,
};

use super::literal::literal;
use crate::grammar::*;
use super::stmt::stmt;
use super::ident::*;
use super::literal::tuple_idx;
use super::path::path_in_expr;
use super::pattern::pat;
use super::r#type::type_no_bound;

named!(t1_expr<&str, Expr>, alt!(
    grouped_expr
  | map!(literal, Expr::Literal) // literal expr
  | tuple_expr
  | array_expr
));

named!(t2_expr<&str, Expr>, alt!(
    map!(path_in_expr, Expr::Path) // Path expr
  | t1_expr
));

named!(t3_expr<&str, Expr>, alt!(
    tuple_idx_expr
  | field_expr

  | t2_expr
));

named!(t4_expr<&str, Expr>, alt!(
    call_expr
  | method_call_expr
  | idx_expr

  | t3_expr
));

named!(t5_expr<&str, Expr>, alt!(
    error_propagation_expr

  | t4_expr
));

named!(t6_expr<&str, Expr>, alt!(
    borrow_expr
  | deref_expr
  | neg_expr
  | not_expr

  | t5_expr
));

named!(t7_expr<&str, Expr>, alt!(
    type_cast_expr

  | t6_expr
));

macro_rules! bin_ltr_expr {
  ($cur:ident, $op_parser:ident, $lower:ident) => {
    named!($cur<&str, Expr>, alt!(
        map!(
          mrws!(tuple!($lower, $op_parser, $cur)),
          |(l, o, r)| Expr::BinaryOp {
            lhs: Box::new(l),
            rhs: Box::new(r),

            op: o.into(),
          }
        )
      | $lower
    ));
  }
}

bin_ltr_expr!(arith_t1_expr, arith_op_t1, t7_expr);
bin_ltr_expr!(arith_t2_expr, arith_op_t2, arith_t1_expr);
bin_ltr_expr!(arith_t3_expr, arith_op_t3, arith_t2_expr);
bin_ltr_expr!(arith_t4_expr, arith_op_t4, arith_t3_expr);
bin_ltr_expr!(arith_t5_expr, arith_op_t5, arith_t4_expr);
bin_ltr_expr!(arith_t6_expr, arith_op_t6, arith_t5_expr);
bin_ltr_expr!(logical_t1_expr, logical_op_t1, arith_t6_expr);
bin_ltr_expr!(logical_t2_expr, logical_op_t2, logical_t1_expr);
bin_ltr_expr!(logical_t3_expr, logical_op_t3, logical_t2_expr);

named!(t8_expr<&str, Expr>, alt!(
    compound_assign_expr
  | logical_t3_expr
));

named!(pub expr_without_block<&str, Expr>, alt!(
    cont_expr
  | break_expr
  | ret_expr

  | closure_expr

  | t8_expr
));

named!(pub expr_with_block<&str, Expr>, alt!(
      block_expr
    | loop_expr
    | if_expr
    | match_expr
));

named!(pub expr<&str, Expr>, alt!(expr_without_block | expr_with_block));

/* Expr \w Block*/
named!(block_expr<&str, Expr>, map!(
  mrws!(delimited!(
    tag!("{"),
    opt!(complete!(block_expr_inner)),
    tag!("}")
  )),
  |i| match i {
    Some((s, r)) => Expr::Block{ body: s, ret: r.map(Box::new) },
    None => Expr::Block{ body: Vec::new(), ret: None },
  }
));

named!(block_expr_inner<&str, (Vec<Stmt>, Option<Expr>)>, mrws!(tuple!(
  mrws!(many0!(complete!(stmt))),
  opt!(complete!(expr_without_block))
)));

/* Expr \wo Block */
named!(tuple_expr<&str, Expr>, map!(
  mrws!(delimited!(
    tag!("("),
    opt!(complete!(tuple_expr_inner)),
    tag!(")")
  )),
  |i| match i {
    Some(i) => Expr::Tuple(i),
    None => Expr::Tuple(Vec::new()),
  }
));

named!(tuple_expr_inner<&str, Vec<Expr>>, map!(
  mrws!(tuple!(
    mrws!(many1!(complete!(terminated!(
      expr,
      tag!(",")
    )))),
    opt!(complete!(expr))
  )),
  |(mut v, l)| {
    if let Some(last) = l {
      v.push(last);
    }

    v
  }
));

named!(grouped_expr<&str, Expr>,
  mrws!(delimited!(
    tag!("("),
    complete!(expr),
    tag!(")")
  ))
);

named!(call_params<&str, Vec<Expr>>, mrws!(terminated!(
  mrws!(separated_list!(tag!(","), expr)),
  opt!(complete!(tag!(",")))
)));

named!(call_expr<&str, Expr>, map!(
  mrws!(tuple!(
    t3_expr,
    tag!("("),
    call_params,
    tag!(")")
  )),
  |(r, _, p, _)| Expr::Call {
    recv: Box::new(r),
    method: None,
    params: p,
  }
));

named!(method_call_expr<&str, Expr>, map!(
  mrws!(tuple!(
    t3_expr,
    tag!("."),
    ident,
    tag!("("),
    call_params,
    tag!(")")
  )),
  |(r, _, m, _, p, _)| Expr::Call {
    recv: Box::new(r),
    method: Some(m),
    params: p,
  }
));

named!(tuple_idx_expr<&str, Expr>, map!(
  mrws!(tuple!(
    t2_expr,
    tag!("."),
    tuple_idx
  )),
  |(o, _, i)| Expr::TupleIndex {
    owner: Box::new(o),
    idx: i,
  }
));

named!(field_expr<&str, Expr>, map!(
  mrws!(tuple!(
    t2_expr,
    tag!("."),
    ident
  )),
  |(o, _, i)| Expr::Field {
    owner: Box::new(o),
    field: i,
  }
));

named!(array_expr<&str, Expr>, map!(
  mrws!(delimited!(
    tag!("["),
    opt!(complete!(array_expr_inner)),
    tag!("]")
  )),
  |i| match i {
    Some(i) => i,
    None => Expr::Array(Vec::new()),
  }
));

named!(array_expr_inner<&str, Expr>, alt!(
    map!(mrws!(terminated!(
      mrws!(separated_nonempty_list!(
        tag!(","),
        expr
      )),
      opt!(complete!(tag!(",")))
    )), Expr::Array)
  | map!(
      mrws!(separated_pair!(
        expr,
        tag!(";"),
        expr
      )),
      |(f, c)| Expr::ArrayFill { filler: Box::new(f), count: Box::new(c) }
    )
));

named!(idx_expr<&str, Expr>, map!(
  mrws!(tuple!(
    t3_expr,
    tag!("("),
    expr,
    tag!(")")
  )),
  |(o, _, i, _)| Expr::ArrayIndex {
    owner: Box::new(o),
    idx: Box::new(i),
  }
));

named!(closure_expr<&str, Expr>, map!(
  mrws!(tuple!(
    tag!("|"),
    opt!(complete!(closure_params)),
    tag!("|"),
    expr
  )),
  |(_, p, _, b)| Expr::Closure {
    params: p.unwrap_or_else(Vec::new),
    body: Box::new(b),
  }
));

// TODO: add type ascription
named!(closure_params<&str, Vec<(Pat, Type)>>, 
  map!(mrws!(terminated!(
    mrws!(separated_nonempty_list!(
      tag!(","),
      pat
    )),
    opt!(complete!(tag!(",")))
  )), |p| p.into_iter().map(|e| (e, Type::Placeholder)).collect())
);

named!(cont_expr<&str, Expr>, map!(
  mrws!(preceded!(
    tag!("continue"),
    opt!(complete!(lifetime_or_label))
  )),
  |l| Expr::FlowCtrl(FlowCtrl::Cont{ label: l })
));

named!(break_expr<&str, Expr>, map!(
  mrws!(tuple!(
    tag!("break"),
    opt!(complete!(lifetime_or_label)),
    opt!(complete!(expr))
  )),
  |(_, label, r)| Expr::FlowCtrl(FlowCtrl::Break { label, ret: r.map(Box::new) })
));

named!(ret_expr<&str, Expr>, map!(
  mrws!(preceded!(
    tag!("return"),
    opt!(complete!(expr))
  )),
  |r| Expr::FlowCtrl(FlowCtrl::Ret { ret: r.map(Box::new) })
));

named!(loop_expr<&str, Expr>, map!(
  mrws!(tuple!(
    opt!(complete!(mrws!(terminated!(lifetime_or_label, tag!(":"))))),
    loop_cond,
    block_expr
  )),
  |(l, c, b)| Expr::Loop {
    label: l,
    cond: c,
    body: Box::new(b),
  }
));

named!(loop_cond<&str, LoopCond>, alt!(
    map!(tag!("loop"), |_| LoopCond::Infty)
  | map!(
      mrws!(preceded!(
        tag!("while"),
        expr
      )),
      |v| LoopCond::While(Box::new(v))
    )
  | map!(
      mrws!(tuple!(
        tag!("while"),
        tag!("let"),
        match_arm_pats,
        tag!("="),
        expr
      )),
      |(_, _, p, _, e)| LoopCond::WhileLet(p, Box::new(e))
    )
  | map!(
      mrws!(tuple!(
        tag!("for"),
        pat,
        tag!("in"),
        expr
      )),
      |(_, p, _, c)| LoopCond::For(p, Box::new(c))
    )
));

named!(if_expr<&str, Expr>, map!(
  mrws!(tuple!(
    if_cond,
    block_expr,
    opt!(complete!(mrws!(preceded!(
      tag!("else"),
      alt!(block_expr | if_expr)
    ))))
  )),
  |(c, b, e)| Expr::If {
    cond: c,
    body: Box::new(b),
    else_arm: e.map(Box::new),
  }
));

named!(if_cond<&str, IfCond>, alt!(
    map!(
      mrws!(preceded!(
        tag!("if"),
        expr
      )),
      |v| IfCond::Bool(Box::new(v))
    )
  | map!(
      mrws!(tuple!(
        tag!("if"),
        tag!("let"),
        pat,
        tag!("="),
        expr
      )),
      |(_, _, p, _, e)| IfCond::Let(p, Box::new(e))
    )
));

named!(match_arm_pats<&str, Vec<Pat>>, mrws!(preceded!(
  opt!(complete!(tag!("|"))),
  mrws!(separated_nonempty_list!(
    tag!("|"),
    pat
  ))
)));

named!(match_arm_guard<&str, Expr>, mrws!(preceded!(
  tag!("if"),
  expr
)));

named!(match_arm<&str, MatchArm>, map!(
  mrws!(tuple!(
    match_arm_pats,
    opt!(complete!(match_arm_guard))
  )),
  |(p, g)| MatchArm {
    pats: p,
    guard: g.map(Box::new),
  }
));

// TODO: make comma following block expr redundant
named!(match_body<&str, Vec<(MatchArm, Expr)>>, mrws!(terminated!(
  mrws!(separated_nonempty_list!(tag!(","), mrws!(separated_pair!(
    match_arm,
    tag!("=>"),
    expr
  )))),
  opt!(tag!(","))
)));

named!(match_expr<&str, Expr>, map!(
  mrws!(tuple!(
    tag!("match"),
    expr,
    tag!("{"),
    match_body,
    tag!("}")
  )),
  |(_, f, _, b, _)| Expr::Match {
    from: Box::new(f),
    arms: b,
  }
));

named!(error_propagation_expr<&str, Expr>, map!(mrws!(terminated!(
  t4_expr,
  tag!("!")
)), |e| Expr::Question(Box::new(e))));

named!(borrow_expr<&str, Expr>, map!(mrws!(tuple!(
  tag!("&"),
  opt!(complete!(tag!("mut"))),
  t5_expr
)), |(_, m, e)| Expr::Borrow {
  cont: Box::new(e),
  is_mut: m.is_some(),
}));

named!(deref_expr<&str, Expr>, map!(mrws!(preceded!(
  tag!("*"),
  t5_expr
)), |e| Expr::Deref(Box::new(e))));

named!(neg_expr<&str, Expr>, map!(mrws!(preceded!(
  tag!("-"),
  t5_expr
)), |e| Expr::Neg(Box::new(e))));

named!(not_expr<&str, Expr>, map!(mrws!(preceded!(
  tag!("!"),
  t5_expr
)), |e| Expr::Not(Box::new(e))));

named!(type_cast_expr<&str, Expr>, map!(mrws!(separated_pair!(
  t6_expr,
  tag!("as"),
  type_no_bound
)), |(e, t)| Expr::Cast {
  value: Box::new(e),
  to: t,
}));

// TODO: split this to confirm assoc
named!(arith_op_t1<&str, ArithOp>, 
  map!(one_of!("*/%"), |v| ArithOp::from_char(v))
);

named!(arith_op_t2<&str, ArithOp>, 
  map!(one_of!("+-"), |v| ArithOp::from_char(v))
);

named!(arith_op_t3<&str, ArithOp>, 
  map!(alt!(tag!("<<") | tag!(">>")), |v| ArithOp::from(v))
);

named!(arith_op_t4<&str, ArithOp>, 
  map!(tag!("&"), |_| ArithOp::BitAnd)
);

named!(arith_op_t5<&str, ArithOp>, 
  map!(tag!("^"), |_| ArithOp::BitXor)
);

named!(arith_op_t6<&str, ArithOp>, 
  map!(tag!("|"), |_| ArithOp::BitOr)
);

named!(logical_op_t1<&str, LogicalOp>,
  map!(alt!(
      tag!("==")
    | tag!("!=")
    | tag!("<")
    | tag!(">")
    | tag!("<=")
    | tag!(">=")
  ), |v| LogicalOp::from(v))
);

named!(logical_op_t2<&str, LogicalOp>,
  map!(tag!("&&"), |_| LogicalOp::And)
);

named!(logical_op_t3<&str, LogicalOp>,
  map!(tag!("||"), |_| LogicalOp::Or)
);

// Don't use mrws: we need this to be a single token
named!(compound_assign<&str, ArithOp>, terminated!(alt!(
    arith_op_t1
  | arith_op_t2
  | arith_op_t3
  | arith_op_t4
  | arith_op_t5
  | arith_op_t6
), tag!("=")));

named!(compound_assign_expr<&str, Expr>, map!(mrws!(tuple!(
  t8_expr,
  alt!(map!(compound_assign, Some) | map!(tag!("="), |_| None)),
  logical_t3_expr
)), |(l, o, r)| Expr::CompoundAssign {
  op: o,
  lhs: Box::new(l),
  rhs: Box::new(r),
}));
