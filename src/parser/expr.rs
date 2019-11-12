use nom::{
  named, map, alt, complete, tag, opt, many0, terminated, separated_nonempty_list, value, one_of,
  call,
  IResult,
};

use super::literal::literal;
use crate::grammar::*;
use super::stmt::stmt;
use super::ident::*;
use super::literal::tuple_idx;
use super::path::path_in_expr;
use super::pattern::pat;
use super::r#type::type_no_bound;

named!(pub t1_expr<&str, Expr>, alt!(
    grouped_expr
  | map!(literal, Expr::Literal) // literal expr
  | tuple_expr
  | array_expr
));

named!(pub t2_expr<&str, Expr>, alt!(
    map!(path_in_expr, Expr::Path) // Path expr
  | t1_expr
));

// T2 falls through field/tuple idx
named!(pub t3_expr<&str, Expr>, call!(t3_full));

// T3 falls through call/array idx
named!(pub t4_expr<&str, Expr>, call!(t4_full));

// T4 falls through type_cast_expr
named!(pub t5_expr<&str, Expr>, call!(error_propagation_expr));

named!(pub t6_expr<&str, Expr>, alt!(
    borrow_expr
  | deref_expr
  | neg_expr
  | not_expr

  | t5_expr
));

// T6 falls through type_cast_expr
named!(pub t7_expr_inner<&str, Expr>, call!(type_cast_expr));

pub fn t7_expr(input: &str) -> IResult<&str, Expr> {
  println!("Parsing: {}", input);
  t7_expr_inner(input)
}

macro_rules! fall_through_expr {
  ($i:expr, $first:tt, $tail:tt, $mapper:expr) => ({
    map!(
      $i,
      mrws!(tuple!(
        $first, opt!(complete!($tail))
      )),
      |(f, b)| match b {
        None => f,
        Some(bc) => $mapper(f, bc),
      }
    )
  })
}

macro_rules! bin_ltr_expr {
  ($cur:ident, $op_parser:ident, $lower:ident) => {
    pub fn $cur(input: &str) -> IResult<&str, Expr> {
      let (sliced, init) = $lower(input)?;

      use nom::{fold_many0};

      fold_many0!(
        sliced,
        complete!(mrws!(tuple!($op_parser, $lower))),
        init,
        |acc, (op, term)| Expr::BinaryOp {
          lhs: Box::new(acc),
          rhs: Box::new(term),

          op: op.into(),
        }
      )
    }
  }
}

macro_rules! bin_rtl_expr {
  ($cur:ident, $op_parser:ident, $lower:ident) => {
    pub fn $cur(input: &str) -> IResult<&str, Expr> {
      dbg!(input);
      let (sliced, init) = $lower(input)?;

      use nom::{fold_many0};

      let (left, terms) = many0!(
        sliced,
        complete!(mrws!(tuple!($op_parser, $lower)))
      )?;

      let ret = match terms.into_iter().rev().fold(None, |acc, (op, term)| {
        match acc {
          None => Some((op, term)),
          Some((cop, dec)) => Some((op, Expr::BinaryOp {
            op: cop.into(),
            lhs: Box::new(term),
            rhs: Box::new(dec),
          }))
        }
      }) {
        None => init,
        Some((cop, dec)) => Expr::BinaryOp {
          op: cop.into(),
          lhs: Box::new(init),
          rhs: Box::new(dec),
        }
      };

      Ok((left, ret))
    }
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

// logical_t3_expr falls through compound_assign_expr
named!(pub t8_expr<&str, Expr>, call!(compound_assign_expr));
bin_rtl_expr!(compound_assign_expr, compound_assign, logical_t3_expr);

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

#[derive(Debug)]
enum T3Args<'a> {
  TupleIdx(u128),
  Field(&'a str),
}

impl<'a> T3Args<'a> {
  fn finalize(self, base: Expr<'a>) -> Expr<'a> {
    match self {
      Self::TupleIdx(idx) => Expr::TupleIndex {
        owner: Box::new(base),
        idx,
      },
      Self::Field(field) => Expr::Field {
        owner: Box::new(base),
        field,
      }
    }
  }
}

pub fn t3_full(input: &str) -> IResult<&str, Expr> {
  let (sliced, init) = t2_expr(input)?;

  use nom::{fold_many0};

  fold_many0!(
    sliced,
    complete!(mrws!(preceded!(tag!("."), t3_seg))),
    init,
    |acc, seg| seg.finalize(acc)
  )
}

named!(t3_seg<&str, T3Args>, alt!(
    complete!(map!(tuple_idx, T3Args::TupleIdx))
  | complete!(map!(ident, T3Args::Field))
));

#[derive(Debug)]
enum T4Args<'a> {
  ArrayIdx(Box<Expr<'a>>),
  Call(Option<&'a str>, Vec<Expr<'a>>),
}

impl<'a> T4Args<'a> {
  fn finalize(self, base: Expr<'a>) -> Expr<'a> {
    match self {
      Self::ArrayIdx(idx) => Expr::ArrayIndex {
        owner: Box::new(base),
        idx: idx,
      },
      Self::Call(method, params) => Expr::Call {
        recv: Box::new(base),
        method,
        params,
      }
    }
  }
}

fn t4_full<'a>(input: &'a str) -> IResult<&'a str, Expr<'a>> {
  fall_through_expr!(
    input,
    t3_expr,
    t4_tail,
    |b, t: T4Args<'a>| t.finalize(b)
  )
}

named!(t4_tail<&str, T4Args>, alt!(
  map!(
    mrws!(tuple!(
      opt!(complete!(mrws!(preceded!(
        tag!("."),
        ident
      )))),
      complete!(tag!("(")),
      call_params,
      complete!(tag!(")"))
    )),
    |(method, _, params, _)| T4Args::Call(method, params)
  )
  |
  map!(mrws!(delimited!(
    complete!(tag!("[")),
    expr,
    complete!(tag!("]"))
  )), |e| T4Args::ArrayIdx(Box::new(e)))
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

named!(error_propagation_expr<&str, Expr>, map!(mrws!(tuple!(
  t4_expr,
  opt!(complete!(tag!("?")))
)), |(e, q)| match q {
  Some(_) => Expr::Question(Box::new(e)),
  None => e,
}));

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

named!(type_cast_tail<&str, Type>, 
  mrws!(preceded!(
    tag!("as"),
    type_no_bound
  ))
);

named!(type_cast_expr<&str, Expr>, fall_through_expr!(
  t6_expr,
  type_cast_tail,
  |e, t| Expr::Cast {
    value: Box::new(e),
    to: t,
  }
));
  
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
