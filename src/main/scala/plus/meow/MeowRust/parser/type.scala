package plus.meow.MeowRust.parser

import plus.meow.MeowRust.grammar
import plus.meow.MeowRust.grammar._

trait Type extends SeparatedParsers with Identifier with Path {
  val EXPRESSION: Parser[Expr]

  // TODO: bounded type
  // TODO: QualifiedPathInType
  override lazy val TYPE = TYPE_NO_BOUND
  lazy val TYPE_NO_BOUND: Parser[grammar.Type] = (
      "(" ~>? TYPE <~? ")"
    | TYPE_PATH ^^ IdentType
    | TUPLE_TYPE
    | "!" ^^^ { NeverType() }
    | "&" ~>? ("mut"?) ~! TYPE_NO_BOUND ^^ { (mut, t) => RefType(t, mut.isDefined) }
    | ((("[" ~>? TYPE) <~? ";") ~? EXPRESSION) <~? "]" ^^ { (t, l) => ArrayType(t, l) }
    | "[" ~>? TYPE <~? "]" ^^ SliceType
    | "_" ^^^ { InferredType() }
    | BARE_FUNCTION_TYPE
  )

  lazy val TUPLE_TYPE: Parser[TupleType] = (
      "(" ~? ")" ^^^ { TupleType(List()) }
    | "(" ~>? (TYPE <~? ",") <~? ")" ^^ { t => TupleType(List(t)) }
    | ("(" ~>? TYPE) ~ (("," ~>? TYPE)*?) <~? (","?) <~? ")" ^^ { (f, l) => TupleType(f :: l) }
  )

  // TODO: spec says that params in bare func types can have names (but why?)
  lazy val BARE_FUNCTION_TYPE: Parser[FuncType] = ("fn" ~>? "(" ~>? BARE_FUNCTION_PARAMS <~? ")") ~? (BARE_FUNCTION_RET?) ^^ { (a, r) => FuncType(FnTypeArg(a, r)) }
  lazy val BARE_FUNCTION_PARAMS: Parser[List[grammar.Type]] = TYPE ~? (("," ~>? TYPE )*?) <~? (","?) ^^ { _ :: _ }
  lazy val BARE_FUNCTION_RET: Parser[grammar.Type] = "->" ~>? TYPE
}
