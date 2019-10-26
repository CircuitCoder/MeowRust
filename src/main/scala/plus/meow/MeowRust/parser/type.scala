package plus.meow.MeowRust.parser

import plus.meow.MeowRust.grammar
import plus.meow.MeowRust.grammar._

trait Type extends SeparatedParsers with Identifier {
  val EXPRESSION: Parser[Expr]

  lazy val SIMPLE_PATH: Parser[SimplePath] = ("::"?) ~? SIMPLE_PATH_SEGMENT ~? (("::" ~>? SIMPLE_PATH_SEGMENT)*?) ^^ {
    (root, first, after) => SimplePath(root.isDefined, first :: after)
  }
  lazy val SIMPLE_PATH_SEGMENT: Parser[PathSeg] = (
      IDENTIFIER ^^ IdentSeg
    | Keyword.MAP("KW_SUPER") ^^^ { SuperSeg() }
    | Keyword.MAP("KW_SELF") ^^^ { SelfSeg() }
  )

  lazy val PATH_IDENT_SEG: Parser[PathSeg] = (
      IDENTIFIER ^^ IdentSeg
    | Keyword.MAP("KW_SUPER") ^^^ { SuperSeg() }
    | Keyword.MAP("KW_SELFVALUE") ^^^ { SelfSeg() }
    | Keyword.MAP("KW_SELFTYPE") ^^^ { SelfTypeSeg() }
  )

  lazy val GENERIC_ARGS: Parser[GenericArgs] = (
      "<" ~? ">" ^^^ { GenericArgs() }
    | "<" ~>? GENERIC_ARGS_TYPES <~? (","?) <~? ">" ^^ { GenericArgs(_) }
    | "<" ~>? GENERIC_ARGS_BINDINGS <~? (","?) <~? ">" ^^ { GenericArgs(List(), _) }
    | ("<" ~>? GENERIC_ARGS_TYPES <~? ",") ~? GENERIC_ARGS_BINDINGS <~? (","?) <~? ">" ^^ { GenericArgs(_, _) }
  )

  lazy val GENERIC_ARGS_TYPES: Parser[List[grammar.Type]] = TYPE ~? (("," ~>? TYPE)*?) ^^ { _ :: _ }
  lazy val GENERIC_ARGS_BINDINGS: Parser[Map[String, grammar.Type]] = GENERIC_ARGS_BINDING ~? (("," ~>? GENERIC_ARGS_BINDING)*?) ^^ { (f, t) => (f :: t).map(p => p._1 -> p._2).toMap }
  lazy val GENERIC_ARGS_BINDING: Parser[(String, grammar.Type)] = (IDENTIFIER <~? "=") ~? TYPE ^^ { (_, _) }

  lazy val PATH_IN_EXPR: Parser[PathInExpression] = ("::"?) ~? PATH_EXPR_SEG ~? (("::" ~>? PATH_EXPR_SEG)*?) ^^ { (r, f, l) => PathInExpression(r.isDefined, f :: l) }
  lazy val PATH_EXPR_SEG: Parser[(PathSeg, GenericArgs)] = PATH_IDENT_SEG ~? (("::" ~>? GENERIC_ARGS)?) ^^ { (p, a) => (p, a getOrElse GenericArgs()) }

  lazy val TYPE_PATH: Parser[TypePath] = (("::"?) ~? TYPE_PATH_SEGMENT) ~? (("::" ~>? TYPE_PATH_SEGMENT)*?) ^^ { (r, f, l) => TypePath(r.isDefined, f :: l) }
  lazy val TYPE_PATH_SEGMENT: Parser[(PathSeg, TypeSegArg)] = (PATH_IDENT_SEG <~? ("::"?)) ~? (GENERIC_ARGS?) ^^ { (p, a) => (p, NonFnTypeArg(a getOrElse GenericArgs())) }

  // TODO: bounded type
  // TODO: QualifiedPathInType
  lazy val TYPE = TYPE_NO_BOUND
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
