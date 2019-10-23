package plus.meow.MeowRust.parser
import com.codecommit.gll.RegexParsers
import plus.meow.MeowRust.grammar._
import plus.meow.MeowRust.grammar

trait Statement extends RegexParsers with Literal with Pattern with Identifier with Label {
  lazy val STATEMENT: Parser[Stmt] = (
      ";" ^^^ { EmptyStmt() }
    // | ITEM // TODO: impl
    | LET_STATEMENT
    | EXPRESSION_STATEMENT
  )

  // TODO: support type ascriptions
  lazy val LET_STATEMENT: Parser[LetStmt] = "let" ~> PATTERN ~ (("=" ~> EXPRESSION)?) ^^ LetStmt
  lazy val EXPRESSION_STATEMENT: Parser[ExprStmt] = (
      EXPRESSION_WITHOUT_BLOCK <~? ";"
    | EXPRESSION_WITH_BLOCK
  ) ^^ ExprStmt

  lazy val EXPRESSION: Parser[Expr] = EXPRESSION_WITH_BLOCK | EXPRESSION_WITHOUT_BLOCK
  lazy val EXPRESSION_WITHOUT_BLOCK: Parser[Expr] = (
      LITERAL_EXPRESSION
    | TUPLE_EXPRESSION
    | GROUPED_EXPRESSION
    | CALL_EXPRESSION
    | METHOD_CALL_EXPRESSION
    | FIELD_EXPRESSION
    // | PATH_EXPRESSION // TODO: impl
    | OPERATOR_EXPRESSION
    | ARRAY_EXPRESSION
    | INDEX_EXPRESSION
    | TUPLE_INDEXING_EXPRESSION
    // | STRUCT_EXPRESSION // TODO: impl
    // | ENUMERATION_VARIANT_EXPRESSION // TODO: impl
    | CLOSURE_EXPRESSION
    | CONTINUE_EXPRESSION
    | BREAK_EXPRESSION
    | RANGE_EXPRESSION
    | RETURN_EXPRESSION
  )
  lazy val EXPRESSION_WITH_BLOCK: Parser[Expr] = (
      BLOCK_EXPRESSION // We don't support unsafe context
    | LOOP_EXPRESSION
    | IF_EXPRESSION
    | IF_LET_EXPRESSION
    | MATCH_EXPRESSION
  )

  lazy val LITERAL_EXPRESSION: Parser[LiteralExpr] = LITERAL ^^ { LiteralExpr(_) }

  lazy val BLOCK_EXPRESSION: Parser[BlockExpr] = "{" ~>? (STATEMENTS?) <~? "}" ^^ { (l) => BlockExpr(l getOrElse List()) }
  lazy val STATEMENTS: Parser[List[Any]] = (
      (STATEMENT+) // Type is ()
    | (STATEMENT+) ~? EXPRESSION_WITHOUT_BLOCK ^^ { (s, e) => s :+ e } // Type is last expr
    | EXPRESSION_WITHOUT_BLOCK ^^ { List(_) } // Type is the expr
  )

  lazy val OPERATOR_EXPRESSION: Parser[Expr] = (
      BORROW_EXPRESSION
    | DEREFERENCE_EXPRESSION
    | ERROR_PROPAGATION_EXPRESSION
    | NEGATION_EXPRESSION
    | ARITHMETIC_OR_LOGICAL_EXPRESSION
    | COMPARISION_EXPRESSION
    | LAZY_BOOLEAN_EXPRESSION
    // | TYPE_CAST_EXPRESSION // TODO: impl
    | ASSIGNMENT_EXPRESSION
    | COMPOUND_ASSIGNMENT_EXPRESSION
  )

  // TODO: maybe we need two borrow signs to override &&?
  lazy val BORROW_EXPRESSION: Parser[BorrowExpr] = (
      "&" ~>? EXPRESSION ^^ { BorrowExpr(_, false) }
    | "&" ~? "mut" ~>! EXPRESSION ^^ { BorrowExpr(_, false) }
  )
  lazy val DEREFERENCE_EXPRESSION: Parser[DerefExpr] = "*" ~>? EXPRESSION ^^ { DerefExpr(_) }
  lazy val ERROR_PROPAGATION_EXPRESSION: Parser[QuestionExpr] = EXPRESSION <~? "?" ^^ { QuestionExpr(_) }
  // TODO: use partial applied functions....
  lazy val NEGATION_EXPRESSION: Parser[UnaryOpExpr] = (
      "-" ~>? EXPRESSION ^^ { UnaryOpExpr(Neg(), _) }
    | "!" ~>? EXPRESSION ^^ { UnaryOpExpr(LogicalNot(), _) }
  )

  // TODO: precedence
  lazy val ARITHMETIC_OR_LOGICAL_EXPRESSION: Parser[BinaryOpExpr] = (
      (EXPRESSION <~? "+") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(Add(), l, r) }
    | (EXPRESSION <~? "-") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(Sub(), l, r) }
    | (EXPRESSION <~? "*") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(Mul(), l, r) }
    | (EXPRESSION <~? "/") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(Div(), l, r) }
    | (EXPRESSION <~? "%") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(Mod(), l, r) }
    | (EXPRESSION <~? "&") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(BitwiseAnd(), l, r) }
    | (EXPRESSION <~? "|") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(BitwiseOr(), l, r) }
    | (EXPRESSION <~? "^") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(BitwiseXor(), l, r) }
    | (EXPRESSION <~? "<<") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(BitwiseLShift(), l, r) }
    | (EXPRESSION <~? ">>") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(BitwiseRShift(), l, r) }
  )

  lazy val COMPARISION_EXPRESSION: Parser[BinaryOpExpr] = (
      (EXPRESSION <~? "==") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(CmpEq(), l, r) }
    | (EXPRESSION <~? "!=") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(CmpNe(), l, r) }
    | (EXPRESSION <~? ">") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(CmpG(), l, r) }
    | (EXPRESSION <~? "<") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(CmpL(), l, r) }
    | (EXPRESSION <~? ">=") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(CmpGe(), l, r) }
    | (EXPRESSION <~? "<=") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(CmpLe(), l, r) }
  )

  lazy val LAZY_BOOLEAN_EXPRESSION: Parser[BinaryOpExpr] = (
      (EXPRESSION <~? "&&") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(LogicalAnd(), l, r) }
    | (EXPRESSION <~? "||") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(LogicalOr(), l, r) }
  )

  lazy val ASSIGNMENT_EXPRESSION: Parser[BinaryOpExpr] = (EXPRESSION <~? "=") ~? EXPRESSION ^^ { (l, r) => BinaryOpExpr(Assign(), l, r) }

  lazy val COMPOUND_ASSIGNMENT_EXPRESSION: Parser[BinaryOpExpr] = (
      (EXPRESSION <~ "+=") ~ EXPRESSION ^^ { (l, r) => BinaryOpExpr(AddAssign(), l, r) }
    | (EXPRESSION <~ "-=") ~ EXPRESSION ^^ { (l, r) => BinaryOpExpr(SubAssign(), l, r) }
    | (EXPRESSION <~ "*=") ~ EXPRESSION ^^ { (l, r) => BinaryOpExpr(MulAssign(), l, r) }
    | (EXPRESSION <~ "/=") ~ EXPRESSION ^^ { (l, r) => BinaryOpExpr(DivAssign(), l, r) }
    | (EXPRESSION <~ "%=") ~ EXPRESSION ^^ { (l, r) => BinaryOpExpr(ModAssign(), l, r) }
    | (EXPRESSION <~ "&=") ~ EXPRESSION ^^ { (l, r) => BinaryOpExpr(BitwiseAndAssign(), l, r) }
    | (EXPRESSION <~ "|=") ~ EXPRESSION ^^ { (l, r) => BinaryOpExpr(BitwiseOrAssign(), l, r) }
    | (EXPRESSION <~ "^=") ~ EXPRESSION ^^ { (l, r) => BinaryOpExpr(BitwiseXorAssign(), l, r) }
    | (EXPRESSION <~ "<<=") ~ EXPRESSION ^^ { (l, r) => BinaryOpExpr(BitwiseLShiftAssign(), l, r) }
    | (EXPRESSION <~ ">>=") ~ EXPRESSION ^^ { (l, r) => BinaryOpExpr(BitwiseRShiftAssign(), l, r) }
  )

  lazy val GROUPED_EXPRESSION: Parser[Expr] = "(" ~>? EXPRESSION <~ ")"

  lazy val ARRAY_EXPRESSION: Parser[Expr] = "[" ~>? ARRAY_ELEMENTS <~? "]"
  lazy val ARRAY_ELEMENTS = (
      EXPRESSION ~? (("," ~>? EXPRESSION) *) <~? (","?) ^^ { (e, l) => ArrayExpr(e :: l) }
    | (EXPRESSION <~? ";") ~? EXPRESSION ^^ ArrayFillExpr
  )
  lazy val INDEX_EXPRESSION: Parser[ArrayIndexExpr] = (EXPRESSION <~? "[") ~? EXPRESSION <~? "]" ^^ ArrayIndexExpr
  
  lazy val TUPLE_EXPRESSION: Parser[TupleExpr] = "(" ~>? TUPLE_ELEMENTS <~? ")" ^^ TupleExpr
  lazy val TUPLE_ELEMENTS: Parser[List[Expr]] = ((EXPRESSION <~? ",")*) ~? (EXPRESSION?) ^^ { (l, e) => e match {
    case None => l
    case Some(value) => l :+ value
  }}
  lazy val TUPLE_INDEXING_EXPRESSION: Parser[TupleIndexExpr] = (EXPRESSION <~? ".") ~? TUPLE_INDEX ^^ TupleIndexExpr

  lazy val CALL_EXPRESSION: Parser[CallExpr] = (EXPRESSION <~? "(") ~? CALL_PARAMS <~? ")" ^^ { CallExpr(_, None, _) }
  lazy val CALL_PARAMS: Parser[List[Expr]] = EXPRESSION ~? (("," ~>? EXPRESSION)*) <~? (","?) ^^ { _ :: _ }

  // TODO: use path instead of identifier
  lazy val METHOD_CALL_EXPRESSION = (EXPRESSION <~? ".") ~? (IDENTIFIER <~? "(") ~? CALL_PARAMS <~? ")" ^^ { (recv, method, params) => CallExpr(recv, Some(method), params) }

  lazy val FIELD_EXPRESSION = (EXPRESSION <~? ".") ~? EXPRESSION ^^ FieldExpr

  // TODO: add type ascription ( -> TYPE BlockExpression
  lazy val CLOSURE_EXPRESSION = (("||" ^^^ { List() }) | "|" ~>? CLOSURE_PARAMETERS <~? "|") ~? EXPRESSION ^^ ClosureExpr
  lazy val CLOSURE_PARAMETERS: Parser[List[grammar.Pattern]] = CLOSURE_PARAM ~? (("," ~>? CLOSURE_PARAM)*) <~? (","?) ^^ { _ :: _ }
  // TODO: add type ascription
  lazy val CLOSURE_PARAM = PATTERN

  lazy val LOOP_EXPRESSION: Parser[Expr] = (LOOP_LABEL?) ~ (
      "loop" ~>? BLOCK_EXPRESSION ^^ { b => l: Option[String] => InftyLoopExpr(l, b) }
    | "while" ~>! EXPRESSION ~? BLOCK_EXPRESSION ^^ { (c, b) => l: Option[String] => WhileExpr(l, c, b) }
    | (("while" ~! "let") ~>! MATCH_ARM_PATTERNS <~? "=") ~? EXPRESSION ~? BLOCK_EXPRESSION ^^ { (p, u, b) => l: Option[String] => WhileLetExpr(l, p, u, b) }
    | ("for" ~>! PATTERN <~! "in") ~! EXPRESSION ~? BLOCK_EXPRESSION ^^ { (p, c, b) => l: Option[String] => ForLoopExpr(l, p, c, b) }
  ) ^^ { (l, f) => f(l) }
  lazy val LOOP_LABEL = LIFETIME_OR_LABEL <~ ":"
  lazy val BREAK_EXPRESSION: Parser[FlowCtrlExpr] = "break" ~>! (LIFETIME_OR_LABEL?) ~! (EXPRESSION?) ^^ { FlowCtrlExpr(Break(), _, _) }
  lazy val CONTINUE_EXPRESSION = "continue" ~>! (LIFETIME_OR_LABEL?) ^^ { FlowCtrlExpr(Continue(), _, None) }

  lazy val RANGE_EXPRESSION: Parser[RangeExpr] = (
      (EXPRESSION <~? "..") ~? EXPRESSION ^^ { (f, t) => RangeExpr(Some(f), Some(t), false) }
    | (EXPRESSION <~? "..=") ~? EXPRESSION ^^ { (f, t) => RangeExpr(Some(f), Some(t), true) }
    | EXPRESSION <~? ".." ^^ { f => RangeExpr(Some(f), None, false) }
    | ".." ~>? EXPRESSION ^^ { t => RangeExpr(None, Some(t), false) }
    | "..=" ~>? EXPRESSION ^^ { t => RangeExpr(None, Some(t), true) }
    | ".." ^^ { t => RangeExpr(None, None, false) }
  )

  lazy val IF_EXPRESSION: Parser[IfExpr] = "if" ~>! EXPRESSION ~? BLOCK_EXPRESSION ~? (ELSE_ARM?) ^^ IfExpr
  lazy val ELSE_ARM: Parser[Expr] = "else" ~>? (BLOCK_EXPRESSION | IF_EXPRESSION | IF_LET_EXPRESSION)
  lazy val IF_LET_EXPRESSION: Parser[IfLetExpr] = (("if" ~! "let") ~>! MATCH_ARM_PATTERNS <~? "=") ~? EXPRESSION ~? BLOCK_EXPRESSION ~(ELSE_ARM?) ^^ { IfLetExpr(_, _, _, _) }

  lazy val MATCH_EXPRESSION: Parser[MatchExpr] = ("match" ~>! EXPRESSION <~? "{") ~? (MATCH_ARMS?) <~? "}" ^^ { (e, arms) => MatchExpr(e, arms getOrElse List()) }
  val armParser = (spec: (List[grammar.Pattern], Option[Expr]), body: Expr) => MatchArm(spec._1, spec._2, body)
  lazy val MATCH_ARMS: Parser[List[MatchArm]] = (
    (
      (MATCH_ARM <~? "=>") ~? ((BLOCK_EXPRESSION <~? (","?)) | (EXPRESSION <~? ",")) ^^ armParser)*
    ) ~ (
      (MATCH_ARM <~? "=>") ~? (BLOCK_EXPRESSION | EXPRESSION) <~? (","?) ^^ armParser
    ) ^^ {
      _ :+ _
    }
  lazy val MATCH_ARM: Parser[(List[grammar.Pattern], Option[Expr])] = MATCH_ARM_PATTERNS ~ ((whitespace ~> "if" ~>! EXPRESSION)?) ^^ { (_, _) }
  lazy val MATCH_ARM_PATTERNS: Parser[List[grammar.Pattern]] = ("|"?) ~>? PATTERN ~? (("|" ~>? PATTERN)*) ^^ { (e, l) => e :: l }

  lazy val RETURN_EXPRESSION = "return" ~>! (EXPRESSION?) ^^ { FlowCtrlExpr(Return(), None, _) }
}
