package plus.meow.MeowRust.parser
import com.codecommit.gll.RegexParsers

trait Statement extends RegexParsers with Literal with Pattern with Identifier with Label {
  lazy val STATEMENT: Parser[Any] = (
      ";"
    // | ITEM // TODO: impl
    | LET_STATEMENT
    | EXPRESSION_STATEMENT
  )

  // TODO: support type ascriptions
  lazy val LET_STATEMENT: Parser[Any] = "let" ~> PATTERN ~ (("=" ~> EXPRESSION)?)
  lazy val EXPRESSION_STATEMENT: Parser[Any] = (
      EXPRESSION_WITHOUT_BLOCK <~ ";"
    | EXPRESSION_WITH_BLOCK
  )

  lazy val EXPRESSION: Parser[Any] = EXPRESSION_WITH_BLOCK | EXPRESSION_WITHOUT_BLOCK
  lazy val EXPRESSION_WITHOUT_BLOCK: Parser[Any] = (
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
  lazy val EXPRESSION_WITH_BLOCK: Parser[Any] = (
      BLOCK_EXPRESSION // We don't support unsafe context
    | LOOP_EXPRESSION
    | IF_EXPRESSION
    | IF_LET_EXPRESSION
    | MATCH_EXPRESSION
  )

  lazy val LITERAL_EXPRESSION: Parser[Any] = LITERAL

  lazy val BLOCK_EXPRESSION: Parser[Any] = "{" ~> (STATEMENTS?) <~ "}"
  lazy val STATEMENTS: Parser[Any] = (
      (STATEMENT+) // Type is ()
    | (STATEMENT+) ~ EXPRESSION_WITHOUT_BLOCK // Type is last expr
    | EXPRESSION_WITHOUT_BLOCK // Type is the expr
  )

  lazy val OPERATOR_EXPRESSION: Parser[Any] = (
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

  lazy val BORROW_EXPRESSION: Parser[Any] = "&&?(mut)?" ~ EXPRESSION // Second & is to override boolean &&
  lazy val DEREFERENCE_EXPRESSION: Parser[Any] = "*" ~> EXPRESSION
  lazy val ERROR_PROPAGATION_EXPRESSION: Parser[Any] = EXPRESSION <~ "?"
  lazy val NEGATION_EXPRESSION: Parser[Any] = (
      "-" ~> EXPRESSION
    | "!" ~> EXPRESSION
  )

  // TODO: precedence
  lazy val ARITHMETIC_OR_LOGICAL_EXPRESSION: Parser[Any] = (
      EXPRESSION <~ "+" ~> EXPRESSION
    | EXPRESSION <~ "-" ~> EXPRESSION
    | EXPRESSION <~ "*" ~> EXPRESSION
    | EXPRESSION <~ "/" ~> EXPRESSION
    | EXPRESSION <~ "%" ~> EXPRESSION
    | EXPRESSION <~ "&" ~> EXPRESSION
    | EXPRESSION <~ "|" ~> EXPRESSION
    | EXPRESSION <~ "^" ~> EXPRESSION
    | EXPRESSION <~ "<<" ~> EXPRESSION
    | EXPRESSION <~ ">>" ~> EXPRESSION
  )

  lazy val COMPARISION_EXPRESSION: Parser[Any] = (
      EXPRESSION <~ "==" ~> EXPRESSION
    | EXPRESSION <~ "!=" ~> EXPRESSION
    | EXPRESSION <~ ">" ~> EXPRESSION
    | EXPRESSION <~ "<" ~> EXPRESSION
    | EXPRESSION <~ ">=" ~> EXPRESSION
    | EXPRESSION <~ "<=" ~> EXPRESSION
  )

  lazy val LAZY_BOOLEAN_EXPRESSION: Parser[Any] = (
      EXPRESSION <~ "&&" ~> EXPRESSION
    | EXPRESSION <~ "||" ~> EXPRESSION
  )

  lazy val ASSIGNMENT_EXPRESSION: Parser[Any] = EXPRESSION <~ "=" ~> EXPRESSION

  lazy val COMPOUND_ASSIGNMENT_EXPRESSION: Parser[Any] = (
      EXPRESSION <~ "+=" ~> EXPRESSION
    | EXPRESSION <~ "-=" ~> EXPRESSION
    | EXPRESSION <~ "*=" ~> EXPRESSION
    | EXPRESSION <~ "/=" ~> EXPRESSION
    | EXPRESSION <~ "%=" ~> EXPRESSION
    | EXPRESSION <~ "&=" ~> EXPRESSION
    | EXPRESSION <~ "|=" ~> EXPRESSION
    | EXPRESSION <~ "^=" ~> EXPRESSION
    | EXPRESSION <~ "<<=" ~> EXPRESSION
    | EXPRESSION <~ ">>=" ~> EXPRESSION
  )

  lazy val GROUPED_EXPRESSION: Parser[Any] = "(" ~> EXPRESSION <~ ")"

  lazy val ARRAY_EXPRESSION: Parser[Any] = "[" ~> ARRAY_ELEMENTS <~ "]"
  lazy val ARRAY_ELEMENTS = (
      EXPRESSION ~ ((EXPRESSION <~ ",") *) <~ (","?)
    | EXPRESSION <~ ";" ~ EXPRESSION
  )
  lazy val INDEX_EXPRESSION = EXPRESSION <~ "[" ~ EXPRESSION ~> "]"
  
  lazy val TUPLE_EXPRESSION = "(" ~> TUPLE_ELEMENTS <~ ")"
  lazy val TUPLE_ELEMENTS = ((EXPRESSION <~ ",")*) ~ (EXPRESSION?)
  lazy val TUPLE_INDEXING_EXPRESSION = EXPRESSION <~ "." ~> TUPLE_INDEX

  lazy val CALL_EXPRESSION = EXPRESSION <~ "(" ~ CALL_PARAMS <~ ")"
  lazy val CALL_PARAMS = EXPRESSION ~ (("," ~> EXPRESSION)*) <~ (","?)

  // TODO: use path instead of identifier
  lazy val METHOD_CALL_EXPRESSION = EXPRESSION <~ "." ~ IDENTIFIER <~ "(" ~ CALL_PARAMS <~ ")"

  lazy val FIELD_EXPRESSION = EXPRESSION <~ "." ~ EXPRESSION

  // TODO: add type ascription ( -> TYPE BlockExpression
  lazy val CLOSURE_EXPRESSION = ("||" | "|" ~> CLOSURE_PARAMETERS <~ "|") ~ EXPRESSION
  lazy val CLOSURE_PARAMETERS = CLOSURE_PARAM ~ (("," ~> CLOSURE_PARAM)*) <~ (","?)
  // TODO: add type ascription
  lazy val CLOSURE_PARAM = PATTERN

  lazy val LOOP_EXPRESSION = (LOOP_LABEL?) ~ (
      "loop" ~> BLOCK_EXPRESSION
    | "while" ~> EXPRESSION ~ BLOCK_EXPRESSION
    | ("while" ~ "let") ~> MATCH_ARM_PATTERNS <~ "=" ~ EXPRESSION ~ BLOCK_EXPRESSION
    | "for" ~> PATTERN <~ "in" ~ EXPRESSION ~ BLOCK_EXPRESSION
  )
  lazy val LOOP_LABEL = LIFETIME_OR_LABEL <~ ":"
  lazy val BREAK_EXPRESSION = "break" ~ (LIFETIME_OR_LABEL?) ~ (EXPRESSION?)
  lazy val CONTINUE_EXPRESSION = "continue" ~ (LIFETIME_OR_LABEL?)

  lazy val RANGE_EXPRESSION = (
      EXPRESSION <~ ".." ~> EXPRESSION
    | EXPRESSION <~ "..=" ~> EXPRESSION
    | EXPRESSION <~ ".."
    | ".." ~> EXPRESSION
    | "..=" ~> EXPRESSION
    | ".."
  )

  lazy val IF_EXPRESSION: Parser[Any] = "if" ~> EXPRESSION ~ BLOCK_EXPRESSION ~ (ELSE_ARM?)
  lazy val ELSE_ARM = "else" ~> (BLOCK_EXPRESSION | IF_EXPRESSION /* | IF_LET_EXPRESSION */)
  lazy val IF_LET_EXPRESSION = ("if" ~ "let") ~> MATCH_ARM_PATTERNS <~ "=" ~ EXPRESSION ~ BLOCK_EXPRESSION ~(ELSE_ARM?)

  lazy val MATCH_EXPRESSION = "match" ~> EXPRESSION <~ "{" ~ (MATCH_ARMS?) <~ "}"
  lazy val MATCH_ARMS = (MATCH_ARM <~ "=>" ~ (
    (BLOCK_EXPRESSION <~ (","?)) | EXPRESSION <~ ","
  )*) ~ MATCH_ARM <~ "=>" ~ (BLOCK_EXPRESSION | EXPRESSION) <~ (","?)
  lazy val MATCH_ARM = MATCH_ARM_PATTERNS ~ (("if" ~> EXPRESSION)?)
  lazy val MATCH_ARM_PATTERNS = ("|"?) ~> PATTERN ~ (("|" ~> PATTERN)*)

  lazy val RETURN_EXPRESSION = "return" ~> EXPRESSION?
}