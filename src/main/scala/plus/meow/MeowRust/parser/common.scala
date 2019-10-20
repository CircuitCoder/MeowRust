package plus.meow.MeowRust.parser
import com.codecommit.gll.RegexParsers

trait Keyword extends RegexParsers {
  override val skipWhitespace: Boolean = false

  lazy val KW_AS: RegexParser = "as".r
  lazy val KW_BREAK: RegexParser = "break".r
  lazy val KW_CONST: RegexParser = "const".r
  lazy val KW_CONTINUE: RegexParser = "continue".r
  // No supports for crate.r
  lazy val KW_ELSE: RegexParser = "else".r
  lazy val KW_ENUM: RegexParser = "enum".r
  // No supports for extern.r
  lazy val KW_FALSE: RegexParser = "false".r
  lazy val KW_FN: RegexParser = "fn".r
  lazy val KW_FOR: RegexParser = "for".r
  lazy val KW_IF: RegexParser = "if".r
  lazy val KW_IMPL: RegexParser = "impl".r
  lazy val KW_IN: RegexParser = "in".r
  lazy val KW_LET: RegexParser = "let".r
  lazy val KW_LOOP: RegexParser = "loop".r
  lazy val KW_MATCH: RegexParser = "match".r
  lazy val KW_MOD: RegexParser = "mod".r
  // No supports for move.r
  lazy val KW_MUT: RegexParser = "mut".r
  // No supports for pub.r
  lazy val KW_REF: RegexParser = "ref".r
  lazy val KW_RETURN: RegexParser = "return".r
  lazy val KW_SELFVALUE: RegexParser = "self".r
  lazy val KW_SELFTYPE: RegexParser = "Self".r
  lazy val KW_STATIC: RegexParser = "static".r
  lazy val KW_SUPER: RegexParser = "super".r // TODO: what is this?
  lazy val KW_TRAIT: RegexParser = "trait".r
  lazy val KW_TRUE: RegexParser = "true".r
  lazy val KW_TYPE: RegexParser = "type".r
  lazy val KW_UNSAFE: RegexParser = "unsafe".r // MeowRust don't differentiate safe and unsafe context
  lazy val KW_USE: RegexParser = "use".r
  lazy val KW_WHERE: RegexParser = "where".r
  lazy val KW_WHILE: RegexParser = "while".r

  // We don't care about reserved keywords

  lazy val KEYWORD: TerminalParser[String] = (
      KW_AS
    | KW_BREAK
    | KW_CONST
    | KW_CONTINUE
    | KW_ELSE
    | KW_ENUM
    | KW_FALSE
    | KW_FN
    | KW_FOR
    | KW_IF
    | KW_IMPL
    | KW_IN
    | KW_LET
    | KW_LOOP
    | KW_MATCH
    | KW_MOD
    | KW_MUT
    | KW_REF
    | KW_RETURN
    | KW_SELFVALUE
    | KW_SELFTYPE
    | KW_STATIC
    | KW_SUPER
    | KW_TRAIT
    | KW_TRUE
    | KW_TYPE
    | KW_UNSAFE
    | KW_USE
    | KW_WHERE
    | KW_WHILE
  ).asInstanceOf[TerminalParser[String]]
}

trait Identifier extends RegexParsers with Keyword {
  override val skipWhitespace: Boolean = false

  lazy val IDENTIFIER_OR_KEYWORD: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*|_[a-zA-Z0-9_]+""".r
  lazy val RAW_IDENTIFIER: Parser[String] = "r#" ~> IDENTIFIER_OR_KEYWORD
  lazy val NON_KEYWORD_IDENTIFIER: Parser[String] = IDENTIFIER_OR_KEYWORD \ KEYWORD
  lazy val IDENTIFIER = NON_KEYWORD_IDENTIFIER | RAW_IDENTIFIER
}

trait Label extends RegexParsers with Identifier {
  override val skipWhitespace: Boolean = false

  // We don't really care about lifetimes
  lazy val LIFETIME_TOKEN: Parser[String] = (
      "'" ~> IDENTIFIER_OR_KEYWORD
    | "'_"
  )

  lazy val LIFETIME_OR_LABEL: Parser[String] = "'" ~> NON_KEYWORD_IDENTIFIER
}

// TODO: support comments
