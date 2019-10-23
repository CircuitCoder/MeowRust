package plus.meow.MeowRust.parser
import com.codecommit.gll.RegexParsers

object Keyword {
  lazy val MAP = Map(
    "KW_AS" -> "as",
    "KW_BREAK" -> "break",
    "KW_CONST" -> "const",
    "KW_CONTINUE" -> "continue",
    // No supports for crate,
    "KW_ELSE" -> "else",
    "KW_ENUM" -> "enum",
    // No supports for extern,
    "KW_FALSE" -> "false",
    "KW_FN" -> "fn",
    "KW_FOR" -> "for",
    "KW_IF" -> "if",
    "KW_IMPL" -> "impl",
    "KW_IN" -> "in",
    "KW_LET" -> "let",
    "KW_LOOP" -> "loop",
    "KW_MATCH" -> "match",
    "KW_MOD" -> "mod",
    // No supports for move,
    "KW_MUT" -> "mut",
    // No supports for pub,
    "KW_REF" -> "ref",
    "KW_RETURN" -> "return",
    "KW_SELFVALUE" -> "self",
    "KW_SELFTYPE" -> "Self",
    "KW_STATIC" -> "static",
    "KW_SUPER" -> "super", // TODO: what is this?
    "KW_TRAIT" -> "trait",
    "KW_TRUE" -> "true",
    "KW_TYPE" -> "type",
    "KW_UNSAFE" -> "unsafe", // MeowRust don't differentiate safe and unsafe context
    "KW_USE" -> "use",
    "KW_WHERE" -> "where",
    "KW_WHILE" -> "while"
  )

  // We don't care about reserved keywords
}

trait Identifier extends RegexParsers {
  override val skipWhitespace: Boolean = false

  lazy val IDENTIFIER_OR_KEYWORD: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*|_[a-zA-Z0-9_]+""".r
  lazy val RAW_IDENTIFIER: Parser[String] = "r#" ~> IDENTIFIER_OR_KEYWORD
  lazy val NON_KEYWORD_IDENTIFIER: Parser[String] = Keyword.MAP.values.foldLeft(IDENTIFIER_OR_KEYWORD)((parser, kw) => parser \ kw)
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

// TODO: impl path
trait Path extends RegexParsers
