package plus.meow.MeowRust.parser
import com.codecommit.gll.RegexParsers

trait Pattern extends RegexParsers with Literal with Identifier {
  // TODO: support other types of pattern
  lazy val PATTERN: Parser[Any] = (
      LITERAL_PATTERN
    | IDENTIFIER_PATTERN
  )

  // TODO: support negative literals
  lazy val LITERAL_PATTERN: Parser[Any] = LITERAL;

  lazy val IDENTIFIER_PATTERN: Parser[Any] = 
    ("ref"?) ~ ("mut"?) ~ IDENTIFIER ~ (("@" ~> PATTERN)?)
}