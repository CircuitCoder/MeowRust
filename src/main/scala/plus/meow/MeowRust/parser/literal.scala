package plus.meow.MeowRust.parser

import plus.meow.MeowRust.grammar._
import com.codecommit.gll._
import com.codecommit.gll.Parsers._

object Literal extends RegexParsers {
  override val skipWhitespace: Boolean = false

  lazy val OCT_DIGIT: Parser[Int] = "[0-7]".r ^^ { _.toInt }
  lazy val HEX_DIGIT = "[0-9a-f]".r ^^ { Integer.parseInt(_, 16) }
  lazy val QUOTE_ESCAPE: Parser[Char] = (
      "\\\'" ^^ { _ => '\'' }
    | "\\\"" ^^ { _ => '"' }
    )
  lazy val ASCII_ESCAPE: Parser[Char] = (
      "\\x" ~ OCT_DIGIT ~ HEX_DIGIT ^^ { (_, o, h) => (o * 16 + h).asInstanceOf[Char] }
    | "\\n" ^^ { _ => '\n' }
    | "\\r" ^^ { _ => '\r' }
    | "\\t" ^^ { _ => '\t' }
    | "\\\\" ^^ { _ => '\\' }
    | "\\0" ^^ { _ => '\0' }
  )
  // TODO: support UNICODE_ESCAPE
  lazy val CHAR_LITERAL: Parser[Char] = (
    "'" ~> (
        """[^'\\\n\r\t]""".r ^^ { _.head }
      | QUOTE_ESCAPE
      | ASCII_ESCAPE
    ) <~ "'"
  )

  // TODO: support STRING_CONTINUE
  lazy val STRING_LITERAL: Parser[String] = (
    "\"" ~> ((
        """[^\"\\\n]""".r ^^ { _.head }
      | QUOTE_ESCAPE
      | ASCII_ESCAPE
    )*) <~ "\"" ^^ { _.mkString }
  )

  lazy val RAW_STRING_LITERAL: Parser[String] = (
    "r" ~> RAW_STRING_CONTENT
  )

  lazy val RAW_STRING_CONTENT: Parser[String] = (
      "\"" ~> ("""\\\n|[^\n]""".r *) <~ "\"" ^^ { _.mkString }
    | "#" ~> RAW_STRING_CONTENT <~ "#"
  )
}