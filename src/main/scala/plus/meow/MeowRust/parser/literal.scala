package plus.meow.MeowRust.parser

import plus.meow.MeowRust.grammar._
import plus.meow.MeowRust.grammar
import com.codecommit.gll._
import com.codecommit.gll.Parsers._

trait Literal extends RegexParsers {
  override val skipWhitespace: Boolean = false

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
  lazy val CHAR_LITERAL: Parser[CharLiteral] = (
    "'" ~> (
        """[^'\\\n\r\t]""".r ^^ { _.head }
      | QUOTE_ESCAPE
      | ASCII_ESCAPE
    ) <~ "'"
  ) ^^ { CharLiteral(_) }

  // TODO: support STRING_CONTINUE
  lazy val STRING_LITERAL: Parser[StrLiteral] = (
    "\"" ~> ((
        """[^\"\\\n]""".r ^^ { _.head }
      | QUOTE_ESCAPE
      | ASCII_ESCAPE
    )*) <~ "\""
  ) ^^ { a => StrLiteral(a.mkString) }

  lazy val RAW_STRING_LITERAL: Parser[StrLiteral] = (
    "r" ~> RAW_STRING_CONTENT
  ) ^^ { StrLiteral(_) }

  lazy val RAW_STRING_CONTENT: Parser[String] = (
      "\"" ~> ("""\\\n|[^\n]""".r *) <~ "\"" ^^ { _.mkString }
    | "#" ~> RAW_STRING_CONTENT <~ "#"
  )

  // TODO: byte strings. More specifically, supports Unicode

  lazy val BIN_DIGIT: Parser[Int] = "[0-1]".r ^^ { _.toInt }
  lazy val OCT_DIGIT: Parser[Int] = "[0-7]".r ^^ { _.toInt }
  lazy val DEC_DIGIT: Parser[Int] = "[0-9]".r ^^ { _.toInt }
  lazy val HEX_DIGIT = "[0-9a-f]".r ^^ { Integer.parseInt(_, 16) }

  lazy val INTEGER_LITERAL: Parser[IntLiteral] = (
    DEC_LITERAL | BIN_LITERAL | OCT_LITERAL | HEX_LITERAL
  ) ~ (INTEGER_SUFFIX?) ^^ { (num, cont) => new IntLiteral(num, cont) }

  def join_int_lit_parts(base: Int)(a: List[Any], b: Int, c: List[Any]) = {
    var result = BigInt(0)
    for(i <- a) {
      if(i.isInstanceOf[Int]) {
        result = result * base + i.asInstanceOf[Int]
      }
    }

    result = result * base + b

    for(i <- c) {
      if(i.isInstanceOf[Int]) {
        result = result * base + i.asInstanceOf[Int]
      }
    }

    result
  }

  lazy val HEX_LITERAL: Parser[BigInt] = (
    "0x" ~> ((HEX_DIGIT | "_")*) ~ HEX_DIGIT ~ ((HEX_DIGIT | "_")*) ^^ join_int_lit_parts(16)
  )

  lazy val OCT_LITERAL: Parser[BigInt] = (
    "0o" ~> ((OCT_DIGIT | "_")*) ~ OCT_DIGIT ~ ((OCT_DIGIT | "_")*) ^^ join_int_lit_parts(8)
  )

  lazy val BIN_LITERAL: Parser[BigInt] = (
    "0b" ~> ((BIN_DIGIT | "_")*) ~ BIN_DIGIT ~ ((BIN_DIGIT | "_")*) ^^ join_int_lit_parts(2)
  )

  lazy val RAW_DEC_LITERAL: Parser[String] = DEC_DIGIT ~ ((DEC_DIGIT | "_") *) ^^ { (a, b) => (a.toString :: b.filter(_.isInstanceOf[Int])).mkString}

  lazy val DEC_LITERAL: Parser[BigInt] = RAW_DEC_LITERAL ^^ { BigInt(_) }

  lazy val INTEGER_SUFFIX: Parser[String] = (
      "u8" | "u16" | "u32" | "u64" | "u128" | "usize"
    | "i8" | "i16" | "i32" | "i64" | "i128" | "isize"
  )

  lazy val FLOAT_LITERAL: Parser[FloatLiteral] = (
      RAW_DEC_LITERAL ~ FLOAT_EXPONENT ^^ { (num, exponent) => {
        val value = BigDecimal(num) * BigDecimal(10).pow(exponent.intValue())
        FloatLiteral(value, None)
      } }
    | ((RAW_DEC_LITERAL <~ ".") ~ RAW_DEC_LITERAL) ~ (FLOAT_EXPONENT?) ^^ {
        (s1, s2, exponent) => {
          val value = BigDecimal(s1 + "." + s2) * BigDecimal(10).pow(exponent.map(_.intValue) getOrElse 0)
          FloatLiteral(value, None)
        }
      }
    | RAW_DEC_LITERAL ~ (("." ~> RAW_DEC_LITERAL)?) ~ (FLOAT_EXPONENT?) ~ FLOAT_SUFFIX ^^ {
        (s1, s2, exponent, suffix) => {
          val tot = s1 + (s2.map("." + _) getOrElse "")
          val value = BigDecimal(tot) * BigDecimal(10).pow(exponent.map(_.intValue) getOrElse 0)
          FloatLiteral(value, Some(suffix))
        }
      }
  )

  lazy val FLOAT_EXPONENT: Parser[BigInt] = (
    "e|E".r ~> ("""\+|-""".r ?) ~ FLOAT_EXPONENT_CONTENT ^^ { (sign, num) => sign match {
      case Some("-") => -num
      case _ => num
    } }
  )

  lazy val FLOAT_EXPONENT_CONTENT: Parser[BigInt] = (
    ((DEC_DIGIT | "_")*) ~ DEC_DIGIT ~ ((DEC_DIGIT | "_")*) ^^ join_int_lit_parts(10)
  )

  lazy val FLOAT_SUFFIX: Parser[String] = ("f32" | "f64")

  lazy val BOOLEAN_LITERAL: Parser[BoolLiteral] = (
      "true" ^^ { (_) => true }
    | "false" ^^ { (_) => false }
  ) ^^ { BoolLiteral }

  lazy val LITERAL: Parser[grammar.Literal] = (
      CHAR_LITERAL
    | STRING_LITERAL
    | RAW_STRING_LITERAL
    | INTEGER_LITERAL
    | FLOAT_LITERAL
  )
}
