package plus.meow.MeowRust.parser
import com.codecommit.gll.RegexParsers
import scala.util.matching.Regex
import com.codecommit.gll.{LineStream, Result}
import com.codecommit.gll.Success
import plus.meow.MeowRust.grammar._
import plus.meow.MeowRust.grammar

trait SeparatedParsers extends RegexParsers {
  override val skipWhitespace: Boolean = false

  val delimStart = Set('+', '-', '*', '/', '%', '^', '!', '&', '|',
    '>', '<', '=', '@', '_', '.', ',', ';', ':', '#', '$', '?')

  class SeparatorParser extends RegexParser("""[\n\t ]+""".r) {
    override def computeFirst(seen: Set[Parser[Any]])
      = Some((super.computeFirst(seen).get ++ delimStart.map(c => Some(c))))

    override def parse(in: LineStream) = {
      if(in.length > 0 && (delimStart contains in.head)) Success("", in)
      else super.parse(in)
    }
  }

  val separator = new SeparatorParser()

  implicit class ParserExt[+R](val parser: Parser[R]) {
    def ~![R2](that: Parser[R2]) = (parser <~ separator) ~ that
    def ~?[R2](that: Parser[R2]) = (parser <~ (separator?)) ~ that

    def ~>![R2](that: Parser[R2]) = (parser <~ separator) ~> that
    def ~>?[R2](that: Parser[R2]) = (parser <~ (separator?)) ~> that

    def <~![R2](that: Parser[R2]) = (parser <~ separator) <~ that
    def <~?[R2](that: Parser[R2]) = (parser <~ (separator?)) <~ that

    def ?!() = (parser <~ separator)?
    def *?() = (parser <~ (separator?))*
    def *!() = (parser <~ separator)*
  }

  implicit class ParserExtStr(val str: String) extends ParserExt(literal(str))
  implicit class ParserExtRegex(val reg: Regex) extends ParserExt(regex(reg))
}

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

trait Identifier extends SeparatedParsers {
  lazy val IDENTIFIER_OR_KEYWORD: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*|_[a-zA-Z0-9_]+""".r
  lazy val RAW_IDENTIFIER: Parser[String] = "r#" ~> IDENTIFIER_OR_KEYWORD
  lazy val NON_KEYWORD_IDENTIFIER: Parser[String] = Keyword.MAP.values.foldLeft(IDENTIFIER_OR_KEYWORD)((parser, kw) => parser \ kw)
  lazy val IDENTIFIER = NON_KEYWORD_IDENTIFIER | RAW_IDENTIFIER
}

trait Label extends SeparatedParsers with Identifier {
  // We don't really care about lifetimes
  lazy val LIFETIME_TOKEN: Parser[String] = (
      "'" ~> IDENTIFIER_OR_KEYWORD
    | "'_"
  )

  lazy val LIFETIME_OR_LABEL: Parser[String] = "'" ~> NON_KEYWORD_IDENTIFIER
}

// TODO: support comments
