package plus.meow.MeowRust.parser
import com.codecommit.gll.RegexParsers
import plus.meow.MeowRust.grammar._
import plus.meow.MeowRust.grammar

trait Pattern extends Literal with Identifier {
  // TODO: support other types of pattern
  lazy val PATTERN: Parser[grammar.Pattern] = (
      LITERAL_PATTERN
    | IDENTIFIER_PATTERN
  )

  // TODO: support negative literals
  lazy val LITERAL_PATTERN: Parser[LiteralPattern] = LITERAL ^^ LiteralPattern

  lazy val IDENTIFIER_PATTERN: Parser[grammar.Pattern] = 
    ("ref"?!) ~ ("mut"?!) ~ IDENTIFIER ~? (("@" ~>? PATTERN)?) ^^ { (ref, mut, id, bounded) => {
      val idpat = IdentifierPattern(id, ref.isDefined, mut.isDefined)
      bounded match {
        case None => idpat
        case Some(value) => BoundPattern(idpat, value)
      }
    }}
}
