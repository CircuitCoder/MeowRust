package plus.meow.MeowRust.parser
import com.codecommit.gll.RegexParsers
import plus.meow.MeowRust.grammar._
import plus.meow.MeowRust.grammar

trait Statement extends Expression {
  lazy val STATEMENT: Parser[Stmt] = (
      ";" ^^^ { EmptyStmt() }
    // | ITEM // TODO: impl
    | LET_STATEMENT
    | EXPRESSION_STATEMENT
  )

  // TODO: support type ascriptions
  lazy val LET_STATEMENT: Parser[LetStmt] = "let" ~>! PATTERN ~? (("=" ~>? EXPRESSION)?) <~? ";" ^^ LetStmt
  lazy val EXPRESSION_STATEMENT: Parser[ExprStmt] = (
      EXPRESSION_WITHOUT_BLOCK <~? ";" ^^ { ExprStmt(_, false) }
    | EXPRESSION_WITH_BLOCK ^^ { ExprStmt(_, true) }
  )
}
