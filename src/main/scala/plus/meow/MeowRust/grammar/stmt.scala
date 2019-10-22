package plus.meow.MeowRust.grammar

abstract class Stmt

case class LetStmt(store: Pattern, value: Option[Expr]) extends Stmt
case class ExprStmt(expr: Expr) extends Stmt
case class EmptyStmt() extends Stmt
