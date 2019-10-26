package plus.meow.MeowRust.grammar
import plus.meow.MeowRust.resolve._

abstract class Stmt extends TypeResolvable {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType
    = plus.meow.MeowRust.resolve.TupleType(List()) // Unit
}

case class LetStmt(store: Pattern, value: Option[Expr]) extends Stmt {
  // TODO: assign type to the pattern
  // TODO: assign type hint to value
}
case class ExprStmt(expr: Expr) extends Stmt {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    expr.resolve(ctx, None)
    plus.meow.MeowRust.resolve.TupleType(List()) // Unit
  }
}
case class EmptyStmt() extends Stmt
