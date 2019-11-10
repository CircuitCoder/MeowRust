package plus.meow.MeowRust.grammar
import plus.meow.MeowRust.resolve._

abstract class Stmt extends TypeResolvable {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType
    = plus.meow.MeowRust.resolve.TupleType(List()) // Unit
}

case class LetStmt(store: Pattern, value: Option[Expr]) extends Stmt {
  // TODO: assign type to the pattern
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    val t = value match {
      case None => throw new Error("Reverse type-inferring not impl")
      case Some(expr) => expr.resolve(ctx, None)
    }

    store.applyBinding(ctx, t)
    plus.meow.MeowRust.resolve.TupleType(List()) // Unit
  }
}
case class ExprStmt(expr: Expr, withBlock: Boolean) extends Stmt {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    val bodyType = expr.resolve(ctx, None)
    if(withBlock) bodyType
    else plus.meow.MeowRust.resolve.TupleType(List()) // Unit
  }
}
case class EmptyStmt() extends Stmt
