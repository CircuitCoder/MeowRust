package plus.meow.MeowRust.grammar
import plus.meow.MeowRust.resolve.TypeResolvable
import plus.meow.MeowRust.resolve.{ResolvedType, TypeHint, TypeResolutionContext}
import plus.meow.MeowRust.resolve.ResolutionErrorType

abstract class Pattern extends TypeResolvable {
  def applyBinding(ctx: TypeResolutionContext, t: ResolvedType) {
    throw new Error("ICE: pattern ident register not impl")
  }
}

case class LiteralPattern(literal: Literal) extends Pattern {
  override def applyBinding(ctx: TypeResolutionContext, t: ResolvedType) {} // No-op
}

case class IdentifierPattern(ident: String, ref: Boolean, mut: Boolean) extends Pattern {
  override def applyBinding(ctx: TypeResolutionContext, t: ResolvedType) {
    ctx.applyBinding(ident, t)
  }

  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    ctx.bindings.get(ident) getOrElse ResolutionErrorType("Cannot find binding for " + ident)
  }
}

case class BoundPattern(ident: IdentifierPattern, pat: Pattern) extends Pattern {
  // TODO: impl pattern typing
}
