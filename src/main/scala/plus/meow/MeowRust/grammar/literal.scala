package plus.meow.MeowRust.grammar
import plus.meow.MeowRust.resolve._
import plus.meow.MeowRust

abstract class Literal extends Node with TypeResolvable with ConstEvaluable {
  lazy val solidType: ResolvedType = ResolutionErrorType("ICE: Literal resolution not overrided")
  override def resolve(
    ctx: TypeResolutionContext,
    hint: Option[TypeHint]
  ): ResolvedType = solidType

  type LitRepr
  val value: LitRepr 
  override def constEval: ConstEvalResult = ConstEvalValue(value)
}

case class CharLiteral(val value: Char) extends Literal {
  override type LitRepr = Char
  override lazy val solidType: ResolvedType = PrimitiveType("char")
}
case class IntLiteral(val value: BigInt, val container: Option[String]) extends Literal {
  override type LitRepr = BigInt
  override lazy val solidType: ResolvedType = PrimitiveType(container getOrElse "isize") // TODO: infer length
}
case class FloatLiteral(val value: BigDecimal, val container: Option[String]) extends Literal {
  override type LitRepr = BigDecimal
  override lazy val solidType: ResolvedType = PrimitiveType(container getOrElse "f128") // TODO: infer length
}
case class StrLiteral(val value: String) extends Literal {
  override type LitRepr = String
  override lazy val solidType: ResolvedType = MeowRust.resolve.RefType(PrimitiveType("str"), false)
}
case class BoolLiteral(val value: Boolean) extends Literal {
  override type LitRepr = Boolean
  override lazy val solidType: ResolvedType = PrimitiveType("bool")
}

// TODO: array, tuple, struct, enum literals
