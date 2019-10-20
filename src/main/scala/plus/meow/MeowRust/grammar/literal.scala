package plus.meow.MeowRust.grammar

abstract class Literal extends Node
case class CharLiteral(val value: Char) extends Literal
case class IntLiteral(val value: BigInt, val container: Option[String]) extends Literal
case class FloatLiteral(val value: BigDecimal, val container: Option[String]) extends Literal
case class StrLiteral(val value: String) extends Literal
case class BoolLiteral(val value: Boolean) extends Literal
