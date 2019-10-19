package plus.meow.MeowRust.grammar

abstract class Literal extends Node
case class IntLiteral(val value: Int) extends Literal
case class FloatLiteral(val value: Int) extends Literal
case class StrLiteral(val value: String) extends Literal