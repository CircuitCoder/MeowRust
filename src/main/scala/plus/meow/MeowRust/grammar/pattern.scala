package plus.meow.MeowRust.grammar

abstract class Pattern
case class LiteralPattern(literal: Literal) extends Pattern
case class IdentifierPattern(ident: String, ref: Boolean, mut: Boolean) extends Pattern
case class BoundPattern(ident: IdentifierPattern, pat: Pattern) extends Pattern
