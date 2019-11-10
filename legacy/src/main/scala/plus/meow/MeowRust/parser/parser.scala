package plus.meow.MeowRust.parser

object Parser extends Statement {
  val parser = EXPRESSION
  def parse = """[\s\n]*""".r ~> parser <~ """[\s\n]*""".r
}
