package plus.meow.MeowRust.parser

object Parser extends Type {
  val parser = TYPE
  def parse = """[\s\n]*""".r ~> parser <~ """[\s\n]*""".r
}
