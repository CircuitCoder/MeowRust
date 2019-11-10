package plus.meow.MeowRust.resolve

abstract class ConstEvalResult
case class ConstEvalValue(val value: Any) extends ConstEvalResult
case class ConstEvalErrored(val reason: String) extends ConstEvalResult

trait ConstEvaluable {
  def constEval: ConstEvalResult = ConstEvalErrored("Cannot be compile-time evaled: " + this.toString)
}
