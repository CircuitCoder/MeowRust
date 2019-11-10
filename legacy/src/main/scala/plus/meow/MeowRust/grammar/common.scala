package plus.meow.MeowRust.grammar

object Printer {
  def fmt(v: Any, indent: Int): String = {
    v match {
      case x: Traversable[_] => {
        if(x.size == 0) ("  " * indent) + "[]"
        else ("  " * indent) + "[\n" +
          x.view.map(fmt(_, indent+1)).mkString(",\n")+ ",\n" +
          ("  " * indent) + "]"
      }

      case x: Product if x.productArity == 0 => ("  " * indent) + x.productPrefix
      case x: Product => {
        ("  " * indent) + x.productPrefix + "(\n" +
          x.productIterator.map(fmt(_, indent+1)).mkString(",\n") + ",\n" +
          ("  " * indent) + ")"
      }

      case null => "NULL"
      case x: String => ("  " * indent) + '"' + x + '"'
      case _ => ("  " * indent) + v.toString
    }
  }
}

abstract class Node {
  def fmt: String = Printer.fmt(this, 0)
}
