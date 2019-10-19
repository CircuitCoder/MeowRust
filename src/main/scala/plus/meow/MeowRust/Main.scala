package plus.meow.MeowRust

import com.codecommit.gll.LineStream
import scala.io.Source
import plus.meow.MeowRust.parser.Parser
import com.codecommit.gll.Success

object Main extends App {
  override def main(args: Array[String]) {
    for(file <- args) {
      println("Processing file " + file)
      val stream = LineStream(Source fromFile file)

      val parsed = Parser.parse(stream)

      if(parsed exists { _.isInstanceOf[Success[String]] }) {
        for(Success(tree, _) <- parsed) { println(tree) }
      }
    }
  }
}