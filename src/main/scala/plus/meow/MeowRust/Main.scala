package plus.meow.MeowRust

import com.codecommit.gll.LineStream
import scala.io.Source
import plus.meow.MeowRust.parser.Parser
import com.codecommit.gll.Success

/**
 * MeowRust compiler
 * 
 * Stages:
 *  - Parser
 *  - Disambiguation
 *     - For example, compare operators requires parentheses to specify associativity
 *  - Codegen: Derive(...)
 *  - Register: types, impls...
 *  - Resolve + Type-check - 1: Generate types for each expression. This also involves picking the most accurate trait impl
 *  - Disugar: Question marks, etc.
 *  - Type-check - 2: Checks if subtyping/conversion works. Insert implicit conversions
 *  - Target generation: Dump LLVM IR
 */


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
