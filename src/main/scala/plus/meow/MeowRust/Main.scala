package plus.meow.MeowRust

import com.codecommit.gll.LineStream
import scala.io.Source
import plus.meow.MeowRust.parser.Parser
import com.codecommit.gll.Success
import com.codecommit.gll.Failure

/**
 * MeowRust compiler
 * 
 * Stages:
 *  - Parser
 *  - Disambiguation
 *     - For example, compare operators requires parentheses to specify associativity
 *  - Codegen: Derive(...)
 *  - Register: types, impls...
 *  - Resolve + Type-check - 1: Generate types for each expression.
 *       This also involves picking the most accurate trait impl,
 *       and actually instantiate generic functions
 *  - Disugar: Question marks, etc. Notably, compile matches \w exhaustiveness check
 *  - Type-check - 2: Checks if subtyping/conversion works. Insert implicit conversions
 *  - Target generation: Dump LLVM IR
 */


object Main extends App {
  override def main(args: Array[String]) {
    for(file <- args) {
      println("Processing file " + file)
      val stream = LineStream(Source fromFile file)

      // First parse-tree / error is with the intended precedence
      for(i <- Parser.parse(stream)) {
        i match {
          case Success(tree, _) =>  {
            println("Parser tree:\n" + tree.fmt)
            return
          }
          case Failure(f, t) => println("Failed: " + f + ", " + t.mkString) 
        }
      }
    }
  }
}
