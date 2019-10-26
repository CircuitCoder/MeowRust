package plus.meow.MeowRust

import com.codecommit.gll.LineStream
import scala.io.Source
import plus.meow.MeowRust.parser.Parser
import com.codecommit.gll.Success
import com.codecommit.gll.Failure
import plus.meow.MeowRust.grammar.Node
import plus.meow.MeowRust.resolve.TypeResolvable
import plus.meow.MeowRust.resolve.TypeResolutionContext
import plus.meow.MeowRust.resolve.ResolvedType
import scala.collection.immutable.HashMap
import scala.collection.mutable

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
  def parse(file: String): Node = {
    println("Parsing file: " + file)

    val stream = LineStream(Source fromFile file)

    // First successful parsing-tree is with the intended precedence
    for(i <- Parser.parse(stream)) {
      i match {
        case Success(tree, _) =>  {
          println("Parser tree:\n" + tree.fmt)
          return tree
        }
        case Failure(f, t) => println("Failed: " + f + ", " + t.mkString) 
      }
    }

    throw new Error("No applicable parsing tree")
  }

  def typing(root: Node): ResolvedType = {
    if(root.isInstanceOf[TypeResolvable]) {
      // TODO: register
      val ctx = TypeResolutionContext(HashMap(), None, None, mutable.HashMap())
      val t = root.asInstanceOf[TypeResolvable].resolve(ctx, None)
      println("Root type: " + t.toString)
      t
    } else {
      throw new Error("Root not type-resolvable")
    }
  }

  override def main(args: Array[String]) {
    for(file <- args) {
      println("Processing file " + file)
      val tree = parse(file)
      val resolvedType = typing(tree)
    }
  }
}
