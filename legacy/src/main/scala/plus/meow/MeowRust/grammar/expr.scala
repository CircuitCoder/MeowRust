package plus.meow.MeowRust.grammar

import plus.meow.MeowRust.resolve._

abstract class UnaryOp
case class Neg() extends UnaryOp
case class LogicalNot() extends UnaryOp

abstract class BinaryOp
case class Add() extends BinaryOp
case class Sub() extends BinaryOp
case class Mul() extends BinaryOp
case class Div() extends BinaryOp
case class Mod() extends BinaryOp
case class BitwiseAnd() extends BinaryOp
case class BitwiseOr() extends BinaryOp
case class BitwiseXor() extends BinaryOp
case class BitwiseLShift() extends BinaryOp
case class BitwiseRShift() extends BinaryOp

case class LogicalAnd() extends BinaryOp
case class LogicalOr() extends BinaryOp

case class CmpEq() extends BinaryOp
case class CmpNe() extends BinaryOp
case class CmpL() extends BinaryOp
case class CmpG() extends BinaryOp
case class CmpLe() extends BinaryOp
case class CmpGe() extends BinaryOp

case class Assign() extends BinaryOp
case class AddAssign() extends BinaryOp
case class SubAssign() extends BinaryOp
case class MulAssign() extends BinaryOp
case class DivAssign() extends BinaryOp
case class ModAssign() extends BinaryOp
case class BitwiseAndAssign() extends BinaryOp
case class BitwiseOrAssign() extends BinaryOp
case class BitwiseXorAssign() extends BinaryOp
case class BitwiseLShiftAssign() extends BinaryOp
case class BitwiseRShiftAssign() extends BinaryOp

abstract class FlowCtrl
case class Return() extends FlowCtrl
case class Continue() extends FlowCtrl
case class Break() extends FlowCtrl

abstract class Expr extends Node with TypeResolvable with ConstEvaluable

// TODO: impl constEval for types other thant literal expr

case class LiteralExpr(lit: Literal) extends Expr {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = lit.resolve(ctx, hint)
  override def constEval: ConstEvalResult = lit.constEval
}

case class BlockExpr(body: List[Stmt], implicitRet: Option[Expr]) extends Expr {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    // TODO: check all ret values are the same?
    for(s <- body) s.resolve(ctx, None)
    implicitRet match {
      case None => plus.meow.MeowRust.resolve.TupleType(List())
      case Some(expr) => expr.resolve(ctx, None)
    }
  }
}
case class TupleExpr(exprs: List[Expr]) extends Expr {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    val types = for(e <- exprs) yield e.resolve(ctx, None)
    plus.meow.MeowRust.resolve.TupleType(types)
  }
}
case class ArrayExpr(elems: List[Expr]) extends Expr {
  // TODO: check all elems are of the same type?
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    // TODO: error if hint doesn't match
    var base = hint.flatMap(_.t match {
      case plus.meow.MeowRust.resolve.ArrayType(t, len) => Some(t)
      case _ => None
    })

    // TODO: apply type hints
    for(e <- elems) {
      val self = e.resolve(ctx, None)
      if(base.isEmpty) base = Some(self)
    }
    plus.meow.MeowRust.resolve.ArrayType(base.get, elems.length)
  }
}
case class ArrayFillExpr(elem: Expr, count: Expr) extends Expr {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    // TODO: error if hint doesn't match
    val base = elem.resolve(ctx, None)
    val countType = count.resolve(ctx, None)
    val countEvaluated = count.constEval.asInstanceOf[ConstEvalValue]
    // TODO: error if count cannot be const-evaluated, or is not evaluated to ints
    plus.meow.MeowRust.resolve.ArrayType(base, countEvaluated.value.asInstanceOf[BigInt])
  }
}
case class CallExpr(recv: Expr, method: Option[String], params: List[Expr]) extends Expr {
  // TODO: resolve functions
}
case class FieldExpr(owner: Expr, field: String) extends Expr
case class ArrayIndexExpr(owner: Expr, index: Expr) extends Expr {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    owner.resolve(ctx, None) match {
      case plus.meow.MeowRust.resolve.ArrayType(b, _) => b
      case _ => {
        // TODO: convert to Index / IndexMut calls
        ResolutionErrorType("Unimplemented: Index/IndexMut override")
      }
    }
  }
}
case class TupleIndexExpr(owner: Expr, index: BigInt) extends Expr {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    owner.resolve(ctx, None) match {
      case plus.meow.MeowRust.resolve.TupleType(l) => {
        if(l.length <= index) {
          ResolutionErrorType("Tuple with length " + l.length + " indexed with " + index)
        } else {
          l(index toInt)
        }
      }
      case t => {
        ResolutionErrorType("Cannot use tuple index on " + t.toString)
      }
    }
  }
}
case class ClosureExpr(params: List[Pattern], body: Expr) extends Expr {
  // TODO: typing patterns
}
case class FlowCtrlExpr(ctrl: FlowCtrl, label: Option[String], param: Option[Expr]) extends Expr {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    val ptype = param.map(_.resolve(ctx, None)) getOrElse plus.meow.MeowRust.resolve.TupleType(List()) // Unit

    // TODO: reverse typehint
    val around = ctrl match {
      case Break() => ctx.getBreakable(label)
      case Return() => ctx.func
    }

    if(around.isDefined) {
      around.get.apply(ptype)
    }

    plus.meow.MeowRust.resolve.NeverType()
  }
}
case class RangeExpr(from: Option[Expr], to: Option[Expr], includeTo: Boolean) extends Expr {
  // TODO: impl range class
}

// Operator expressions
case class BorrowExpr(content: Expr, mut: Boolean) extends Expr {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    // TODO: deref hint
    val inner = content.resolve(ctx, None)
    plus.meow.MeowRust.resolve.RefType(inner, mut)
  }
}

case class DerefExpr(from: Expr) extends Expr {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    // TODO: deref hint
    from.resolve(ctx, None) match {
      case plus.meow.MeowRust.resolve.RefType(inner, _) => inner
      case t => ResolutionErrorType("Cannot deref " + t.toString)
    }
  }
}
case class QuestionExpr(unwrapped: Expr) extends Expr {
  // TODO: check surrounding function ret type
  // TODO: impl result type
}

// TODO: desuger into function calls
case class UnaryOpExpr(op: UnaryOp, operand: Expr) extends Expr
case class BinaryOpExpr(op: BinaryOp, lhs: Expr, rhs: Expr) extends Expr

// Loops
abstract class LoopExpr extends Expr {
  val label: Option[String]
  val body: Expr

  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    var selfType: Option[ResolvedType] = None
    val newCtx = ctx.withBreakable(label, t => {
      selfType = Some(t)
      true
    })
    // TODO: check if the end of the loop is reachable
    val bodyType = body.resolve(newCtx, None)

    // TODO: check if bodyType is unit
    selfType getOrElse plus.meow.MeowRust.resolve.NeverType()
  }
}
case class InftyLoopExpr(label: Option[String], body: Expr) extends LoopExpr
case class ForLoopExpr(label: Option[String], pat: Pattern, collection: Expr, body: Expr) extends LoopExpr {
  // TODO: IntoIterator trait
  // TODO: apply pattern type into ctx when resolving types
  // TODO: for loops cannot have breaks with values
}
case class WhileExpr(label: Option[String], cond: Expr, body: Expr) extends LoopExpr {
  // TODO: while loops cannot have breaks with values
}
case class WhileLetExpr(label: Option[String], pat: List[Pattern], unwrapped: Expr, body: Expr) extends LoopExpr {
  // TODO: apply pattern type into ctx when resolving types
  // TODO: while loops cannot have breaks with values
}

// Conditionals
case class IfExpr(cond: Expr, body: Expr, otherwise: Option[Expr]) extends Expr {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    val condType = cond.resolve(ctx, None)
    if(condType != ResolvedType.bool)
      return ResolutionErrorType("Cannot use as condition: " + condType.toString)

    val trueArmType = body.resolve(ctx, None)
    val falseArmType = otherwise map { _.resolve(ctx, None) } getOrElse plus.meow.MeowRust.resolve.TupleType(List()) // Unit
    if(falseArmType != trueArmType)
      return ResolutionErrorType("Cannot use different types on if arms: " + trueArmType.toString + ", " + falseArmType.toString)

    trueArmType
  }
}
case class IfLetExpr(pat: List[Pattern], unwrapped: Expr, body: Expr, otherwise: Option[Expr]) extends Expr {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    // TODO: apply binding
    val trueArmType = body.resolve(ctx, hint)
    if(otherwise.isDefined) {
      val inner = otherwise.get
      val falseArmType = inner.resolve(ctx, hint)
      if(falseArmType != trueArmType)
        return ResolutionErrorType("Cannot use different types on if arms: " + trueArmType.toString + ", " + falseArmType.toString)
    }

    trueArmType
  }
}

// Matches
case class MatchArm(pat: List[Pattern], guardCond: Option[Expr], body: Expr)
case class MatchExpr(input: Expr, arms: List[MatchArm]) extends Expr {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    val inputType = input.resolve(ctx, None)
    // TODO: check match arm pattern
    // TODO: register match arm pattern
    var store: Option[ResolvedType] = None
    for(arm <- arms) arm match {
      case MatchArm(pat, _, body) => {
        val t = body.resolve(ctx, hint)

        if(store.isEmpty) store = Some(t)
        else if(store.get != t) {
          return ResolutionErrorType("Match arm typing errored: Expected " + store.get.toString + ", got " + t.toString)
        }
      }
    }

    store getOrElse plus.meow.MeowRust.resolve.NeverType()
  }
}

case class PathExpr(path: EvaluablePath) extends Expr {
  override def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    ctx.resolveBinding(path)
  }
}
