package plus.meow.MeowRust.grammar

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

abstract class Expr extends Node
case class LiteralExpr(lit: Literal) extends Expr
// TODO: statements
case class BlockExpr(body: List[Any]) extends Expr
case class TupleExpr(exprs: List[Expr]) extends Expr
case class ArrayExpr(elems: List[Expr]) extends Expr
case class ArrayFillExpr(elem: Expr, count: Expr) extends Expr
case class CallExpr(recv: Expr, method: Option[String], params: List[Expr]) extends Expr
case class FieldExpr(owner: Expr, field: Expr) extends Expr
case class ArrayIndexExpr(owner: Expr, index: Expr) extends Expr
case class TupleIndexExpr(owner: Expr, index: BigInt) extends Expr
// TODO: pattern
case class ClosureExpr(params: List[Any], body: Expr) extends Expr
case class FlowCtrlExpr(ctrl: FlowCtrl, label: Option[String], param: Option[Expr]) extends Expr
case class RangeExpr(from: Option[Expr], to: Option[Expr], includeTo: Boolean) extends Expr

// Operator expressions
case class BorrowExpr(content: Expr, mut: Boolean) extends Expr
case class DerefExpr(from: Expr) extends Expr
case class QuestionExpr(unwrapped: Expr) extends Expr
case class UnaryOpExpr(op: UnaryOp, operand: Expr) extends Expr
case class BinaryOpExpr(op: BinaryOp, lhs: Expr, rhs: Expr) extends Expr

// Loops
case class InftyLoopExpr(label: Option[String], body: Expr) extends Expr
// TODO: pattern
case class ForLoopExpr(label: Option[String], pat: Any, collection: Expr, body: Expr) extends Expr
case class WhileExpr(label: Option[String], cond: Expr, body: Expr) extends Expr
// TODO: pattern
case class WhileLetExpr(label: Option[String], pat: List[Any], unwrapped: Expr, body: Expr) extends Expr

// Conditionals
case class IfExpr(cond: Expr, body: Expr, otherwise: Option[Expr]) extends Expr
// TODO: pattern
case class IfLetExpr(pat: List[Any], unwrapped: Expr, body: Expr, otherwise: Option[Expr]) extends Expr

// Matches
// TODO: pattern
case class MatchArm(pat: Any, guardCond: Option[Expr], body: Expr)
case class MatchExpr(input: Expr, arms: List[MatchArm]) extends Expr
