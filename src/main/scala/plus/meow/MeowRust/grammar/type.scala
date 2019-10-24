package plus.meow.MeowRust.grammar

abstract class PathSeg()
case class IdentSeg(ident: String) extends PathSeg
case class SuperSeg() extends PathSeg
case class SelfSeg() extends PathSeg
case class SelfTypeSeg() extends PathSeg

case class GenericArgs(
  val types: List[Type] = List(),
  val bindings: Map[String, Type] = Map()
)

abstract class TypeSegArg
case class NonFnTypeArg(args: GenericArgs) extends TypeSegArg
case class FnTypeArg(args: List[Type], ret: Option[Type]) extends TypeSegArg

case class SimplePath(fromRoot: Boolean, segs: List[PathSeg]) extends Node
case class PathInExpression(fromRoot: Boolean, segs: List[(PathSeg, GenericArgs)]) extends Node
case class QualifiedPath[F](base: Type, as: Option[TypePath], follow: F) extends Node
case class TypePath(fromRoot: Boolean, segs: List[(PathSeg, TypeSegArg)]) extends Node

abstract class Type extends Node
// TODO: Maybe support existential types?
// TODO: trait object types
case class TupleType(types: List[Type]) extends Type
case class RefType(deref: Type, mut: Boolean) extends Type
case class ArrayType(base: Type, length: Expr) extends Type
case class SliceType(base: Type) extends Type
case class IdentType(path: TypePath) extends Type
case class QualifiedIdentType(path: QualifiedPath[TypePath]) extends Type
case class FuncType(args: FnTypeArg) extends Type
case class NeverType() extends Type

// Placeholder for holes
case class InferredType() extends Type
