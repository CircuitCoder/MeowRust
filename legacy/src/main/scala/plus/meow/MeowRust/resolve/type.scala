package plus.meow.MeowRust.resolve
import scala.collection.immutable.HashMap
import scala.collection.mutable
import plus.meow.MeowRust.grammar.EvaluablePath
import plus.meow.MeowRust.grammar.PathInExpression
import plus.meow.MeowRust.grammar.IdentSeg

class ResolvedImpl(
)

abstract class ResolvedType {
  val isPrimitive = false
  val isSolid = false
}

case class NeverType() extends ResolvedType
case class RefType(val inner: ResolvedType, val mut: Boolean) extends ResolvedType
case class PrimitiveType(val name: String) extends ResolvedType
case class ArrayType(val base: ResolvedType, val len: BigInt) extends ResolvedType
case class TupleType(val elems: List[ResolvedType]) extends ResolvedType
// Unit = TupleType(List())
case class FuncType(val params: List[ResolvedType], val ret: ResolvedType) extends ResolvedType
case class TraitType() extends ResolvedType
case class ParamType() extends ResolvedType
case class StructType() extends ResolvedType
case class EnumType() extends ResolvedType

// Errored
case class ResolutionErrorType(val cause: ResolutionError) extends ResolvedType
object ResolutionErrorType {
  def apply(reason: String): ResolutionErrorType = ResolutionErrorType(OriginResolutionError(reason))
}

class TypeHint(val t: ResolvedType, val solidify: ResolvedType => Unit)

// Immutable
case class TypeResolutionContext(
  val labels: HashMap[String, ResolvedType => Boolean],
  val func: Option[ResolvedType => Boolean],
  val breakable: Option[ResolvedType => Boolean],

  var bindings: mutable.HashMap[String, ResolvedType]
) {
  def getBreakable(label: Option[String]) = label match {
    case Some(l) => labels.get(l)
    case _ => breakable
  }

  def withBreakable(label: Option[String], act: ResolvedType => Boolean): TypeResolutionContext = {
    val newMap = label match {
      case None => labels
      case Some(l) => labels + (l -> act)
    }

    TypeResolutionContext(newMap, func, Some(act), bindings.clone())
  }

  def withCtx = TypeResolutionContext(labels, func, breakable, bindings.clone())

  // Called within block
  // TODO: mutable state
  def applyBinding(path: String, t: ResolvedType) {
    bindings += (path-> t)
  }

  def resolveBinding(path: EvaluablePath): ResolvedType = {
    path match {
      case PathInExpression(r, segs) => {
        if(r || segs.length > 1) {
          // TODO: resolve global bindings
          ResolutionErrorType("Global bindings not impl: " + path.toString)
        } else {
          // TODO: instantiate generic functions
          segs(0)._1 match {
            case IdentSeg(ident) =>
              bindings.get(ident) match {
                case Some(v) => v
                case None => ResolutionErrorType("Ident not found: " + path.toString)
              }
            case _ => ResolutionErrorType("super/self not impl: " + path.toString)
          }

        }
      }
      // Qualified paths cannot happen for now
    }
  }
}

trait TypeResolvable {
  var typeCache: Option[ResolvedType] = None

  def resolve(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    ResolutionErrorType("Type resolution not implemented for " + this.toString)
  }
  def getType(ctx: TypeResolutionContext, hint: Option[TypeHint]): ResolvedType = {
    // TODO: check hint
    val result = typeCache getOrElse { resolve(ctx, hint) }
    typeCache = Some(result)
    result
  }
}

object ResolvedType {
  val u8 = PrimitiveType("u8")
  val u16 = PrimitiveType("u16")
  val u32 = PrimitiveType("u32")
  val u64 = PrimitiveType("u64")
  val u128 = PrimitiveType("u128")
  val usize = PrimitiveType("usize")

  val i8 = PrimitiveType("i8")
  val i16 = PrimitiveType("i16")
  val i32 = PrimitiveType("i32")
  val i64 = PrimitiveType("i64")
  val i128 = PrimitiveType("i128")
  val isize = PrimitiveType("isize")

  val char = PrimitiveType("char")
  val str = PrimitiveType("str")
  val bool = PrimitiveType("bool")
}
