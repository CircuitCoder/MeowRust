package plus.meow.MeowRust.resolve

abstract class ResolutionError
case class OriginResolutionError(val reason: String) extends ResolutionError
case class ImmediateResolutionError(val cause: List[ResolutionError]) extends ResolutionError
