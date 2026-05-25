package builders.scalafix

sealed abstract class BuilderStyle(val value: String)
object BuilderStyle {
  case object Simple extends BuilderStyle("Simple")
  case object Validating extends BuilderStyle("Validating")
  case object Effect extends BuilderStyle("Effect")

  val all: List[BuilderStyle] = List(Simple, Validating, Effect)
}

sealed abstract class PrimitivePolicy(val value: String)
object PrimitivePolicy {
  case object WrappedOnly extends PrimitivePolicy("WrappedOnly")
  case object PrimitiveAndWrappedIfDerivable extends PrimitivePolicy("PrimitiveAndWrappedIfDerivable")
  case object PrimitiveOnly extends PrimitivePolicy("PrimitiveOnly")

  val all: List[PrimitivePolicy] = List(WrappedOnly, PrimitiveAndWrappedIfDerivable, PrimitiveOnly)
}

sealed abstract class PathMode(val value: String)
object PathMode {
  case object Disabled extends PathMode("Disabled")
  case object CustomPrefixOnly extends PathMode("CustomPrefixOnly")
  case object FullCollectionAware extends PathMode("FullCollectionAware")

  val all: List[PathMode] = List(Disabled, CustomPrefixOnly, FullCollectionAware)
}

sealed abstract class EffectMode(val value: String)
object EffectMode {
  case object ValidationOnly extends EffectMode("ValidationOnly")
  case object AbstractCapability extends EffectMode("AbstractCapability")

  val all: List[EffectMode] = List(ValidationOnly, AbstractCapability)
}

sealed abstract class ConversionMode(val value: String)
object ConversionMode {
  case object UseExistingOnly extends ConversionMode("UseExistingOnly")
  case object SynthesizeIfMissing extends ConversionMode("SynthesizeIfMissing")
  case object SynthesizeAndExposeHelpers extends ConversionMode("SynthesizeAndExposeHelpers")

  val all: List[ConversionMode] = List(UseExistingOnly, SynthesizeIfMissing, SynthesizeAndExposeHelpers)
}

sealed abstract class StaleCheckMode(val value: String)
object StaleCheckMode {
  case object SignatureHash extends StaleCheckMode("SignatureHash")
  case object StructuralOnly extends StaleCheckMode("StructuralOnly")

  val all: List[StaleCheckMode] = List(SignatureHash, StructuralOnly)
}

sealed abstract class MergeMode(val value: String)
object MergeMode {
  case object GeneratedRegionOnly extends MergeMode("GeneratedRegionOnly")
  case object ReplaceGeneratedMembers extends MergeMode("ReplaceGeneratedMembers")

  val all: List[MergeMode] = List(GeneratedRegionOnly, ReplaceGeneratedMembers)
}

sealed abstract class SimpleOptionalValues(val value: String)
object SimpleOptionalValues {
  case object ExplicitOptionalValues extends SimpleOptionalValues("ExplicitOptionalValues")
  case object OptionalValuesFromDefaults extends SimpleOptionalValues("OptionalValuesFromDefaults")
  case object OptionalValuesWithEmptyDefaults extends SimpleOptionalValues("OptionalValuesWithEmptyDefaults")

  val all: List[SimpleOptionalValues] = List(
    ExplicitOptionalValues,
    OptionalValuesFromDefaults,
    OptionalValuesWithEmptyDefaults
  )
}

sealed abstract class SmartConstructorMode(val value: String)
object SmartConstructorMode {
  case object ZValidation extends SmartConstructorMode("ZValidation")
  case object Either extends SmartConstructorMode("Either")
  case object Direct extends SmartConstructorMode("Direct")

  val all: List[SmartConstructorMode] = List(ZValidation, Either, Direct)
}

sealed abstract class ErrorCombination(val value: String)
object ErrorCombination {
  case object Union extends ErrorCombination("Union")
  case object LeastUpperBound extends ErrorCombination("LeastUpperBound")

  val all: List[ErrorCombination] = List(Union, LeastUpperBound)
}

sealed abstract class EffectFailureMode(val value: String)
object EffectFailureMode {
  case object Propagate extends EffectFailureMode("Propagate")
  case object OrDie extends EffectFailureMode("OrDie")
  case object OrElseProvided extends EffectFailureMode("OrElseProvided")

  val all: List[EffectFailureMode] = List(Propagate, OrDie, OrElseProvided)
}
