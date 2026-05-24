package builders.configuration

import scala.annotation.StaticAnnotation

enum BuilderStyle {
  case Simple
  case Validating
  case Effect
}

enum PrimitivePolicy {
  case WrappedOnly
  case PrimitiveAndWrappedIfDerivable
  case PrimitiveOnly
}

enum PathMode {
  case Disabled
  case CustomPrefixOnly
  case FullCollectionAware
}

enum EffectMode {
  case ValidationOnly
  case AbstractCapability
}

enum ConversionMode {
  case UseExistingOnly
  case SynthesizeIfMissing
  case SynthesizeAndExposeHelpers
}

enum StaleCheckMode {
  case SignatureHash
  case StructuralOnly
}

enum MergeMode {
  case GeneratedRegionOnly
  case ReplaceGeneratedMembers
}

enum SimpleOptionalValues {
  case ExplicitOptionalValues
  case OptionalValuesFromDefaults
  case OptionalValuesWithEmptyDefaults
}

enum SmartConstructorMode {
  case ZValidation
  case Either
  case Direct
}

final class GenerateBuilder(
  val style: BuilderStyle = BuilderStyle.Validating,
  val primitivePolicy: PrimitivePolicy = PrimitivePolicy.PrimitiveAndWrappedIfDerivable,
  val pathMode: PathMode = PathMode.FullCollectionAware,
  val effectMode: EffectMode = EffectMode.ValidationOnly,
  val conversionMode: ConversionMode = ConversionMode.SynthesizeIfMissing,
  val staleCheckMode: StaleCheckMode = StaleCheckMode.SignatureHash,
  val mergeMode: MergeMode = MergeMode.GeneratedRegionOnly,
  val builderMethodName: String = "builder",
  val generateExtraVariants: Boolean = true,
  val simpleOptionalValues: SimpleOptionalValues = SimpleOptionalValues.ExplicitOptionalValues,
  val smartConstructorMode: SmartConstructorMode = SmartConstructorMode.ZValidation
) extends StaticAnnotation

final case class GenerateBuilderOptions(
  style: BuilderStyle,
  primitivePolicy: PrimitivePolicy,
  pathMode: PathMode,
  effectMode: EffectMode,
  conversionMode: ConversionMode,
  staleCheckMode: StaleCheckMode,
  mergeMode: MergeMode,
  builderMethodName: String,
  generateExtraVariants: Boolean,
  simpleOptionalValues: SimpleOptionalValues,
  smartConstructorMode: SmartConstructorMode
)

object GenerateBuilderOptions {
  val default: GenerateBuilderOptions = GenerateBuilderOptions(
    style = BuilderStyle.Validating,
    primitivePolicy = PrimitivePolicy.PrimitiveAndWrappedIfDerivable,
    pathMode = PathMode.FullCollectionAware,
    effectMode = EffectMode.ValidationOnly,
    conversionMode = ConversionMode.SynthesizeIfMissing,
    staleCheckMode = StaleCheckMode.SignatureHash,
    mergeMode = MergeMode.GeneratedRegionOnly,
    builderMethodName = "builder",
    generateExtraVariants = true,
    simpleOptionalValues = SimpleOptionalValues.ExplicitOptionalValues,
    smartConstructorMode = SmartConstructorMode.ZValidation
  )
}
