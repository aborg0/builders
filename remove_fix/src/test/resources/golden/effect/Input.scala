import builders.configuration.*

@GenerateBuilder(
  style = BuilderStyle.Effect,
  primitivePolicy = PrimitivePolicy.WrappedOnly,
  pathMode = PathMode.CustomPrefixOnly,
  effectMode = EffectMode.AbstractCapability,
  conversionMode = ConversionMode.SynthesizeAndExposeHelpers,
  staleCheckMode = StaleCheckMode.StructuralOnly,
  mergeMode = MergeMode.ReplaceGeneratedMembers
)
case class EffectGoldenUser(id: Int)
