package builders.configuration

import utest.*

object GenerateBuilderSchemaTests extends TestSuite {
  val tests: Tests = Tests {
    test("all enum values are present") {
      assert(BuilderStyle.values.toList == List(
        BuilderStyle.Simple,
        BuilderStyle.Validating,
        BuilderStyle.Effect
      ))
      assert(PrimitivePolicy.values.toList == List(
        PrimitivePolicy.WrappedOnly,
        PrimitivePolicy.PrimitiveAndWrappedIfDerivable,
        PrimitivePolicy.PrimitiveOnly
      ))
      assert(PathMode.values.toList == List(
        PathMode.Disabled,
        PathMode.CustomPrefixOnly,
        PathMode.FullCollectionAware
      ))
      assert(EffectMode.values.toList == List(
        EffectMode.ValidationOnly,
        EffectMode.AbstractCapability
      ))
      assert(ConversionMode.values.toList == List(
        ConversionMode.UseExistingOnly,
        ConversionMode.SynthesizeIfMissing,
        ConversionMode.SynthesizeAndExposeHelpers
      ))
      assert(StaleCheckMode.values.toList == List(
        StaleCheckMode.SignatureHash,
        StaleCheckMode.StructuralOnly
      ))
      assert(MergeMode.values.toList == List(
        MergeMode.GeneratedRegionOnly,
        MergeMode.ReplaceGeneratedMembers
      ))
      assert(SimpleOptionalValues.values.toList == List(
        SimpleOptionalValues.ExplicitOptionalValues,
        SimpleOptionalValues.OptionalValuesFromDefaults,
        SimpleOptionalValues.OptionalValuesWithEmptyDefaults
      ))
    }

    test("default options are stable") {
      val expected = GenerateBuilderOptions(
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
      assert(GenerateBuilderOptions.default == expected)
    }

    test("annotation defaults mirror options defaults") {
      val annotation = new GenerateBuilder()
      assert(annotation.style == GenerateBuilderOptions.default.style)
      assert(annotation.primitivePolicy == GenerateBuilderOptions.default.primitivePolicy)
      assert(annotation.pathMode == GenerateBuilderOptions.default.pathMode)
      assert(annotation.effectMode == GenerateBuilderOptions.default.effectMode)
      assert(annotation.conversionMode == GenerateBuilderOptions.default.conversionMode)
      assert(annotation.staleCheckMode == GenerateBuilderOptions.default.staleCheckMode)
      assert(annotation.mergeMode == GenerateBuilderOptions.default.mergeMode)
      assert(annotation.builderMethodName == GenerateBuilderOptions.default.builderMethodName)
      assert(annotation.generateExtraVariants == GenerateBuilderOptions.default.generateExtraVariants)
      assert(annotation.simpleOptionalValues == GenerateBuilderOptions.default.simpleOptionalValues)
    }
  }
}
