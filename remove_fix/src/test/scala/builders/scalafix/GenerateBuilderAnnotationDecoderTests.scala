package builders.scalafix
import utest._

object GenerateBuilderAnnotationDecoderTests extends TestSuite {
  val tests: Tests = Tests {
    test("empty annotation uses defaults") {
      val decoded = GenerateBuilderAnnotationDecoder.decode(Map.empty)
      assert(decoded == Right(DecodedGenerateBuilder.default))
    }

    test("every BuilderStyle value decodes") {
      BuilderStyle.all.foreach { style =>
        val decoded = GenerateBuilderAnnotationDecoder.decode(Map("style" -> style.value))
        assert(decoded == Right(DecodedGenerateBuilder.default.copy(style = style)))
      }
    }

    test("every PrimitivePolicy value decodes") {
      PrimitivePolicy.all.foreach { policy =>
        val decoded = GenerateBuilderAnnotationDecoder.decode(Map("primitivePolicy" -> policy.value))
        assert(decoded == Right(DecodedGenerateBuilder.default.copy(primitivePolicy = policy)))
      }
    }

    test("every PathMode value decodes") {
      PathMode.all.foreach { mode =>
        val decoded = GenerateBuilderAnnotationDecoder.decode(Map("pathMode" -> mode.value))
        assert(decoded == Right(DecodedGenerateBuilder.default.copy(pathMode = mode)))
      }
    }

    test("every EffectMode value decodes") {
      EffectMode.all.foreach { mode =>
        val decoded = GenerateBuilderAnnotationDecoder.decode(Map("effectMode" -> mode.value))
        assert(decoded == Right(DecodedGenerateBuilder.default.copy(effectMode = mode)))
      }
    }

    test("every ConversionMode value decodes") {
      ConversionMode.all.foreach { mode =>
        val decoded = GenerateBuilderAnnotationDecoder.decode(Map("conversionMode" -> mode.value))
        assert(decoded == Right(DecodedGenerateBuilder.default.copy(conversionMode = mode)))
      }
    }

    test("every StaleCheckMode value decodes") {
      StaleCheckMode.all.foreach { mode =>
        val decoded = GenerateBuilderAnnotationDecoder.decode(Map("staleCheckMode" -> mode.value))
        assert(decoded == Right(DecodedGenerateBuilder.default.copy(staleCheckMode = mode)))
      }
    }

    test("every MergeMode value decodes") {
      MergeMode.all.foreach { mode =>
        val decoded = GenerateBuilderAnnotationDecoder.decode(Map("mergeMode" -> mode.value))
        assert(decoded == Right(DecodedGenerateBuilder.default.copy(mergeMode = mode)))
      }
    }

    test("every ErrorCombination value decodes") {
      ErrorCombination.all.foreach { mode =>
        val decoded = GenerateBuilderAnnotationDecoder.decode(Map("combineErrors" -> mode.value))
        assert(decoded == Right(DecodedGenerateBuilder.default.copy(combineErrors = mode)))
      }
    }

    test("every EffectFailureMode value decodes") {
      EffectFailureMode.all.foreach { mode =>
        val decoded = GenerateBuilderAnnotationDecoder.decode(Map("effectFailureMode" -> mode.value))
        assert(decoded == Right(DecodedGenerateBuilder.default.copy(effectFailureMode = mode)))
      }
    }

    test("qualified enum values decode") {
      val decoded = GenerateBuilderAnnotationDecoder.decode(
        Map(
          "style" -> "BuilderStyle.Simple",
          "primitivePolicy" -> "PrimitivePolicy.WrappedOnly",
          "pathMode" -> "PathMode.Disabled",
          "effectMode" -> "EffectMode.AbstractCapability",
          "conversionMode" -> "ConversionMode.SynthesizeAndExposeHelpers",
          "staleCheckMode" -> "StaleCheckMode.StructuralOnly",
          "mergeMode" -> "MergeMode.ReplaceGeneratedMembers",
          "builderMethodName" -> "\"make\"",
          "generateExtraVariants" -> "false",
          "simpleOptionalValues" -> "SimpleOptionalValues.OptionalValuesWithEmptyDefaults",
          "combineErrors" -> "ErrorCombination.LeastUpperBound",
          "effectFailureMode" -> "EffectFailureMode.OrElseProvided"
        )
      )
      assert(
        decoded == Right(
          DecodedGenerateBuilder(
            style = BuilderStyle.Simple,
            primitivePolicy = PrimitivePolicy.WrappedOnly,
            pathMode = PathMode.Disabled,
            effectMode = EffectMode.AbstractCapability,
            conversionMode = ConversionMode.SynthesizeAndExposeHelpers,
            staleCheckMode = StaleCheckMode.StructuralOnly,
            mergeMode = MergeMode.ReplaceGeneratedMembers,
            builderMethodName = "make",
            generateExtraVariants = false,
            simpleOptionalValues = SimpleOptionalValues.OptionalValuesWithEmptyDefaults,
            smartConstructorMode = SmartConstructorMode.ZValidation,
            combineErrors = ErrorCombination.LeastUpperBound,
            effectFailureMode = EffectFailureMode.OrElseProvided
          )
        )
      )
    }

    test("unknown keys and bad values return diagnostics") {
      val decoded = GenerateBuilderAnnotationDecoder.decode(
        Map(
          "style" -> "BuilderStyle.NotARealStyle",
          "builderMethodName" -> "\"not-valid-name!\"",
          "generateExtraVariants" -> "maybe",
          "simpleOptionalValues" -> "not-a-mode",
          "combineErrors" -> "not-a-combination",
          "effectFailureMode" -> "not-a-failure-mode",
          "unknownOption" -> "42"
        )
      )
      assert(decoded.isLeft)
      val errors = decoded.left.toOption.getOrElse(Nil)
      assert(errors.exists(_.contains("Unknown annotation argument: unknownOption")))
      assert(errors.exists(_.contains("Invalid value 'BuilderStyle.NotARealStyle' for style")))
      assert(errors.exists(_.contains("Invalid value '\"not-valid-name!\"' for builderMethodName")))
      assert(errors.exists(_.contains("Invalid value 'maybe' for generateExtraVariants")))
      assert(errors.exists(_.contains("Invalid value 'not-a-mode' for simpleOptionalValues")))
      assert(errors.exists(_.contains("Invalid value 'not-a-combination' for combineErrors")))
      assert(errors.exists(_.contains("Invalid value 'not-a-failure-mode' for effectFailureMode")))
    }
  }
}
