package builders.scalafix

import utest._

object GenerateBuilderTextRewriterOptionMatrixTests extends TestSuite {
  val tests: Tests = Tests {
    test("simple style emits readable typed step methods") {
      val input =
        """import builders.configuration.*

@GenerateBuilder(style = BuilderStyle.Simple)
case class SimpleFieldUser(id: Int, label: String)
"""

      val actual = GenerateBuilderTextRewriter.rewrite(input)

      assert(actual.contains("private val generatorRuleVersion: String = \"0.1.0-SNAPSHOT\""))
      assert(actual.contains("// format: off"))
      assert(actual.contains("// format: on"))
      assert(actual.contains("private val builderApi: Any = builder"))
      assert(actual.contains("private type Builder = (id: IdInput => AfterStep1)"))
      assert(actual.contains("inline def builder: Builder = (id= (id: IdInput) => step1(id))"))
      assert(actual.contains("private type IdInput = Int"))
      assert(actual.contains("private type LabelInput = String"))
      assert(actual.contains("private type AfterStep1 = (label: LabelInput => AfterStep2)"))
      assert(actual.contains("private type AfterStep2 = SimpleFieldUser"))
      assert(actual.contains("private inline def step1(id: IdInput): AfterStep1 = (label= (label: LabelInput) => step2(id, label))"))
      assert(actual.contains("private inline def step2(id: IdInput, label: LabelInput): AfterStep2 = SimpleFieldUser(id, label)"))
      assert(!actual.contains("private def headStep"))
      assert(!actual.contains("private def advance"))
      assert(!actual.contains("private val fieldSteps"))
      assert(!actual.contains("private def buildFromValues"))
    }

    test("text-only rewrite does not synthesize smart-constructor helpers") {
      val input =
        """import builders.configuration.*

@GenerateBuilder(style = BuilderStyle.Simple)
case class SmartSimpleUser(id: Int, region: Region)
"""

      val actual = GenerateBuilderTextRewriter.rewrite(input)

      assert(actual.contains("private type RegionInput = Region"))
      assert(!actual.contains("SimpleSmartConstructor"))
      assert(!actual.contains("unwrapSmartResult"))
      assert(actual.contains("private inline def step2(id: IdInput, region: RegionInput): AfterStep2 = SmartSimpleUser(id, region)"))
    }

    test("simple style can opt in to omitted optional values with build action") {
      val input =
        """import builders.configuration.*

@GenerateBuilder(style = BuilderStyle.Simple, simpleOptionalValues = SimpleOptionalValues.OptionalValuesWithEmptyDefaults)
case class OptionalsUser(optInt: java.util.Optional[Int], mandatory: String, maybeString: Option[Int], nullable: Boolean | Null)
"""

      val actual = GenerateBuilderTextRewriter.rewrite(input)

      assert(actual.contains("private type OptIntInput = Int"))
      assert(actual.contains("private type MaybeStringInput = Int"))
      assert(actual.contains("private type NullableInput = Boolean"))
      assert(actual.contains("inline def builder: Builder = state0()"))
      assert(actual.contains("private inline def state0(): Builder = (optInt= (optInt: OptIntInput) => state1(java.util.Optional.of(optInt)), mandatory= (mandatory: MandatoryInput) => state2(java.util.Optional.empty(), mandatory))"))
      assert(actual.contains("build= () => state4(optInt, mandatory, None, null)"))
      assert(actual.contains("private inline def state4(optInt: java.util.Optional[Int], mandatory: String, maybeString: Option[Int], nullable: Boolean | Null): AfterStep4 = OptionalsUser(optInt, mandatory, maybeString, nullable)"))
    }

    test("simple style can use constructor defaults for optional omissions") {
      val input =
        """import builders.configuration.*

@GenerateBuilder(style = BuilderStyle.Simple, simpleOptionalValues = SimpleOptionalValues.OptionalValuesFromDefaults)
case class DefaultsUser(mandatory: String, maybe: Option[Int] = Some(99), nullable: Boolean | Null = true)
"""

      val actual = GenerateBuilderTextRewriter.rewrite(input)

      assert(actual.contains("private type MaybeInput = Int"))
      assert(actual.contains("private type NullableInput = Boolean"))
      assert(actual.contains("build= () => state3(mandatory, Some(99), true)"))
      assert(actual.contains("private inline def state3(mandatory: String, maybe: Option[Int], nullable: Boolean | Null): AfterStep3 = DefaultsUser(mandatory, maybe, nullable)"))
    }

    test("effect style and non-default options propagate into generated companion") {
      val input =
        """import builders.configuration.*

@GenerateBuilder(
  style = BuilderStyle.Effect,
  primitivePolicy = PrimitivePolicy.WrappedOnly,
  pathMode = PathMode.CustomPrefixOnly,
  effectMode = EffectMode.AbstractCapability,
  conversionMode = ConversionMode.SynthesizeAndExposeHelpers,
  staleCheckMode = StaleCheckMode.StructuralOnly,
  mergeMode = MergeMode.ReplaceGeneratedMembers
)
case class EffectUser(id: Int)
"""

      val actual = GenerateBuilderTextRewriter.rewrite(input)

      assert(actual.contains("private val style: builders.configuration.BuilderStyle = builders.configuration.BuilderStyle.Effect"))
      assert(actual.contains("private val modeTag: String = \"effect\""))
      assert(actual.contains("private val builderApi: Any = api.ValidatedBuilderGenerator.builder[EffectUser]"))
      assert(actual.contains("private type Builder = api.ValidatedBuilderSelectable[EffectUser, ?, (id: Int)]"))
      assert(actual.contains("def builder: Builder = api.ValidatedBuilderGenerator.builder[EffectUser]"))
      assert(actual.contains("def builderEffect: Builder = builder"))
      assert(actual.contains("private type IdInput = Int"))
      assert(actual.contains("private type AfterStep1 = zio.prelude.ZValidation[Nothing, ?, EffectUser]"))
      assert(actual.contains("private def fieldStep1Id(current: Builder, input: IdInput): AfterStep1"))
      assert(actual.contains("private val fieldSteps: List[String] = List(idStepName)"))
      assert(actual.contains("private def buildEffectFromValues(idValue: IdInput): zio.prelude.ZValidation[Nothing, ?, EffectUser] ="))
      assert(actual.contains("private def builderRef: Builder = builder"))
      assert(actual.contains("private def builderEffectRef: Any = builderEffect"))
      assert(actual.contains("private val primitivePolicy: builders.configuration.PrimitivePolicy = builders.configuration.PrimitivePolicy.WrappedOnly"))
      assert(actual.contains("private val pathMode: builders.configuration.PathMode = builders.configuration.PathMode.CustomPrefixOnly"))
      assert(actual.contains("private val effectMode: builders.configuration.EffectMode = builders.configuration.EffectMode.AbstractCapability"))
      assert(actual.contains("private val conversionMode: builders.configuration.ConversionMode = builders.configuration.ConversionMode.SynthesizeAndExposeHelpers"))
      assert(actual.contains("private val staleCheckMode: builders.configuration.StaleCheckMode = builders.configuration.StaleCheckMode.StructuralOnly"))
      assert(actual.contains("private val mergeMode: builders.configuration.MergeMode = builders.configuration.MergeMode.ReplaceGeneratedMembers"))
    }

    test("validating style supports custom builder method name and disabling extra variants") {
      val input =
        """import builders.configuration.*

@GenerateBuilder(
  style = BuilderStyle.Validating,
  builderMethodName = "make",
  generateExtraVariants = false
)
case class NamedUser(id: Int, code: String)
"""

      val actual = GenerateBuilderTextRewriter.rewrite(input)

      assert(actual.contains("private type Builder = api.ValidatedBuilderSelectable[NamedUser, ?, (id: Int, code: String)]"))
      assert(actual.contains("def make: Builder = api.ValidatedBuilderGenerator.builder[NamedUser]"))
      assert(!actual.contains("def makeAllow:"))
      assert(!actual.contains("def makeNoAllow:"))
      assert(!actual.contains("private def makeAllowRef:"))
      assert(!actual.contains("private def makeNoAllowRef:"))
      assert(actual.contains("val afterStep1: AfterStep1 = fieldStep1Id(make, idValue)"))
    }

    test("validating style preserves smart-constructor seams") {
      val input =
        """import builders.configuration.*

@GenerateBuilder(style = BuilderStyle.Validating)
case class SmartCtorUser(id: Int, code: String)
"""

      val actual = GenerateBuilderTextRewriter.rewrite(input)

      assert(actual.contains("def builder: Builder = api.ValidatedBuilderGenerator.builder[SmartCtorUser]"))
      assert(actual.contains("def builderAllow: Builder = api.ValidatedBuilderGenerator.builderAllow[SmartCtorUser]"))
      assert(actual.contains("def builderNoAllow: Builder = api.ValidatedBuilderGenerator.builderNoAllow[SmartCtorUser]"))
      assert(actual.contains("private def derived: api.ValidatedBuilderGenerator[SmartCtorUser] = api.ValidatedBuilderGenerator.derived[SmartCtorUser]"))
      assert(actual.contains("private def derivedAllow: api.ValidatedBuilderGenerator[SmartCtorUser] = api.ValidatedBuilderGenerator.derivedAllow[SmartCtorUser]"))
      assert(actual.contains("private def derivedNoAllow: api.ValidatedBuilderGenerator[SmartCtorUser] = api.ValidatedBuilderGenerator.derivedNoAllow[SmartCtorUser]"))
    }

    test("existing companion is not regenerated") {
      val input =
        """import builders.configuration.*

@GenerateBuilder(style = BuilderStyle.Simple)
case class ExistingCompanionUser(i: Int)

object ExistingCompanionUser {
  val unchanged = 1
}
"""

      val actual = GenerateBuilderTextRewriter.rewrite(input)
      assert(actual == input)
    }
  }
}
