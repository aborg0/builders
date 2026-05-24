package builders.scalafix

import builders.scalafix.GenerateBuilderCompanionRenderer.SmartCtorResultKind
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
      assert(actual.contains("private val builderApi: Any = builder"))
      assert(actual.contains("private type Builder = ValidatedBuilderSelectable[EffectUser, ?, (id: Int)]"))
      assert(actual.contains("def builder: Builder = builderState0()"))
      assert(actual.contains("def builderEffect: Builder = builderState0()"))
      assert(actual.contains("private type IdInput = Int"))
      assert(actual.contains("private type AfterStep1 = zio.prelude.ZValidation[Nothing, ?, EffectUser]"))
      assert(actual.contains("private inline def builderState0(): Builder ="))
      assert(actual.contains("Tuple1((idValue: IdInput) => buildEffectFromValues(idValue).asInstanceOf[Any])"))
      assert(actual.contains("private def buildEffectFromValues(idValue: IdInput): zio.prelude.ZValidation[Nothing, ?, EffectUser] ="))
      assert(actual.contains("private inline given idSmartConstructor: (IdInput => IdValidation) ="))
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

      assert(actual.contains("private type Builder = ValidatedBuilderSelectable[NamedUser, ?, (id: Int, code: String)]"))
      assert(actual.contains("def make: Builder = builderState0()"))
      assert(!actual.contains("def makeAllow:"))
      assert(!actual.contains("def makeNoAllow:"))
      assert(!actual.contains("derivedAllow"))
      assert(!actual.contains("derivedNoAllow"))
      assert(actual.contains("Tuple1((idValue: IdInput) => builderState1(idValue).asInstanceOf[Any])"))
    }

    test("validating style emits local builder seams") {
      val input =
        """import builders.configuration.*

@GenerateBuilder(style = BuilderStyle.Validating)
case class SmartCtorUser(id: Int, code: String)
"""

      val actual = GenerateBuilderTextRewriter.rewrite(input)

      assert(actual.contains("def builder: Builder = builderState0()"))
      assert(actual.contains("def builderAllow: Builder = builderState0()"))
      assert(actual.contains("def builderNoAllow: Builder = builderState0()"))
      assert(actual.contains("private inline given idSmartConstructor: (IdInput => IdValidation) ="))
      assert(actual.contains("private inline given codeSmartConstructor: (CodeInput => CodeValidation) ="))
      assert(!actual.contains("ValidatedBuilderGenerator.derived"))
    }

    test("validating renderer emits smart constructor conversions") {
      val actual = GenerateBuilderCompanionRenderer.render(
        className = "SmartRenderedUser",
        fields = List(
          GenerateBuilderCompanionRenderer.ClassField(
            name = "code",
            typeExpr = "OpaqueCode",
            smartCtorMethodName = Some("apply"),
            smartCtorResultKind = Some(SmartCtorResultKind.EitherResult)
          ),
          GenerateBuilderCompanionRenderer.ClassField(
            name = "op",
            typeExpr = "OpaqueOp",
            smartCtorMethodName = Some("apply"),
            smartCtorResultKind = Some(SmartCtorResultKind.Validation)
          ),
          GenerateBuilderCompanionRenderer.ClassField(
            name = "amount",
            typeExpr = "PositiveInt",
            smartCtorMethodName = Some("make"),
            smartCtorResultKind = Some(SmartCtorResultKind.EitherResult)
          ),
          GenerateBuilderCompanionRenderer.ClassField(
            name = "region",
            typeExpr = "Region",
            smartCtorMethodName = Some("apply"),
            smartCtorResultKind = Some(SmartCtorResultKind.Direct)
          )
        ),
        options = DecodedGenerateBuilder.default
      )

      assert(actual.contains("zio.prelude.ZValidation.fromEither(OpaqueCode(codeValue)).asInstanceOf[CodeValidation]"))
      assert(actual.contains("OpaqueOp(opValue).asInstanceOf[OpValidation]"))
      assert(actual.contains("zio.prelude.ZValidation.fromEither(PositiveInt.make(amountValue)).asInstanceOf[AmountValidation]"))
      assert(actual.contains("zio.prelude.ZValidation.succeed(Region(regionValue)).asInstanceOf[RegionValidation]"))
    }

    test("validating rewrite discovers smart constructors from source") {
      val input =
        """import builders.configuration.*
import zio.prelude.ZValidation

opaque type OpaqueCode = String
object OpaqueCode {
  def apply(raw: String): Either[String, OpaqueCode] = Right(raw: OpaqueCode)
}

opaque type OpaqueOp = String
object OpaqueOp {
  def apply(raw: String): ZValidation[Nothing, String, OpaqueOp] = zio.prelude.Validation.succeed(raw: OpaqueOp)
}

@GenerateBuilder(style = BuilderStyle.Validating)
case class SmartDiscoveredUser(code: OpaqueCode, op: OpaqueOp)
"""

      val actual = GenerateBuilderTextRewriter.rewrite(input)

      assert(actual.contains("zio.prelude.ZValidation.fromEither(OpaqueCode(codeValue)).asInstanceOf[CodeValidation]"))
      assert(actual.contains("OpaqueOp(opValue).asInstanceOf[OpValidation]"))
    }

    test("validating rewrite discovers subtype newtype make from alias owner") {
      val input =
        """import builders.configuration.*
import zio.prelude.Subtype
import zio.prelude.Assertion.greaterThanOrEqualTo

object SequenceNumber extends Subtype[Int] {
  override inline def assertion = greaterThanOrEqualTo(0)
}
type SequenceNumber = SequenceNumber.Type

@GenerateBuilder(style = BuilderStyle.Validating)
case class SmartSubtypeUser(sequence: SequenceNumber)
"""

      val actual = GenerateBuilderTextRewriter.rewrite(input)

      assert(actual.contains("SequenceNumber.make(sequenceValue).asInstanceOf[SequenceValidation]"))
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
