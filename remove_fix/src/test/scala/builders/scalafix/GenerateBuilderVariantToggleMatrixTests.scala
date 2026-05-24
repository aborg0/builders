package builders.scalafix

import utest._

object GenerateBuilderVariantToggleMatrixTests extends TestSuite {
  private final case class VariantCase(
    name: String,
    style: String,
    className: String,
    extraVariants: Boolean,
    expectedPresent: List[String],
    expectedAbsent: List[String]
  )

  val tests: Tests = Tests {
    test("custom prefix and extra variant toggle matrix") {
      val cases = List(
        VariantCase(
          name = "simple ignores extra variants",
          style = "Simple",
          className = "SimpleToggleUser",
          extraVariants = false,
          expectedPresent = List(
            "inline def make: Builder = (id= (id: IdInput) => step1(id))",
            "private inline def step1(id: IdInput): AfterStep1 = (code= (code: CodeInput) => step2(id, code))",
            "private inline def step2(id: IdInput, code: CodeInput): AfterStep2 = SimpleToggleUser(id, code)"
          ),
          expectedAbsent = List(
            "def makeAllow:",
            "def makeNoAllow:",
            "def makeEffect:"
          )
        ),
        VariantCase(
          name = "validating with extra variants",
          style = "Validating",
          className = "ValidatingToggleUser",
          extraVariants = true,
          expectedPresent = List(
            "def make: Builder = api.ValidatedBuilderGenerator.builder[ValidatingToggleUser]",
            "def makeAllow: Builder = api.ValidatedBuilderGenerator.builderAllow[ValidatingToggleUser]",
            "def makeNoAllow: Builder = api.ValidatedBuilderGenerator.builderNoAllow[ValidatingToggleUser]"
          ),
          expectedAbsent = Nil
        ),
        VariantCase(
          name = "validating without extra variants",
          style = "Validating",
          className = "ValidatingNoExtraUser",
          extraVariants = false,
          expectedPresent = List(
            "def make: Builder = api.ValidatedBuilderGenerator.builder[ValidatingNoExtraUser]"
          ),
          expectedAbsent = List(
            "def makeAllow:",
            "def makeNoAllow:",
            "private def makeAllowRef:",
            "private def makeNoAllowRef:"
          )
        ),
        VariantCase(
          name = "effect with extra variants",
          style = "Effect",
          className = "EffectToggleUser",
          extraVariants = true,
          expectedPresent = List(
            "def make: Builder = api.ValidatedBuilderGenerator.builder[EffectToggleUser]",
            "def makeEffect: Builder = make"
          ),
          expectedAbsent = Nil
        ),
        VariantCase(
          name = "effect without extra variants",
          style = "Effect",
          className = "EffectNoExtraUser",
          extraVariants = false,
          expectedPresent = List(
            "def make: Builder = api.ValidatedBuilderGenerator.builder[EffectNoExtraUser]"
          ),
          expectedAbsent = List(
            "def makeEffect:",
            "private def makeEffectRef:"
          )
        )
      )

      cases.foreach { c =>
        val input =
          s"""import builders.configuration.*

@GenerateBuilder(
  style = BuilderStyle.${c.style},
  builderMethodName = \"make\",
  generateExtraVariants = ${c.extraVariants}
)
case class ${c.className}(id: Int, code: String)
"""

        val actual = GenerateBuilderTextRewriter.rewrite(input)

        c.expectedPresent.foreach { snippet =>
          assert(actual.contains(snippet))
        }
        c.expectedAbsent.foreach { snippet =>
          assert(!actual.contains(snippet))
        }
      }
    }
  }
}
