package builders.scalafix

import scala.meta._
import utest._

object GenerateBuildersRuleLogicTests extends TestSuite {
  private def firstClass(code: String): Defn.Class = {
    val source = code.parse[Source].get
    source.stats.collectFirst { case c: Defn.Class => c }.get
  }

  val tests: Tests = Tests {
    test("extracts args from parsed annotation") {
      val code =
        """@GenerateBuilder(
  style = BuilderStyle.Validating,
  primitivePolicy = PrimitivePolicy.WrappedOnly,
  mergeMode = MergeMode.ReplaceGeneratedMembers
)
case class A(i: Int)
"""

      val cls = firstClass(code)
      val init = cls.mods.collectFirst { case Mod.Annot(i) => i }.get

      val args = GenerateBuildersRuleLogic.annotationArguments(init)

      assert(args("style") == "BuilderStyle.Validating")
      assert(args("primitivePolicy") == "PrimitivePolicy.WrappedOnly")
      assert(args("mergeMode") == "MergeMode.ReplaceGeneratedMembers")
    }

    test("symbol match uses semantic symbol") {
      val code =
        """@GenerateBuilder
case class B(i: Int)
"""
      val cls = firstClass(code)
      val init = cls.mods.collectFirst { case Mod.Annot(i) => i }.get

      assert(
        GenerateBuildersRuleLogic.isGenerateBuilderAnnotation(
          init,
          "builders/configuration/GenerateBuilder#"
        )
      )
    }

    test("name fallback works when symbol is unavailable") {
      val code =
        """@builders.configuration.GenerateBuilder
case class C(i: Int)
"""
      val cls = firstClass(code)
      val init = cls.mods.collectFirst { case Mod.Annot(i) => i }.get

      assert(GenerateBuildersRuleLogic.isGenerateBuilderAnnotation(init, ""))
    }

    test("non-target annotation is ignored") {
      val code =
        """@Deprecated
case class D(i: Int)
"""
      val cls = firstClass(code)
      val init = cls.mods.collectFirst { case Mod.Annot(i) => i }.get

      assert(!GenerateBuildersRuleLogic.isGenerateBuilderAnnotation(init, ""))
    }
  }
}
