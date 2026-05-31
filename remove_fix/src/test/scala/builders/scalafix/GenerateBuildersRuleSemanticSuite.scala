package builders.scalafix

import org.scalatest.funsuite.AnyFunSuiteLike
import scalafix.testkit.AbstractSemanticRuleSuite

class GenerateBuildersRuleSemanticSuite extends AbstractSemanticRuleSuite with AnyFunSuiteLike {
  runAllTests()
}
