package builders.scalafix

import utest._

object GenerateBuilderAnnotationMatcherTests extends TestSuite {
  val tests: Tests = Tests {
    test("matches canonical class symbol") {
      assert(
        GenerateBuilderAnnotationMatcher.matches(
          "builders/configuration/GenerateBuilder#",
          "GenerateBuilder"
        )
      )
    }

    test("matches constructor symbol") {
      assert(
        GenerateBuilderAnnotationMatcher.matches(
          "builders/configuration/GenerateBuilder#`<init>`().",
          "GenerateBuilder"
        )
      )
    }

    test("matches normalized symbol fallback") {
      assert(
        GenerateBuilderAnnotationMatcher.matches(
          "builders/configuration/GenerateBuilder#something",
          "GenerateBuilder"
        )
      )
    }

    test("matches annotation-name fallback") {
      assert(
        GenerateBuilderAnnotationMatcher.matches(
          "",
          "builders.configuration.GenerateBuilder"
        )
      )
    }

    test("does not match unrelated annotation") {
      assert(!GenerateBuilderAnnotationMatcher.matches("foo/bar/Baz#", "Baz"))
    }
  }
}
