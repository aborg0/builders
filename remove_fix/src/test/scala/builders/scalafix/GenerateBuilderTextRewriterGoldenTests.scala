package builders.scalafix

import java.nio.file.Files
import java.nio.file.Path
import utest._

object GenerateBuilderTextRewriterGoldenTests extends TestSuite {
  private val resourcesRoot = Path.of("remove_fix", "src", "test", "resources", "golden")

  val tests: Tests = Tests {
    test("simple style golden") {
      assertGolden("simple")
    }

    test("validating style golden") {
      assertGolden("validating")
    }

    test("effect style golden") {
      assertGolden("effect")
    }
  }

  private def assertGolden(caseName: String): Unit = {
    val inputPath = resourcesRoot.resolve(caseName).resolve("Input.scala")
    val expectedPath = resourcesRoot.resolve(caseName).resolve("Output.scala")

    val input = Files.readString(inputPath)
    val expected = normalize(Files.readString(expectedPath))
    val actual = normalize(GenerateBuilderTextRewriter.rewrite(input))

    assert(actual == expected)
  }

  private def normalize(value: String): String = {
    value.replace("\r\n", "\n").stripTrailing() + "\n"
  }
}
