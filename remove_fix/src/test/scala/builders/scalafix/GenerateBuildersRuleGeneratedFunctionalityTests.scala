package builders.scalafix

import java.nio.charset.StandardCharsets
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.SimpleFileVisitor
import java.nio.file.StandardCopyOption
import java.nio.file.attribute.BasicFileAttributes

import scala.jdk.CollectionConverters._

import utest._

object GenerateBuildersRuleGeneratedFunctionalityTests extends TestSuite {
  private val repoRoot: Path = Paths.get("").toAbsolutePath.normalize()
  private val fixtureTemplate: Path =
    repoRoot
      .resolve("remove_fix")
      .resolve("src")
      .resolve("test")
      .resolve("resources")
      .resolve("e2e")
      .resolve("generated_builder_fixture")

  val tests: Tests = Tests {
    test("scala3 fixture builders work after sbt scalafix") {
      val tempDir = Files.createTempDirectory("builders-scalafix-e2e-")

      try {
        copyDirectory(fixtureTemplate, tempDir)

        val result = runSbtFixture(tempDir)

        if (result.exitCode != 0) {
          throw new java.lang.AssertionError(result.output)
        }

        val rewritten =
          Files.readString(
            tempDir.resolve("src").resolve("main").resolve("scala").resolve("e2e").resolve("GeneratedBuilders.scala"),
            StandardCharsets.UTF_8
          )

        assert(rewritten.contains("object ValidatedUser"))
        assert(rewritten.contains("def builder: Builder = api.ValidatedBuilderGenerator.builder[ValidatedUser]"))
        assert(rewritten.contains("private type Builder = api.ValidatedBuilderSelectable[ValidatedUser, ?, (id: Int, code: String)]"))
        assert(rewritten.contains("private type AfterStep1 = api.ValidatedBuilderSelectable[ValidatedUser, ?, (code: CodeInput)]"))
        assert(rewritten.contains("current.`id`(input)"))
        assert(rewritten.contains("current.`code`(input)"))
      } finally {
        deleteRecursively(tempDir)
      }
    }
  }

  private final case class CommandResult(exitCode: Int, output: String)

  private def runSbtFixture(workingDirectory: Path): CommandResult = {
    val command =
      if (System.getProperty("os.name").toLowerCase(java.util.Locale.ROOT).contains("win")) {
        List("cmd", "/c", "sbt", "--no-colors", "--batch", "clean", "applyGenerateBuilders", "test")
      } else {
        List("sbt", "--no-colors", "--batch", "clean", "applyGenerateBuilders", "test")
      }

    val processBuilder = new ProcessBuilder(command: _*)
    processBuilder.directory(workingDirectory.toFile)
    processBuilder.redirectErrorStream(true)
    processBuilder.environment().put("BUILDERS_REPO_ROOT", repoRoot.toString)

    val process = processBuilder.start()
    val output = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exitCode = process.waitFor()

    CommandResult(exitCode, output)
  }

  private def copyDirectory(source: Path, target: Path): Unit = {
    Files.walkFileTree(
      source,
      new SimpleFileVisitor[Path]() {
        override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
          val relative = source.relativize(dir)
          Files.createDirectories(target.resolve(relative))
          FileVisitResult.CONTINUE
        }

        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          val relative = source.relativize(file)
          Files.copy(file, target.resolve(relative), StandardCopyOption.REPLACE_EXISTING)
          FileVisitResult.CONTINUE
        }
      }
    )
  }

  private def deleteRecursively(path: Path): Unit = {
    if (!Files.exists(path)) {
      return
    }

    Files.walk(path).iterator().asScala.toList.reverse.foreach { current =>
      Files.deleteIfExists(current)
    }
  }
}