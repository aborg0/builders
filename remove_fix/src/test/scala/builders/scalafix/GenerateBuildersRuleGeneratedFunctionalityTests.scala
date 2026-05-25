package builders.scalafix

import java.nio.charset.StandardCharsets
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.SimpleFileVisitor
import java.nio.file.StandardCopyOption
import java.nio.file.attribute.BasicFileAttributes
import java.util.jar.JarEntry
import java.util.jar.JarOutputStream

import scala.jdk.CollectionConverters._

import utest._

object GenerateBuildersRuleGeneratedFunctionalityTests extends TestSuite {
  private val repoRoot: Path = Paths.get("").toAbsolutePath.normalize()
  private val removeFixTargetDir: Path = repoRoot.resolve("remove_fix").resolve("target").resolve("scala-2.13")
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
        assert(rewritten.contains("private val builderApi: Any = builder"))
        assert(rewritten.contains("def builder: Builder = builderState0()"))
        assert(rewritten.contains("type Builder = (id: IdInput => AfterStep1)"))
        assert(rewritten.contains("private type AfterStep1 = (code: CodeInput => AfterStep2)"))
        assert(rewritten.contains("(id = (idValue: IdInput) => builderState1(idValue))"))
        assert(rewritten.contains("validateId(idValue)"))
        assert(rewritten.contains("validateCode(codeValue)"))
        assert(rewritten.contains("object PerfValidatedUser"))
        assert(rewritten.contains("private val generatedCodeShape: builders.configuration.GeneratedCodeShape = builders.configuration.GeneratedCodeShape.Performance"))
        assert(rewritten.contains("private inline given codeSmartConstructor: (CodeInput => CodeValidation) ="))
        assert(rewritten.contains("zio.prelude.ZValidation.fromEither(OpaqueCode(codeValue)).asInstanceOf[CodeValidation]"))
      } finally {
        deleteRecursively(tempDir)
      }
    }
  }

  private final case class CommandResult(exitCode: Int, output: String)

  private def runSbtFixture(workingDirectory: Path): CommandResult = {
    prepareLocalScalafixRuleArtifact()

    runSbtCommand(
      directory = workingDirectory,
      commands = List("clean", "applyGenerateBuilders", "test"),
      extraEnv = Map("BUILDERS_REPO_ROOT" -> repoRoot.toString)
    )
  }

  private def runSbtCommand(directory: Path, commands: List[String], extraEnv: Map[String, String] = Map.empty): CommandResult = {
    val sbtCommand =
      if (System.getProperty("os.name").toLowerCase(java.util.Locale.ROOT).contains("win")) {
        "sbt.bat"
      } else {
        "sbt"
      }
    val baseCommand = List(sbtCommand, "--no-colors", "--batch") ++ commands
    val command = baseCommand

    val processBuilder = new ProcessBuilder(command: _*)
    processBuilder.directory(directory.toFile)
    processBuilder.redirectErrorStream(true)
    extraEnv.foreach { case (key, value) =>
      processBuilder.environment().put(key, value)
    }

    val process = processBuilder.start()
    val output = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exitCode = process.waitFor()

    CommandResult(exitCode, output)
  }

  private def prepareLocalScalafixRuleArtifact(): Unit = {
    val localBase =
      Paths.get(System.getProperty("user.home"))
        .resolve(".ivy2")
        .resolve("local")
        .resolve("builders-scalafix-rules")
        .resolve("builders-scalafix-rules_2.13")
        .resolve("0.1.0-SNAPSHOT")
    val binaryJar = localBase.resolve("jars").resolve("builders-scalafix-rules_2.13.jar")
    val sourcesJar = localBase.resolve("srcs").resolve("builders-scalafix-rules_2.13-sources.jar")
    val javadocJar = localBase.resolve("docs").resolve("builders-scalafix-rules_2.13-javadoc.jar")
    val pomFile = localBase.resolve("poms").resolve("builders-scalafix-rules_2.13.pom")
    val ivyFile = localBase.resolve("ivys").resolve("ivy.xml")

    deleteRecursively(localBase)
    Files.createDirectories(binaryJar.getParent)
    Files.createDirectories(sourcesJar.getParent)
    Files.createDirectories(javadocJar.getParent)
    Files.createDirectories(pomFile.getParent)
    Files.createDirectories(ivyFile.getParent)

    writeJarFromClasses(removeFixTargetDir.resolve("classes"), binaryJar)
    copyIfExists(removeFixTargetDir.resolve("builders-scalafix-rules_2.13-0.1.0-SNAPSHOT-sources.jar"), sourcesJar)
    copyIfExists(removeFixTargetDir.resolve("builders-scalafix-rules_2.13-0.1.0-SNAPSHOT-javadoc.jar"), javadocJar)
    Files.copy(removeFixTargetDir.resolve("builders-scalafix-rules_2.13-0.1.0-SNAPSHOT.pom"), pomFile, StandardCopyOption.REPLACE_EXISTING)
    Files.copy(removeFixTargetDir.resolve("ivy-0.1.0-SNAPSHOT.xml"), ivyFile, StandardCopyOption.REPLACE_EXISTING)
  }

  private def copyIfExists(source: Path, target: Path): Unit = {
    if (Files.exists(source)) {
      Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING)
    }
  }

  private def writeJarFromClasses(classesDir: Path, jarPath: Path): Unit = {
    val output = new JarOutputStream(Files.newOutputStream(jarPath))
    try {
      Files.walk(classesDir).iterator().asScala
        .filter(Files.isRegularFile(_))
        .toList
        .sortBy(path => classesDir.relativize(path).toString)
        .foreach { path =>
          val entryName = classesDir.relativize(path).toString.replace('\\', '/')
          val entry = new JarEntry(entryName)
          output.putNextEntry(entry)
          Files.copy(path, output)
          output.closeEntry()
        }
    } finally {
      output.close()
    }
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