import _root_.scalafix.sbt.ScalafixPlugin.autoImport._

lazy val applyGenerateBuilders = taskKey[Unit]("Apply the custom GenerateBuildersRule to fixture sources")

ThisBuild / scalaVersion := "3.7.4"

val buildersRepoRoot = file(sys.env.getOrElse("BUILDERS_REPO_ROOT", sys.error("BUILDERS_REPO_ROOT must be set")))

lazy val simpleBuildersRef = ProjectRef(buildersRepoRoot, "simpleBuilders")
lazy val configurationRef = ProjectRef(buildersRepoRoot, "configuration")
lazy val withPreludeRef = ProjectRef(buildersRepoRoot, "withPrelude")
lazy val removeFixRef = ProjectRef(buildersRepoRoot, "removeFix")

lazy val root = (project in file("."))
  .dependsOn(simpleBuildersRef, configurationRef, withPreludeRef)
  .settings(
    name := "generated-builder-fixture",
    publish / skip := true,
    semanticdbEnabled := true,
    Compile / scalacOptions += "-Xsemanticdb",
    Compile / scalacOptions += s"-sourceroot:${baseDirectory.value.getCanonicalPath}",
    scalafixDependencies += "builders-scalafix-rules" % "builders-scalafix-rules_2.13" % "0.1.0-SNAPSHOT",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.9.5" % Test,
    testFrameworks += new TestFramework("utest.runner.Framework"),
    applyGenerateBuilders := {
      (removeFixRef / publishLocal).value
      (Compile / scalafix).toTask(" class:builders.scalafix.GenerateBuildersRule").value
    }
  )