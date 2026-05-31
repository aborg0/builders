import _root_.scalafix.sbt.ScalafixPlugin.autoImport._

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"
ThisBuild / crossScalaVersions := Seq("3.7.4", "3.8.3")

// A new simple_builders subproject holds the sources previously at sthe root `src` directory.
lazy val simpleBuilders = (project in file("simple_builders"))
  .settings(
    name := "simpleBuilders",
    scalacOptions ++= Seq(
      "-Xprint-inline",
      "-Xmax-inlines", "1000"
    ),
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.9.5" % Test,
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

lazy val configuration = (project in file("configuration"))
  .settings(
    name := "builders-configuration",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.9.5" % Test,
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

// The root project is now an empty aggregator. It does not publish; subprojects are the real modules.
lazy val root = (project in file("."))
  .aggregate(simpleBuilders, configuration, withPrelude, removeFix, docs)
  .settings(
    name := "builders-root",
    publish / skip := true
  )

lazy val docs = project       // new documentation project
  .in(file("builders_docs")) // important: it must not be docs/
  .settings(
    // docs should not be published as an artifact
    publish / skip := true,
  )
  .dependsOn(simpleBuilders, withPrelude)
  .enablePlugins(MdocPlugin)

lazy val withPrelude = project
  .in(file("with_zio_prelude"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-prelude" % "1.0.0-RC47",
      "com.kubuszok" %% "hearth" % "0.2.0",
      //compilerPlugin(("com.kubuszok" % "hearth" % "0.1.0").cross(CrossVersion.Patch)),
      "com.lihaoyi" %% "utest" % "0.9.5" % Test, // Scala-JVM
    ),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-no-indent"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .dependsOn(simpleBuilders)

lazy val removeFix = project
  .in(file("remove_fix"))
  .settings(
    name := "builders-scalafix-rules",
    scalaVersion := "2.13.17",
    libraryDependencies ++= Seq(
      "ch.epfl.scala" %% "scalafix-core" % "0.14.4",
      "ch.epfl.scala" % "scalafix-testkit_2.13.17" % "0.14.4" % Test,
      compilerPlugin("org.scalameta" % "semanticdb-scalac_2.13.17" % "4.17.0" % Test),
      "com.lihaoyi" %% "utest" % "0.9.5" % Test,
      "dev.zio" %% "zio" % "2.1.26" % Test,
      "dev.zio" %% "zio-prelude" % "1.0.0-RC46" % Test
    ),
    Test / unmanagedSourceDirectories += (baseDirectory.value / "src" / "test" / "resources" / "testkit" / "GenerateBuildersRuleSemanticSuite" / "input"),
    Test / scalacOptions ++= Seq(
      "-Yrangepos",
      "-Xplugin-require:semanticdb",
      s"-P:semanticdb:targetroot:${(Test / classDirectory).value.getAbsolutePath}",
      s"-P:semanticdb:sourceroot:${(baseDirectory.value / "src" / "test" / "resources" / "testkit").getAbsolutePath}"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

// JMH microbenchmarks project
lazy val jmh = project
  .in(file("jmh"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "builders-jmh",
    // ensure same Scala version; ThisBuild already sets it but be explicit
    scalaVersion := "3.7.4",
    // Fork JVM for stable JMH runs
    Compile / fork := true,
    // Reasonable JVM options for benchmarking
    Compile / javaOptions ++= Seq("-Xms1G", "-Xmx2G", "-XX:+UseG1GC"),
    // Optional: set default jmh options when running via sbt (warmups, iters)
    // import sbtjmh.JmhPlugin.autoImport._ is not required here; users can pass args to jmh:run
  )
  .dependsOn(withPrelude, simpleBuilders)

lazy val example = project
  .in(file("example"))
  .settings(
    name := "builders-example",
    publish / skip := true,
    scalaVersion := "3.7.4",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "utest" % "0.9.5" % Test,
      "dev.zio" %% "zio-prelude" % "1.0.0-RC47",
    ),
    semanticdbEnabled := true,
    Compile / scalacOptions += "-Xsemanticdb",
    // Compile / scalacOptions += s"-sourceroot:${baseDirectory.value.getCanonicalPath}",
    scalafixDependencies += ("builders-scalafix-rules" %% "builders-scalafix-rules" % "0.1.0-SNAPSHOT")/* .cross(CrossVersion.for3Use2_13) */,
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .dependsOn(configuration)

// Add minimal GitHub Packages publishing configuration.
// It reads repository and credentials from environment variables provided by GitHub Actions
// (GITHUB_REPOSITORY, GITHUB_ACTOR, GITHUB_TOKEN). When running locally, set these env vars
// or configure your own credentials in ~/.sbt/1.0/credentials
publishMavenStyle := true
publishTo := {
  val repo = sys.env.get("GITHUB_REPOSITORY")
  repo.map { r =>
    // example repo format: owner/repo
    val host = s"https://maven.pkg.github.com/$r"
    "GitHub Packages" at host
  }
}
credentials += {
  val user = sys.env.getOrElse("GITHUB_ACTOR", "")
  val token = sys.env.getOrElse("GITHUB_TOKEN", "")
  // The realm/name used here should match how sbt resolves credentials for GitHub Packages.
  Credentials("GitHub Packages", "maven.pkg.github.com", user, token)
}
