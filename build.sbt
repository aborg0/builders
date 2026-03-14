ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"
ThisBuild / crossScalaVersions := Seq("3.7.4", "3.8.1")

// A new simple_builders subproject holds the sources previously at the root `src` directory.
lazy val simple_builders = (project in file("simple_builders"))
  .settings(
    name := "simple_builders",
    scalacOptions ++= Seq(
      "-Xprint-inline",
      "-Xmax-inlines", "1000"
    ),
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.9.5" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

// The root project is now an empty aggregator. It does not publish; subprojects are the real modules.
lazy val root = (project in file("."))
  .aggregate(simple_builders, withPrelude, removeFix)
  .settings(
    name := "builders-root",
    publish / skip := true
  )

lazy val docs = project       // new documentation project
  .in(file("builders_docs")) // important: it must not be docs/
  .dependsOn(simple_builders)
  .settings(
    // docs should not be published as an artifact
    publish / skip := true
  )
  .enablePlugins(MdocPlugin)

lazy val withPrelude = project
  .in(file("with_zio_prelude"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-prelude" % "1.0.0-RC46",
      "com.kubuszok" %% "hearth" % "0.2.0",
      //compilerPlugin(("com.kubuszok" % "hearth" % "0.1.0").cross(CrossVersion.Patch)),
      "com.lihaoyi" %% "utest" % "0.9.5" % "test", // Scala-JVM
    ),
    scalacOptions ++= Seq(
      "--feature",
      "--deprecation",
      "--unchecked",
      "no-indent"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .dependsOn(simple_builders)

lazy val removeFix = project
  .in(file("remove_fix"))
  .settings(

  )
  .dependsOn(simple_builders)

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
