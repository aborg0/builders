ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"
ThisBuild / crossScalaVersions := Seq("3.7.4", "3.8.1")

lazy val root = (project in file("."))
  .settings(
    name := "builders",
    scalacOptions ++= Seq(
      "-Xprint-inline",
      "-Xmax-inlines", "1000"
    ),
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.9.5" % "test", // Scala-JVM

    testFrameworks += new TestFramework("utest.runner.Framework")
  )

lazy val docs = project       // new documentation project
  .in(file("builders_docs")) // important: it must not be docs/
  .dependsOn(root)
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
  .dependsOn(root)

lazy val removeFix = project
  .in(file("remove_fix"))
  .settings(

  )
