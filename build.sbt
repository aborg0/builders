ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"

lazy val root = (project in file("."))
  .settings(
    name := "builders",
    scalacOptions ++= Seq(
      "-Xprint-inline",
      "-Xmax-inlines", "1000",
//      "-P:hearth.cross-quotes:logging=true",
    ),
    libraryDependencies ++= Seq(
      "com.kubuszok" %% "hearth" % "0.1.0",
      //compilerPlugin(("com.kubuszok" % "hearth" % "0.1.0").cross(CrossVersion.Patch)),
      "com.lihaoyi" %% "utest" % "0.9.1" % "test", // Scala-JVM
    ),

    testFrameworks += new TestFramework("utest.runner.Framework")
  )

lazy val docs = project       // new documentation project
  .in(file("builders_docs")) // important: it must not be docs/
  .dependsOn(root)
  .enablePlugins(MdocPlugin)