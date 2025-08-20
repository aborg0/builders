ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.2"

lazy val root = (project in file("."))
  .settings(
    name := "builders",
    scalacOptions ++= Seq(
      "-Xprint-inline",
      "-Xmax-inlines", "1000"
    ),
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.9.1" % "test", // Scala-JVM

    testFrameworks += new TestFramework("utest.runner.Framework")
  )
