addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.9.2" )
addSbtPlugin("ch.epfl.scala" % "sbt-missinglink" % "0.3.8")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.9")
libraryDependencies ++= List(
  "com.spotify" % "missinglink-core" % "0.2.11",
  "org.ow2.asm" % "asm-tree" % "9.9.1",
)