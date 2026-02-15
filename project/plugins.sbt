addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.8.2" )
addSbtPlugin("ch.epfl.scala" % "sbt-missinglink" % "0.3.6")
libraryDependencies ++= List(
  "com.spotify" % "missinglink-core" % "0.2.11",
  "org.ow2.asm" % "asm-tree" % "9.9.1",
)