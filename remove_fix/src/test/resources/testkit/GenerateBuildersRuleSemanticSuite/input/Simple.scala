package testkit.simple

/*
rules = ["class:builders.scalafix.GenerateBuildersRule"]
*/

import scala.annotation.StaticAnnotation

object BuilderStyle {
  val Simple: String = "Simple"
}

final class GenerateBuilder(
  val style: String = "Simple",
  val builderMethodName: String = "builder",
  val generateExtraVariants: Boolean = true
) extends StaticAnnotation

@GenerateBuilder(style = BuilderStyle.Simple)
case class SimpleUser(id: Int, name: String)
