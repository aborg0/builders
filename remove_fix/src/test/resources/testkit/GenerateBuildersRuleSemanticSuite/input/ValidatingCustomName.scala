package testkit.validating

/*
rules = ["class:builders.scalafix.GenerateBuildersRule"]
*/

import scala.annotation.StaticAnnotation

object BuilderStyle {
  val Validating: String = "Validating"
}

final class GenerateBuilder(
  val style: String = "Validating",
  val builderMethodName: String = "builder",
  val generateExtraVariants: Boolean = true
) extends StaticAnnotation

@GenerateBuilder(style = BuilderStyle.Validating, builderMethodName = "make", generateExtraVariants = false)
case class NamedUser(id: Int, code: String)
