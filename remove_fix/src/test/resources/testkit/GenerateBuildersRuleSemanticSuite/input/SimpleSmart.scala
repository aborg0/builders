package testkit.simplesmart

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

case class Region private(code: String)
object Region {
  def apply(raw: String): Either[String, Region] = Right(new Region(raw))
}

@GenerateBuilder(style = BuilderStyle.Simple)
case class SimpleSmartUser(id: Int, region: Region)
