package builders.scalafix

import scala.meta._

object GenerateBuildersRuleLogic {
  def isGenerateBuilderAnnotation(init: Init, symbolValue: String): Boolean = {
    GenerateBuilderAnnotationMatcher.matches(symbolValue, annotationName(init))
  }

  def annotationArguments(init: Init): Map[String, String] = {
    init.argClauses.flatMap(_.values).flatMap {
      case Term.Assign(Term.Name(key), value) =>
        Some(key -> renderValue(value))
      case _ =>
        None
    }.toMap
  }

  def annotationName(init: Init): String = {
    init.tpe match {
      case Type.Name(value) =>
        value
      case Type.Select(_, Type.Name(value)) =>
        value
      case other =>
        other.syntax
    }
  }

  private def renderValue(term: Term): String = {
    term match {
      case Term.Select(qual, Term.Name(name)) =>
        s"${qual.syntax}.$name"
      case Term.Name(name) =>
        name
      case Lit.String(value) =>
        value
      case other =>
        other.syntax
    }
  }
}
