package api

import zio.prelude.Subtype
import zio.prelude.Assertion.greaterThanOrEqualTo

object ValidationPathIndex extends Subtype[Int] {
  override inline def assertion = greaterThanOrEqualTo(0)
}

type ValidationPathIndex = ValidationPathIndex.Type

final case class ValidationPathConfig(
  customPrefix: Option[String] = None
)

enum ValidationPathPart {
  case Custom(prefix: String)
  case Name(name: String)
  case Index(zeroIndex: ValidationPathIndex)
}

final case class ValidationPathError[+E](path: Seq[ValidationPathPart], error: E)
