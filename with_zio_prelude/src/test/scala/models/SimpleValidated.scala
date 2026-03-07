package models

import api.ValidatedBuilderGenerator
import playground.Opaque

import java.time.LocalDate

/**
 * A simple test case class that uses types with validation.
 */
case class SimpleValidated(
                            i: Int,
                            op: Opaque.Op
                          )

object SimpleValidated {
  import Opaque.Op
  // The builder keeps the primitive input types and original field names,
  // so call sites can use `.i(...).op(...)` directly.
  val validator = ValidatedBuilderGenerator.builder[SimpleValidated]
}


final case class Simple(i: Int, s: String, d: LocalDate)

object Simple {
  val validator: ValidatedBuilderGenerator[Simple] = ValidatedBuilderGenerator.derived[Simple]
}