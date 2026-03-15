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
  val validatorSmarter = ValidatedBuilderGenerator.builder[SimpleValidated]
  val validator = ValidatedBuilderGenerator.builderNoAllow[SimpleValidated]
}


final case class Simple(i: Int, s: String, d: LocalDate)

object Simple {
  val validatorSmarter = ValidatedBuilderGenerator.builder[Simple]
  val validator = ValidatedBuilderGenerator.builderNoAllow[Simple]
}