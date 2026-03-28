package models

import api.ValidatedBuilderGenerator

import java.time.LocalDate

final case class OptionalInputDummy(name: String, maybe: Option[Int])

object OptionalInputDummy {
  val validator = ValidatedBuilderGenerator.builder[OptionalInputDummy]
  val validatorAllow = ValidatedBuilderGenerator.builderAllow[OptionalInputDummy]
}

final case class TrailingOptionalDummy(name: String, i: Int, date: LocalDate | Null, result: Option[Int])

object TrailingOptionalDummy {
  val validator = ValidatedBuilderGenerator.builder[TrailingOptionalDummy]
  val validatorAllow = ValidatedBuilderGenerator.builderAllow[TrailingOptionalDummy]
}

final case class JavaOptionalDummy(
  name: String,
  maybe: java.util.Optional[String],
  maybeInt: java.util.OptionalInt,
  maybeLong: java.util.OptionalLong,
  maybeDouble: java.util.OptionalDouble
)

object JavaOptionalDummy {
  val validator = ValidatedBuilderGenerator.builder[JavaOptionalDummy]
  val validatorAllow = ValidatedBuilderGenerator.builderAllow[JavaOptionalDummy]
}
