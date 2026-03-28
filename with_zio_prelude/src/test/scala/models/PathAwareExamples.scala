package models

import api.{ValidatedBuilderGenerator, ValidationPathConfig, ValidationPathError, ValidationPathPart}
import zio.prelude.ZValidation

opaque type PathOp = Int

object PathOp {
  def apply(value: Int): Either[String, PathOp] =
    value match {
      case 42 => Right(value: PathOp)
      case _ => Left(s"$value is not 42.")
    }
}

final case class PathAwareDummy(name: String, right: PathOp)

object PathAwareDummy {
  val validator = ValidatedBuilderGenerator.builder[PathAwareDummy](ValidationPathConfig(customPrefix = Some("custom prefix")))
  val defaultValidator = ValidatedBuilderGenerator.builder[PathAwareDummy]
}

final case class NestedInner(value: PathOp)

object NestedInner {
  val validator = ValidatedBuilderGenerator.builder[NestedInner](ValidationPathConfig(customPrefix = Some("inner prefix")))

  def make(value: Int): ZValidation[Nothing, ValidationPathError[String], NestedInner] =
    validator.value(value)
}

final case class NestedOuter(name: String, child: NestedInner, right: PathOp)

object NestedOuter {
  val validator = ValidatedBuilderGenerator.builder[NestedOuter](ValidationPathConfig(customPrefix = Some("outer prefix")))
}

final case class PlainPathDummy(name: String, count: Int)

object PlainPathDummy {
  val validator = ValidatedBuilderGenerator.builder[PlainPathDummy](ValidationPathConfig(customPrefix = Some("plain prefix")))
  val defaultValidator = ValidatedBuilderGenerator.builder[PlainPathDummy]
}
