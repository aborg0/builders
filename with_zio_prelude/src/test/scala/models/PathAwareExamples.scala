package models

import api.{Name, ValidatedBuilderGenerator, ValidationPathConfig, ValidationPathError, ValidationPathPart}
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

final case class NamedInner(@Name id: String, value: PathOp)

object NamedInner {
  val validator = ValidatedBuilderGenerator.builder[NamedInner](ValidationPathConfig(customPrefix = Some("inner prefix")))

  def make(id: String, value: Int): ZValidation[Nothing, ValidationPathError[String], NamedInner] =
    validator.id(id).value(value)
}

final case class SeqContainer(name: String, items: Seq[NamedInner])

object SeqContainer {
  val validator = ValidatedBuilderGenerator.builder[SeqContainer](ValidationPathConfig(customPrefix = Some("seq prefix")))
}

final case class MapContainer(name: String, parts: Map[String, NamedInner])

object MapContainer {
  val validator = ValidatedBuilderGenerator.builder[MapContainer](ValidationPathConfig(customPrefix = Some("map prefix")))
}

final case class SetPlainContainer(name: String, items: Set[Int])

object SetPlainContainer {
  val validator = ValidatedBuilderGenerator.builder[SetPlainContainer]
}

final case class SetContainer(name: String, items: Set[NamedInner])

object SetContainer {
  val validator = ValidatedBuilderGenerator.builder[SetContainer](ValidationPathConfig(customPrefix = Some("set prefix")))
}
