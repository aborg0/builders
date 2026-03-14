package models

import api.ValidatedBuilderGenerator
import playground.Opaque

import zio.prelude.{Validation, ZValidation}

// Example enum-based error
enum ErrorCode {
  case NotFound, Invalid
}

// Opaque type with Throwable as error channel using ZValidation
opaque type ExThrowable = String
object ExThrowable {
  def apply(s: String): ZValidation[Nothing, Throwable, ExThrowable] =
    s match {
      case "ok" => Validation.succeed(s: ExThrowable)
      case other => Validation.fail(new IllegalArgumentException(s"Invalid ExThrowable: $other"))
    }
}

// Opaque type with enum-based Either error channel
opaque type CodeWrapped = String
object CodeWrapped {
  def apply(s: String): Either[ErrorCode, CodeWrapped] =
    s match {
      case "ok"  => Right(s: CodeWrapped)
      case "miss" => Left(ErrorCode.NotFound)
      case _      => Left(ErrorCode.Invalid)
    }
}

// Example 1: mix of no-validation (Int), Either[String, Op], and Validation[String, ValidOp]
case class MixedString(i: Int, op: Opaque.Op, valid: Opaque.ValidOp)
object MixedString {
  val validator = ValidatedBuilderGenerator.builder[MixedString]
}

// Example 2: mix including a Throwable error channel
case class MixedThrowable(i: Int, ex: ExThrowable, op: Opaque.Op)
object MixedThrowable {
  val validator = ValidatedBuilderGenerator.builder[MixedThrowable]
}

// Example 3: mix including an enum-based Either error channel
case class MixedEnum(i: Int, code: CodeWrapped, valid: Opaque.ValidOp)
object MixedEnum {
  val validator = ValidatedBuilderGenerator.builder[MixedEnum]
}

// Usage examples (compile-time examples; runtime usage may be demonstrated in REPL/tests):
// val v1 = MixedString.validator().i(1).op("Op").valid("Op") // ZValidation[...]
// val v2 = MixedThrowable.validator().i(2).ex("ok").op("Op")
// val v3 = MixedEnum.validator().i(3).code("ok").valid("Op")
