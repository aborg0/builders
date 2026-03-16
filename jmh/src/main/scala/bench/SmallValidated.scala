package bench

import playground._
import zio.prelude.Validation

final case class SmallValidated(i: Int, op: Opaque.Op, v: Opaque.ValidOp)

object SmallValidatedManual {
  // Manual builder that mirrors how ValidatedBuilderGenerator would validate inputs
  def apply(): (i: Int => (op: String => (v: String => Validation[String, SmallValidated]))) =
    (i = (i: Int) =>
      (op = (op: String) =>
        (v = (v: String) =>
          Validation.validateWith(
            Validation.succeed(i),
            Validation.fromEither(Opaque.Op(op)),
            Opaque.ValidOp(v)
          )((ii, oop, vv) => SmallValidated(ii, oop, vv)))))
}

