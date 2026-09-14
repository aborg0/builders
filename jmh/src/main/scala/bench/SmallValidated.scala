package bench

import benchmodel.BenchSmallValidated
import playground._
import zio.prelude.Validation

object SmallValidatedManual {
  // Manual builder that mirrors the chained builder API used by the generated builders:
  // manual.i(42).op("Op").v("Op") => Validation[String, BenchSmallValidated]
  def apply() = new Builder()

  class Builder {
    def i(i: Int) = new LevelI(i)
  }

  class LevelI(i: Int) {
    def op(op: String) = new LevelOp(i, op)
  }

  class LevelOp(i: Int, op: String) {
    def v(v: String): Validation[String, BenchSmallValidated] =
      Validation.validateWith(
        Validation.succeed(i),
        Validation.fromEither(Opaque.Op(op)),
        Opaque.ValidOp(v)
      )((ii, oop, vv) => BenchSmallValidated(ii, oop, vv))
  }
}
