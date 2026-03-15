package models

import api.ValidatedBuilderGenerator
import playground.Opaque
import zio.prelude.{Validation, ZValidation}

// ─── Arity 4 ────────────────────────────────────────────────────────────────

case class Quad(a: Int, b: String, op: Opaque.Op, valid: Opaque.ValidOp)
object Quad {
  val builder      = ValidatedBuilderGenerator.builder[Quad]
  val builderAllow = ValidatedBuilderGenerator.builderAllow[Quad]
}

// ─── Arity 5 ────────────────────────────────────────────────────────────────

case class Quint(a: Int, b: Int, c: String, op: Opaque.Op, valid: Opaque.ValidOp)
object Quint {
  val builder = ValidatedBuilderGenerator.builder[Quint]
}

// ─── Arity 6 (all plain, error = Nothing) ───────────────────────────────────

case class Plain6(a: Int, b: Int, c: Int, d: String, e: String, f: String)
object Plain6 {
  val builder = ValidatedBuilderGenerator.builder[Plain6]
}

// ─── Arity 7 ─────────────────────────────────────────────────────────────────

case class Seven(
  a: Int, b: Int, c: Int, d: Int,
  op1: Opaque.Op, op2: Opaque.Op, valid: Opaque.ValidOp
)
object Seven {
  val builder = ValidatedBuilderGenerator.builder[Seven]
}

// ─── Arity 10 ────────────────────────────────────────────────────────────────

case class Ten(
  a1: Int, a2: Int, a3: Int, a4: Int, a5: Int,
  a6: Int, a7: Int, a8: Int, a9: Int, a10: Int
)
object Ten {
  val builder = ValidatedBuilderGenerator.builder[Ten]
}

// ─── Arity 22 (maximum, all plain) ───────────────────────────────────────────

case class TwentyTwo(
  f01: Int, f02: Int, f03: Int, f04: Int, f05: Int, f06: Int, f07: Int,
  f08: Int, f09: Int, f10: Int, f11: Int, f12: Int, f13: Int, f14: Int,
  f15: Int, f16: Int, f17: Int, f18: Int, f19: Int, f20: Int, f21: Int, f22: Int
)
object TwentyTwo {
  val builder = ValidatedBuilderGenerator.builder[TwentyTwo]
}

