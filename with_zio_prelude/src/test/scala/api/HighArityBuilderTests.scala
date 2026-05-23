package api

import models.*
import playground.Opaque
import utest.*
import zio.prelude.ZValidation

/**
 * Tests for builders with arity > 3.
 * Covers arity 4, 5, 6, 7, 10, and 22.
 */
object HighArityBuilderTests extends TestSuite {

  val tests = Tests {

    // ── Arity 4 ────────────────────────────────────────────────────────────

    test("arity4 success") {
      val result = Quad.builder.a(1).b("hello").op("Op").valid("Op")
      assert(result.isSuccess)
      val q = result.toEither.toOption.get
      assert(q.a == 1)
      assert(q.b == "hello")
    }

    test("arity4 first field fails") {
      // op field: "NotOp" is invalid
      val result = Quad.builder.a(2).b("x").op("NotOp").valid("Op")
      assert(result.isFailure)
    }

    test("arity4 second field fails") {
      // valid field: "NotOp" is invalid for ValidOp
      val result = Quad.builder.a(2).b("x").op("Op").valid("NotOp")
      assert(result.isFailure)
    }

    test("arity4 both validated fields fail — errors accumulated") {
      val result = Quad.builder.a(2).b("x").op("NotOp").valid("NotOp")
      assert(result.isFailure)
      // Both errors should be accumulated (ZValidation semantics)
      val errors = result.toEither.left.toOption.get
      assert(errors.size > 1)
    }

    test("arity4 allow — pre-wrapped Op accepted") {
      val wrappedOp    = Opaque.Op("Op").toOption.get
      val wrappedValid = Opaque.ValidOp("Op").toEither.toOption.get
      val result = Quad.builderAllow.a(3).b("y").op(wrappedOp).valid(wrappedValid)
      assert(result.isSuccess)
    }

    test("arity4 error type is String") {
      val result: ZValidation[Nothing, String, Quad] =
        Quad.builder.a(1).b("b").op("Op").valid("Op")
      assert(result.isSuccess)
    }

    // ── Arity 5 ────────────────────────────────────────────────────────────

    test("arity5 success") {
      val result = Quint.builder.a(1).b(2).c("c").op("Op").valid("Op")
      assert(result.isSuccess)
      val q = result.toEither.toOption.get
      assert(q.a == 1 && q.b == 2 && q.c == "c")
    }

    test("arity5 failure") {
      val result = Quint.builder.a(1).b(2).c("c").op("Bad").valid("Op")
      assert(result.isFailure)
    }

    // ── Arity 6 (all plain, Nothing error) ─────────────────────────────────

    test("arity6 plain all-Nothing success") {
      val result: ZValidation[Nothing, Nothing, Plain6] =
        Plain6.builder.a(1).b(2).c(3).d("d").e("e").f("f")
      assert(result.isSuccess)
      val p = result.toEither.toOption.get
      assert(p.a == 1 && p.f == "f")
    }

    // ── Arity 7 ────────────────────────────────────────────────────────────

    test("arity7 success") {
      val result = Seven.builder.a(1).b(2).c(3).d(4).op1("Op").op2("Op").valid("Op")
      assert(result.isSuccess)
    }

    test("arity7 multiple failures accumulated") {
      val result = Seven.builder.a(1).b(2).c(3).d(4).op1("Bad").op2("Bad").valid("Bad")
      assert(result.isFailure)
      val errors = result.toEither.left.toOption.get
      assert(errors.size == 3)
    }

    // ── Arity 10 (all plain) ───────────────────────────────────────────────

    test("arity10 success") {
      val result: ZValidation[Nothing, Nothing, Ten] =
        Ten.builder.a1(1).a2(2).a3(3).a4(4).a5(5).a6(6).a7(7).a8(8).a9(9).a10(10)
      assert(result.isSuccess)
      val t = result.toEither.toOption.get
      assert(t.a1 == 1 && t.a10 == 10)
    }

    // ── Arity 22 (maximum, all plain) ─────────────────────────────────────

    test("arity22 success") {
      val result: ZValidation[Nothing, Nothing, TwentyTwo] =
        TwentyTwo.builder
          .f01(1).f02(2).f03(3).f04(4).f05(5).f06(6).f07(7).f08(8).f09(9).f10(10)
          .f11(11).f12(12).f13(13).f14(14).f15(15).f16(16).f17(17).f18(18).f19(19).f20(20)
          .f21(21).f22(22)
      assert(result.isSuccess)
      val t = result.toEither.toOption.get
      assert(t.f01 == 1 && t.f22 == 22)
    }

    // ── Builder re-use (referential transparency) ──────────────────────────

    test("arity4 builder is reusable") {
      val b = Quad.builder
      val r1 = b.a(1).b("x").op("Op").valid("Op")
      val r2 = b.a(1).b("x").op("Op").valid("Op")
      assert(r1.isSuccess && r2.isSuccess)
      assert(r1.toEither == r2.toEither)
    }
  }
}

