package api

import utest.*
import models.*
import playground.Opaque
import playground.Opaque.*
import zio.prelude.ZValidation

object ValidatedBuilderAllowTests extends TestSuite {
  val tests = Tests {
    test("SimpleValidated accept wrapped Op") {
      val opWrapped = Opaque.Op("Op").toOption.get
      val res: ZValidation[Nothing, String, models.SimpleValidated] =
        ValidatedBuilderGenerator.builderAllow[models.SimpleValidated].i(1).op(opWrapped)
      assert(res.isSuccess)
    }

    test("SimpleValidated primitive invalid still fails") {
      val res = ValidatedBuilderGenerator.builderAllow[models.SimpleValidated].i(99).op("NotOp")
      assert(res.isFailure)
    }

    test("MixedString accept wrapped values") {
      val opWrapped = Opaque.Op("Op").toOption.get
      val validWrapped = Opaque.ValidOp("Op").toEither.toOption.get
      val res = ValidatedBuilderGenerator.builderAllow[models.MixedString].i(1).op(opWrapped).valid(validWrapped)
      assert(res.isSuccess)
    }

    test("MixedThrowable primitive validation") {
      // primitive success
      val ok = ValidatedBuilderGenerator.builderAllow[models.MixedThrowable].i(2).ex("ok").op("Op")
      assert(ok.isSuccess)
      // primitive failure
      val bad = ValidatedBuilderGenerator.builderAllow[models.MixedThrowable].i(2).ex("bad").op("Op")
      assert(bad.isFailure)
      // wrapped success
      val exWrapped = models.ExThrowable.apply("ok").toEither.toOption.get
      val ok2 = ValidatedBuilderGenerator.builderAllow[models.MixedThrowable].i(2).ex(exWrapped).op("Op")
      assert(ok2.isSuccess)
    }

    test("MixedEnum primitive failure") {
      val miss = ValidatedBuilderGenerator.builderAllow[models.MixedEnum].i(3).code("miss").valid("Op")
      assert(miss.isFailure)
    }
  }
}

