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

    test("builderAllow accepts wrapped Option values") {
      val some = OptionalInputDummy.validatorAllow.name("opt").maybe(Some(11))
      assert(some.isSuccess)
      assert(some.toEither.toOption.get == OptionalInputDummy("opt", Some(11)))

      val none = OptionalInputDummy.validatorAllow.name("opt").maybe(None)
      assert(none.isSuccess)
      assert(none.toEither.toOption.get == OptionalInputDummy("opt", None))
    }

    test("builderAllow accepts wrapped java Optional values") {
      val wrapped = JavaOptionalDummy.validatorAllow
        .name("x")
        .maybe(java.util.Optional.of("v"))
        .maybeInt(java.util.OptionalInt.of(1))
        .maybeLong(java.util.OptionalLong.of(2L))
        .maybeDouble(java.util.OptionalDouble.of(3.0))

      assert(wrapped.isSuccess)
      val built = wrapped.toEither.toOption.get
      assert(built.maybe.get == "v")
      assert(built.maybeInt.getAsInt == 1)
      assert(built.maybeLong.getAsLong == 2L)
      assert(built.maybeDouble.getAsDouble == 3.0)
    }

    test("builderAllowTyped provides typed staged allow entrypoint") {
      val wrapped = Opaque.Op("Op").toOption.get
      val typedOk = ValidatedBuilderGenerator.builderAllowTyped[models.SimpleValidated].i(1).op(wrapped)
      assert(typedOk.isSuccess)

      val typedBad = ValidatedBuilderGenerator.builderAllowTyped[models.SimpleValidated].i(1).op("NotOp")
      assert(typedBad.isFailure)
    }
  }
}

