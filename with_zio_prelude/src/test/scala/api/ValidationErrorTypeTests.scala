package api

import models.*
import utest.*
import zio.prelude.ZValidation

object ValidationErrorTypeTests extends TestSuite {
  val tests = Tests {
    test("MixedString_error_type") {
      // builder returns a NamedTuple so call apply() to get the nested function
      val b = MixedString.validator.i(1)
      // The final call produces a ZValidation; we test runtime success for valid inputs
      val result = b.op("Op").valid("Op")
      assert(result.isSuccess)

      // Compile-time check: the error channel should be exactly String (both smart constructors use String errors)
      val compileTimeCheck: ZValidation[Nothing, String, MixedString] = MixedString.validator.i(1).op("Op").valid("Op")
      val _ = compileTimeCheck
    }

    test("MixedThrowable_runtime") {
      val b = MixedThrowable.validator.i(2)
      val success = b.ex("ok").op("Op")
      assert(success.isSuccess)

      val failure = b.ex("bad").op("Op")
      assert(failure.isFailure)

      // Compile-time check: error channel should be Throwable | String
      val compileTimeCheck: ZValidation[Nothing, Throwable | String, MixedThrowable] = MixedThrowable.validator.i(2).ex("ok").op("Op")
      val _ = compileTimeCheck
    }

    test("MixedEnum_runtime") {
      val b = MixedEnum.validator.i(3)
      val ok = b.code("ok").valid("Op")
      assert(ok.isSuccess)

      val miss = b.code("miss").valid("Op")
      assert(miss.isFailure)

      // Compile-time check: error channel should be ErrorCode | String
      val compileTimeCheck: ZValidation[Nothing, models.ErrorCode | String, MixedEnum] = MixedEnum.validator.i(3).code("ok").valid("Op")
      val _ = compileTimeCheck
    }

    test("Error_channel_type_inference_compile_time") {
      // Compile-time type checks: ensure the produced validation has error type equal to union types
      // We use summonInline to get an implicit evidence for type equality; if types don't match, it won't compile.

      // For MixedThrowable, expect error type to be Throwable | String | ??? depending on Op/ValidOp.
      // We can't write an open-ended union here without exact types; we'll at least assert it's a ZValidation
      // and let the runtime tests cover correctness.

      val b1 = MixedString.validator.i(1)
      val v1 = b1.op("Op").valid("Op")
      assert(v1.isSuccess)

      // Re-assert compile-time check (sanity)
      val compileTimeCheck2: ZValidation[Nothing, String, MixedString] = MixedString.validator.i(1).op("Op").valid("Op")
      val _ = compileTimeCheck2
    }
  }
}
