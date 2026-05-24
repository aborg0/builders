package e2e

import api.ValidatedBuilderGenerator
import utest._

object GeneratedBuilderFunctionalityTests extends TestSuite {
  val tests: Tests = Tests {
    test("validating generated builder succeeds for valid input") {
      val result = ValidatedUser.builder.id(2).code("ZX")

      assert(result.isSuccess)
      assert(result.toEither.toOption.contains(ValidatedUser(2, "ZX")))
    }

    test("generated smart builder applies opaque constructors") {
      val builder = ValidatedBuilderGenerator.builder[OpaqueSmartUser]
      val success = builder.id(1).code("ok").op("Op")
      val codeFailure = builder.id(1).code("miss").op("Op")
      val opFailure = builder.id(1).code("ok").op("bad")

      assert(success.isSuccess)
      assert(codeFailure.isFailure)
      assert(opFailure.isFailure)
    }

    test("generated smart builder applies make-based constructors") {
      val builder = ValidatedBuilderGenerator.builder[MakeSmartUser]
      val success = builder.amount(3).label("hi")
      val failure = builder.amount(0).label("hi")

      assert(success.isSuccess)
      assert(failure.isFailure)
    }
  }
}