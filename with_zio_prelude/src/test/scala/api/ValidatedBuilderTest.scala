package api

import models.{Simple, SimpleValidated}
import utest.*
import playground.Opaque
import zio.prelude.ZValidation

import scala.reflect.Selectable.reflectiveSelectable
import java.time.LocalDate
import api.ValidatedBuilderGenerator.ValidatedBuilder

object ValidatedBuilderTest extends TestSuite {

  val tests = Tests {
    test("TripleVal with manual implementation") {
      import playground.{TripleVal, TripleValO}
      
      // Test successful validation
      val result1 = TripleValO().i(3).op("Op").v("Op").s(2)
      assert(result1.isSuccess)
      
      // Test failed validation - invalid op
      val result2 = TripleValO().i(3).op("NotOp").v("Op").s(2)
      assert(result2.isFailure)
      
      // Test failed validation - invalid sequence number
      val result3 = TripleValO().i(3).op("Op").v("Op").s(-1)
      assert(result3.isFailure)
    }
    
    test("Simple case class with no validation") {
      val result: ZValidation[Nothing, String, Simple] =
        ValidatedBuilderGenerator.builder[Simple].i(2).s("@@").d(LocalDate.of(2026, 1, 24))
      assert(result.isSuccess)
    }

    test("SimpleValidated builder exposes named fields") {
      val builder = SimpleValidated.validator
      val afterI  = builder.i(42)
      val result: ZValidation[Nothing, String, SimpleValidated] = afterI.op("Op")

      assert(result.isSuccess)
      val sv1 = result.toEither.toOption.get
      assert(sv1.i == 42)
    }
    
    test("SimpleValidated with validation") {
      val validator = SimpleValidated.validator
      val result1 = validator.i(42).op("Op")
      assert(result1.isSuccess)
      val sv1 = result1.toEither.toOption.get
      assert(sv1.i == 42)

      val result2 = validator.i(99).op("NotOp")
      assert(result2.isFailure)
    }

    test("SimpleValidated rejects empty op string") {
      val validator = SimpleValidated.validator
      val result = validator.i(10).op("")
      assert(result.isFailure)
    }

    test("SimpleValidated preserves opaque value semantics") {
      val validator = SimpleValidated.validator
      val result = validator.i(1).op("Op")
      assert(result.isSuccess)

      val sv = result.toEither.toOption.get
      val direct = Opaque.Op("Op")
      assert(direct.isRight)
      val directOp = direct.toOption.get

      assert(sv.op == directOp)
    }

    test("Simple builder compiles to expected curried shape") {
      val d = LocalDate.of(2026, 1, 24)
      val result: ZValidation[Nothing, String, Simple] =
        ValidatedBuilderGenerator.builder[Simple].i(0).s("x").d(d)
      assert(result.isSuccess)
      val s = result.toEither.toOption.get
      assert(s.i == 0)
      assert(s.s == "x")
      assert(s.d == d)
    }

    test("SimpleValidated builder returns ZValidation with String error type") {
      val validator = SimpleValidated.validator
      val result: ZValidation[Nothing, String, SimpleValidated] = validator.i(123).op("NotOp")

      assert(result.isFailure)

      val errors = result.toEither.left.toOption.get
      assert(errors.nonEmpty)
    }

    test("SimpleValidated builder is referentially transparent (no hidden state)") {
      val validator = SimpleValidated.validator
      val res1 = validator.i(1).op("Op")
      val res2 = validator.i(1).op("Op")

      // Same inputs -> both success, and results equal
      assert(res1.isSuccess)
      assert(res2.isSuccess)
      assert(res1.toEither == res2.toEither)
    }
  }
}
