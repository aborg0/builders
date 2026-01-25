package api

import models.Simple
import utest.*
import playground.Opaque
import zio.prelude.ZValidation

import java.time.LocalDate

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
      // The validator.apply() returns the curried function
      // We need to help the compiler with types since apply() returns Any
      type Builder = Int => String => LocalDate => ZValidation[Nothing, String, Simple]
      val builder = Simple.validator.apply().asInstanceOf[Builder]
      val result  = builder(2)("@@")(LocalDate.of(2026, 1, 24))
      assert(result.isSuccess)
    }
    
    test("SimpleValidated with validation") {
      import models.SimpleValidated

      type Builder = Int => String => ZValidation[Nothing, String, SimpleValidated]
      val builder = SimpleValidated.validator().asInstanceOf[Builder]

      // Test successful validation
      val result1 = builder(42)("Op")
      assert(result1.isSuccess)
      val sv1 = result1.toEither.toOption.get
      assert(sv1.i == 42)

      // Test failed validation - "NotOp" should be rejected by Op.apply
      val result2 = builder(99)("NotOp")
      // Let's also test Op.apply directly to make sure it works
      val directTest = Opaque.Op("NotOp")
      assert(directTest.isLeft)
      assert(result2.isFailure)
    }

    // --- Additional tests below ---

    test("SimpleValidated rejects empty op string") {
      import models.SimpleValidated

      type Builder = Int => String => ZValidation[Nothing, String, SimpleValidated]
      val builder = SimpleValidated.validator().asInstanceOf[Builder]

      val result = builder(10)("")
      assert(result.isFailure)
    }

    test("SimpleValidated preserves opaque value semantics") {
      import models.SimpleValidated

      type Builder = Int => String => ZValidation[Nothing, String, SimpleValidated]
      val builder = SimpleValidated.validator().asInstanceOf[Builder]

      val result = builder(1)("Op")
      assert(result.isSuccess)

      val sv = result.toEither.toOption.get
      // Ensure the same opaque Op behavior as direct construction
      val direct = Opaque.Op("Op")
      assert(direct.isRight)
      val directOp = direct.toOption.get

      // Types should line up: sv.op should be the same underlying representation
      assert(sv.op == directOp)
    }

    test("Simple builder compiles to expected curried shape") {
      // This is mostly a type-level test: ensure we can ascribe the builder type
      type Builder = Int => String => LocalDate => ZValidation[Nothing, String, Simple]
      val builder = Simple.validator.apply().asInstanceOf[Builder]

      val d      = LocalDate.of(2026, 1, 24)
      val result = builder(0)("x")(d)
      assert(result.isSuccess)
      val s = result.toEither.toOption.get
      assert(s.i == 0)
      assert(s.s == "x")
      assert(s.d == d)
    }

    test("SimpleValidated builder returns ZValidation with String error type") {
      import models.SimpleValidated

      type Builder = Int => String => ZValidation[Nothing, String, SimpleValidated]
      val builder = SimpleValidated.validator().asInstanceOf[Builder]

      // Intentionally invalid to exercise error type plumbing
      val result = builder(123)("NotOp")

      // Assert the general shape: isFailure and error type = String
      assert(result.isFailure)

      // We don't care about exact message, but we can pattern match to ensure it's a String
      val errors = result.toEither.left.toOption.get
      // errors is a NonEmptyChunk[String] under the hood, so we can at least
      // check that there is at least one String
      assert(errors.head.isInstanceOf[String])
    }

    test("SimpleValidated builder is referentially transparent (no hidden state)") {
      import models.SimpleValidated

      type Builder = Int => String => ZValidation[Nothing, String, SimpleValidated]
      val builder = SimpleValidated.validator().asInstanceOf[Builder]

      val res1 = builder(1)("Op")
      val res2 = builder(1)("Op")

      // Same inputs -> both success, and results equal
      assert(res1.isSuccess)
      assert(res2.isSuccess)
      assert(res1.toEither == res2.toEither)
    }
  }
}

