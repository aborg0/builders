package api

import utest.*
import zio.prelude.Validation
import zio.prelude.Assertion.greaterThanOrEqualTo
import api.ValidatedBuilderGenerator
import playground._

object SubtypeNewtypeTests extends TestSuite {

  object TestNew extends zio.prelude.Newtype[Int] {
    override inline def assertion = greaterThanOrEqualTo{0}
  }
  type TestNew = TestNew.Type
  case class WithNew(n: TestNew)

  object TestNew2 extends zio.prelude.Newtype[Int] {
    override inline def assertion = greaterThanOrEqualTo{0}
  }
  type TestNew2 = TestNew2.Type
  case class WithNew2(n: TestNew2)

  val tests = Tests {

    test("SequenceNumber Subtype works with generated builderNoAllow: valid primitive passes, invalid fails") {
      val gen = ValidatedBuilderGenerator.builderNoAllow[playground.TripleVal]

      val success = gen.i(1).op("Op").v("Op").s(5)
      assert(success.isSuccess)

      val fail = gen.i(1).op("Op").v("Op").s(-1)
      assert(fail.isFailure)
    }

    test("Custom Newtype with primitive inputs is supported (builderNoAllow runs make validation)") {
      val gen = ValidatedBuilderGenerator.builderNoAllow[WithNew]

      val ok = gen.n(3)
      assert(ok.isSuccess)

      val bad = gen.n(-5)
      assert(bad.isFailure)
    }

    test("SequenceNumber Subtype works with generated builderAllow using primitive values") {
      val gen = ValidatedBuilderGenerator.builderAllow[playground.TripleVal]

      val success = gen.i(1).op("Op").v("Op").s(5)
      assert(success.isSuccess)

      val fail = gen.i(1).op("Op").v("Op").s(-1)
      assert(fail.isFailure)
    }

    test("Custom Newtype with primitive inputs is supported (builderAllow)") {
      val gen = ValidatedBuilderGenerator.builderAllow[WithNew2]

      val ok = gen.n(3)
      assert(ok.isSuccess)

      val bad = gen.n(-5)
      assert(bad.isFailure)
    }

  }
}
