package api

import utest.*
import playground.Opaque
import zio.prelude.ZValidation

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
      // TODO: Create a test case class and use ValidatedBuilderGenerator
      // This will test the macro generation
    }
  }
}
