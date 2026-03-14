package api

import scala.reflect.Selectable.reflectiveSelectable

object TypeInferenceTest {
  // Test what type gets inferred
  
  val testObj: { def apply(): String } = new {
    def apply(): String = "hello"
  }
  
  // Can we call it?
  val result1 = testObj.apply() // Direct call works
  val result2 = testObj()       // Does this work?
  
  // What if we use Any?
  val testAny: Any = new {
    def apply(): String = "world"
  }
  
  // val result3 = testAny() // This won't work - Any doesn't have apply
  
  // What about with asInstanceOf?
  val testCast = testAny.asInstanceOf[{ def apply(): String }]
  val result4 = testCast() // Does this work?
}
