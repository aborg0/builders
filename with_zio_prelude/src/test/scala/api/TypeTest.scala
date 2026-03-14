package api

import playground.TripleValO
import scala.language.implicitConversions
import scala.reflect.Selectable.reflectiveSelectable

object TypeTest {
  // Let's see what type TripleValO has
  val manual = TripleValO
  
  // This should compile - TripleValO() returns the builder
  val result = TripleValO()
  
  // What's the type of manual?
  // It's an object, so calling () invokes apply()
  
  // Let's try to mimic this with a val
  val testVal = new {
    def apply(): String = "hello"
  }
  
  // Can we call testVal()?
  // val test = testVal() // This should work if the type is right
  
  // The key is that TripleValO is an object reference,
  // and object references have special treatment for apply()
  
  // Let's create a similar structure
  object TestObject {
    def apply(): String = "world"
  }
  
  val objRef = TestObject
  val test2 = objRef() // This works!
  
  // So the question is: can we make a val that acts like an object reference?
  // The answer: we need a structural type with Selectable
  
  val testStructural: { def apply(): String } = new {
    def apply(): String = "structural"
  }
  
  // Can we call it?
  val test3 = testStructural() // Direct method call works
  
  // Or with reflectiveSelectable:
  given Conversion[{ def apply(): String }, Selectable] = reflectiveSelectable
  val test4 = testStructural() // This should work with the conversion
  
  println(s"test2: $test2")
  println(s"test3: $test3")
}
