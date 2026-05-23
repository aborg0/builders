package playground

import api.ValidatedBuilderGenerator
import playground.Opaque.SequenceNumber
import scala.NamedTuple

import scala.reflect.Selectable.reflectiveSelectable

// Example case class with validation
case class Person(name: String, age: Int, email: String)

object Person {
  // val validator = ValidatedBuilderGenerator.derived[Person]
}

// Example with opaque types and validation
case class ValidatedTriple(i: Int, op: Opaque.Op, s: SequenceNumber)

object ValidatedTriple {
  // val validator = ValidatedBuilderGenerator.derived[ValidatedTriple]
}

object Example {
  def main(args: Array[String]): Unit = {
    println("=== Manual Implementations ===")
    println(TripleValO().i(3).op("Op").v("Op").s(2))
    println(TripleValVal().i(4).op("JOp").v("NonOp").s(-2))
    
                                                println("\n=== Generated Builder (no validation) ===")
    // Using NamedTuple-based builder produced by the generated validator
    // val personBuilder = Person.validator()
    // val person1 = personBuilder.name("Alice").age(30).email("alice@example.com")
    // println(s"Person 1: $person1")
    
        println("\n=== Generated Builder (with validation) ===")
    
    // val tripleBuilder = ValidatedTriple.validator()

/*    // Success case
    val success = tripleBuilder.i(42).op("Op").s(5)
    println(s"Success: $success")
    
    // Failure case - invalid op
    val failure1 = tripleBuilder.i(42).op("NotOp").s(5)
    println(s"Failure (bad op): $failure1")
    
    // Failure case - invalid sequence number
    val failure2 = tripleBuilder.i(42).op("Op").s(-1)
    println(s"Failure (negative seq): $failure2")
    
    // Multiple failures
    val failure3 = tripleBuilder.i(42).op("BadOp").s(-5)
    println(s"Failure (both bad): $failure3")*/
  }

}
