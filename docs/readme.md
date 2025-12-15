# Builders in Scala

Requires Scala 3.7 at least, as it uses named tuples.

Example usage:

```scala mdoc
enum Gender {
  case Female
  case Male
  case Other
}

final case class Person(firstName: String, lastName: String, gender: Gender)

import api.BuilderGeneratorSimplest
import api.BuilderGeneratorSimplest.given
import api.BuilderTypeClass

object Person extends BuilderGeneratorSimplest[Person]

val firstName = "Maria"
val personWithoutGender = Person.firstName(firstName).lastName("Doe")

val callToExternalService = Gender.Female
val person = personWithoutGender.gender(callToExternalService)
println(person)
```

You only need to extend `BuilderGeneratorSimplest[YourCaseClass]` and provide a `BuilderTypeClass` for that (for cardinality up to 10, there are helper methods that can generate the proper builders from the `apply` function).

You might want to customize the methods to require the arguments to be non-`null` in the builders (hint: `require`).

In case you want extra safety, you might want to disallow the `copy` method, constructor call, by making the constructor private:

```scala mdoc:fail
final case class SimpleHolder private(value: String)
import api.BuilderGeneratorSimplest
import api.BuilderGeneratorSimplest.given

object SimpleHolder extends BuilderGeneratorSimplest[SimpleHolder]

val h = SimpleHolder.value("Hello")
h.copy("sdf") // Does not compile
```

## Limitations
In case you depend on a type parameter in the construction, this approach requires to specify it for each type parameter you want to support for building or you need to use a `class` instead of an `object`, though this does not work currently:
```scala mdoc
final case class Holder[T](value: T)
import api.BuilderGeneratorSimplest
import api.BuilderGeneratorSimplest.given

// class HolderBuilder[T] extends BuilderGeneratorSimplest[Holder[T]]

// val h = new HolderBuilder[String].value("Hello")

```