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
import api.BuilderGeneratorSimplest.caseclass3

object Person extends BuilderGeneratorSimplest[Person]//(using BuilderTypeClass[Person](caseclass3(Person.apply)))

val firstName = "Maria"
val personWithoutGender = Person.builder.firstName(firstName).lastName("Doe")

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
import api.BuilderTypeClass
import api.BuilderGeneratorSimplest.caseclass1

object SimpleHolder extends BuilderGeneratorSimplest[SimpleHolder]//(using BuilderTypeClass[SimpleHolder](caseclass1(SimpleHolder.apply)))

val h = SimpleHolder.builder.value("Hello")
h.copy("sdf") // Does not compile
```

## Limitations
In case you depend on a type parameter in the construction, this approach requires to specify it for each type parameter you want to support for building or you need to use a `class` instead of an `object`:
```scala mdoc
//final case class Holder[T](value: T)
//import api.BuilderGeneratorGeneric1
//import api.BuilderGeneratorSimplest.given
//import api.BuilderTypeClass
//import api.BuilderGeneratorSimplest.caseclass1
//
//object HolderBuilder extends BuilderGeneratorGeneric1[Holder]//(using BuilderTypeClass[Holder[T]](caseclass1(Holder.apply)))
//
//val h = HolderBuilder.builder[String].value("Hello")

```

## Validated builders (with zio-prelude)

This project includes a macro-based validated builder that integrates with zio-prelude smart
constructors. By default the macro keeps the original strict behaviour: builder parameters are
the primitive (unwrapped) types and the macro calls the smart constructor to validate them.

If you want callers to be able to pass either the primitive input (e.g. String) or an already-
constructed wrapped value (e.g. an opaque type instance), use the per-call opt-in APIs
`builderAllow` / `derivedAllow`.

First, a minimal demo using the existing `playground.Opaque` smart constructors. We define a
local case class to demonstrate how the macro works without depending on test-only `models`.

```scala mdoc
import api.ValidatedBuilderGenerator
import playground.Opaque

// A tiny case class that uses the Opaque.Op wrapped type
final case class Demo(i: Int, op: Opaque.Op)
object Demo {
  // The default builder (strict validation)
  val validator = ValidatedBuilderGenerator.builder[Demo]
}

// Use the validator by calling fields by name. The builder validates primitive inputs
// (here: String -> Opaque.Op) using Opaque.Op.apply.
Demo.validator.i(1).op("Op")
```

The next example demonstrates the opt-in union-accepting builder that also accepts already-
wrapped values.

```scala mdoc
import api.ValidatedBuilderGenerator
import playground.Opaque

final case class Demo2(i: Int, op: Opaque.Op)
object Demo2 {
  // Opt-in builder that accepts either primitive or wrapped values for the Op field
  val validatorAllow = ValidatedBuilderGenerator.builderAllow[Demo2]
}

// Create a wrapped value using the smart constructor (concise form)
val wrapped = Opaque.Op("Op").toOption.get

// Pass the already-wrapped value directly to the builderAllow
Demo2.validatorAllow.i(2).op(wrapped)
```

You can mix fields that have different validation styles (Either-based, ZValidation-based,
plain types) — the macro computes a unified error type automatically. If you prefer the strict
behaviour, use `builder` / `derived`; if you want callers to be able to pass already-wrapped
values too, use `builderAllow` / `derivedAllow`.
