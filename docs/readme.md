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

Current generated companion shape (Scalafix rule):

- Validating/effect builders are emitted as named-tuple step chains, for example `(id: IdInput => AfterStep1)`.
- Smart-constructor givens are emitted as regular givens with explicit `apply` methods (not inline function-value aliases), to avoid Scala 3 E174 inline-given code-size warnings.
- Error-channel widening is expressed through `mapError` in generated validation flows, reducing broad cast usage in generated code.
- Validating/effect configuration includes `combineErrors`; effect mode additionally includes `effectFailureMode` and `effectExecutionMode`.
- Effect style now emits `zio.ZIO` results (not `zio.prelude.ZValidation`).
  - Smart constructors returning `ZValidation` are converted to `ZIO`.
  - Smart constructors returning `Either` are converted via `ZIO.fromEither`.
  - Smart constructors returning `ZIO` are used directly.
  - The resulting environment type is inferred from all field effects and combined as an intersection type (capturing the union of required capabilities).
  - `effectExecutionMode = EffectExecutionMode.Sequential` composes fields in order (`flatMap` / for-comprehension style).
  - `effectExecutionMode = EffectExecutionMode.Parallel` composes independent fields with `zipPar`.
- Generated code shape can be configured with `generatedCodeShape`:
  - `GeneratedCodeShape.Readable` emits explicit `apply` givens and `mapError` widening.
  - `GeneratedCodeShape.Performance` emits inline function-value givens and cast-oriented widening.

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

### Optional-like fields and `.!` completion

Validated builders support optional-like inputs for plain (no-smart-constructor) fields:

- `Option[T]`
  - `builder` accepts raw `T` and `None`
  - `builderAllow` accepts raw `T` and wrapped `Option[T]` (`Some`/`None`)
- Java optionals
  - `java.util.Optional[T]`, `java.util.OptionalInt`, `java.util.OptionalLong`, `java.util.OptionalDouble`
  - `builder` accepts raw values
  - `builderAllow` accepts both raw and wrapped optional values
- Nullable unions (`T | Null`) are treated as optional-like and can use `null`

When only trailing optional-like fields remain, you can complete with `.!`.
Omitted trailing values are synthesized as empty/default optional values:

- `Option[_]` -> `None`
- `java.util.Optional[_]` -> `Optional.empty()`
- `OptionalInt` -> `OptionalInt.empty()`
- `OptionalLong` -> `OptionalLong.empty()`
- `OptionalDouble` -> `OptionalDouble.empty()`
- `T | Null` -> `null`

`.!` is compile-time rejected if required fields still remain.

```scala
final case class Dummy(
  s: String,
  date: java.time.LocalDate | Null,
  i: Int,
  result: Option[Dummy]
)

val dummyBuilder = api.ValidatedBuilderGenerator.builder[Dummy]
val dummyBuilderAllow = api.ValidatedBuilderGenerator.builderAllow[Dummy]

dummyBuilder.s("hello").date(java.time.LocalDate.now).i(1).result(None)
dummyBuilder.s("hello").date(null).i(2).!
dummyBuilderAllow.s("hello").date(null).i(0).result(Some(Dummy("x", null, 1, None)))
```

### Path-aware validation failures

Validated builders can also attach path information to failures. Use `ValidationPathConfig`
to opt into path-aware errors.

```scala mdoc
import api.{ValidatedBuilderGenerator, ValidationPathConfig, ValidationPathError, ValidationPathPart}
import zio.prelude.ZValidation

opaque type PathOp = Int
object PathOp {
  def apply(value: Int): Either[String, PathOp] =
    value match {
      case 42 => Right(value: PathOp)
      case _ => Left(s"$value is not 42.")
    }
}

final case class PathDemo(name: String, right: PathOp)
object PathDemo {
  val validator = ValidatedBuilderGenerator.builder[PathDemo](
    ValidationPathConfig(customPrefix = Some("custom prefix"))
  )
}

val built: ZValidation[Nothing, ValidationPathError[String], PathDemo] =
  PathDemo.validator.name("inner").right(43)
```

For the same example, the path-aware failure value is:

```scala
ValidationPathError(
  path = Seq(
    ValidationPathPart.Custom("custom prefix"),
    ValidationPathPart.Field("right")
  ),
  error = "43 is not 42."
)
```

For the failing `right(43)` call, the failure path is:

```scala
Seq(
  ValidationPathPart.Custom("custom prefix"),
  ValidationPathPart.Field("right")
)
```

If a field is validated by another path-aware validated builder, the outer path is prepended to
the inner one. For example, a failure in an outer `child` field whose inner builder fails at
`value` becomes:

```scala
Seq(
  ValidationPathPart.Custom("outer prefix"),
  ValidationPathPart.Field("child"),
  ValidationPathPart.Custom("inner prefix"),
  ValidationPathPart.Field("value")
)
```

When a case class has no validating fields, `ValidationPathConfig` does not change the error
type; it remains `Nothing`.

For mixed-validator builders, the wrapped error type `E` can be a union. For example, if the
unified error type is `String | Throwable`, the path-aware error channel becomes
`ValidationPathError[String | Throwable]`.

#### Collection fields (Seq/List/Set/Vector/Map)

For collection fields whose element/value type is validated, generated setters accept both:

- pre-built collections, for example `Seq[Inner]` or `Map[String, Inner]`
- pre-validated collections, for example `Seq[ZValidation[Nothing, E, Inner]]` or
  `Map[String, ZValidation[Nothing, E, Inner]]`

When pre-validated collections fail, element/value failures are enriched with collection path
segments:

- `ValidationPathPart.Field("items")` for the collection field name
- `ValidationPathPart.Index(i)` for element position (or iterator order for `Map`)
- `ValidationPathPart.Named(...)` when available

Examples:

```scala
Seq(
  ValidationPathPart.Field("items"),
  ValidationPathPart.Index(ValidationPathIndex.wrap(0)),
  ValidationPathPart.Named(None),
  ValidationPathPart.Custom("inner prefix"),
  ValidationPathPart.Field("value")
)

Seq(
  ValidationPathPart.Field("parts"),
  ValidationPathPart.Index(ValidationPathIndex.wrap(0)),
  ValidationPathPart.Named(None),
  ValidationPathPart.Named(Some("left")),
  ValidationPathPart.Custom("inner prefix"),
  ValidationPathPart.Field("value")
)
```

Notes:

- For pre-validated element failures, `Named(None)` is expected when the element value is not
  available due to validation failure.
- For `Map`, a second `Named(Some(key.toString))` segment is appended to capture the key.
