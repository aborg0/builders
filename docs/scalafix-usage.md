# Scalafix Builder Generation - Usage

This document focuses on using the generated builders in application code.

## 1. Add The Annotation

```scala
import builders.configuration.*

@GenerateBuilder(style = BuilderStyle.Simple)
case class User(id: Int, name: String)
```

You can also choose validating/effect styles and set option values explicitly:

```scala
import builders.configuration.*

@GenerateBuilder(
  style = BuilderStyle.Validating,
  primitivePolicy = PrimitivePolicy.PrimitiveAndWrappedIfDerivable,
  pathMode = PathMode.FullCollectionAware,
  effectMode = EffectMode.ValidationOnly,
  conversionMode = ConversionMode.SynthesizeIfMissing,
  staleCheckMode = StaleCheckMode.SignatureHash,
  mergeMode = MergeMode.GeneratedRegionOnly,
  builderMethodName = "builder",
  generateExtraVariants = true,
  simpleOptionalValues = SimpleOptionalValues.ExplicitOptionalValues
)
case class ValidatedUser(id: Int, code: String)
```

### Simple Optional Value Strategies

For simple builders, `simpleOptionalValues` controls how optional-like fields can be omitted:

1. `SimpleOptionalValues.ExplicitOptionalValues` (default)
  - strict linear builder chain
  - optional-like fields must be provided explicitly
2. `SimpleOptionalValues.OptionalValuesFromDefaults`
  - enables omission and `build()` completion
  - omitted optional-like fields use case-class constructor default values
3. `SimpleOptionalValues.OptionalValuesWithEmptyDefaults`
  - enables omission and `build()` completion
  - omitted optional-like fields use generated empty/null defaults (`None`, `Optional.empty()`, `null`, etc.)

## 2. Run Scalafix

Run the rule so companions are generated for annotated case classes.

Example command:

```bash
sbt --no-colors "scalafixAll GenerateBuildersRule"
```

If you run Scalafix outside sbt, use the equivalent rule invocation for your build tooling and ensure SemanticDB is enabled.

## 3. Use Generated Public Builder Entry Methods

After rewrite, generated companions expose public builder entry methods:

1. Simple style: `builder`
2. Validating style: `builder`, `builderAllow`, `builderNoAllow`
3. Effect style: `builder`, `builderEffect`

Example (simple style):

```scala
val userBuilder = User.builder
// val user: User = userBuilder.id(3).name("Bob")
```

Example (validating style):

```scala
val strict = ValidatedUser.builderNoAllow
val relaxed = ValidatedUser.builderAllow
```

## 4. What Is Generated Privately

The generated companion also contains private helper members for typed step progression:

1. Typed per-field aliases (`IdInput`, `AfterStepN`)
2. Step functions (`fieldStepN...`)
3. Internal composition helpers (`buildFromValues` / `buildValidationFromValues` / `buildEffectFromValues`)

These internal helpers are implementation details and are intentionally not part of the public API.

## 5. End-To-End Example

Input:

```scala
import builders.configuration.*

@GenerateBuilder(style = BuilderStyle.Simple)
case class User(id: Int, name: String)
```

After Scalafix (simplified):

```scala
object User {
  def builder: api.BuilderGeneratorSimplest.Builder[User] = ...
  // plus private typed chain helpers
}
```

Then in application code:

```scala
val b = User.builder
// val user: User = b.id(3).name("Bob")
```
