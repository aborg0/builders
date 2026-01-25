# Validated Builder Generator - Implementation Summary

## What We Built

An automatic generator for validated builders in Scala 3 that:
- Discovers smart constructors (validation methods) at compile time
- Generates curried builder functions with proper named parameters
- Combines validations using ZIO Prelude's `validateWith`
- Supports both `Either` and `Validation` return types from smart constructors

## Architecture

### Core Components

1. **`SmartConstructor.scala`** - Type class for describing validation
2. **`ValidatorInfo.scala`** - Data structures holding discovered validation info
3. **`SmartConstructorDiscovery.scala`** - Compile-time discovery of validators
4. **`ValidatedBuilderGenerator.scala`** - Macro that generates the builders

### How It Works

#### Discovery Phase
```scala
def discoverValidator(fieldName: String, fieldType: TypeRepr): ValidatorInfo
```
For each field in a case class:
1. Check if the type has a companion object
2. Look for `apply` or `make` methods returning `Validation[E, T]` or `Either[E, T]`
3. Store information about how to validate this field

#### Generation Phase
```scala
inline def derived[T]: ValidatedBuilderGenerator[T]
```
1. Extract all fields from the case class
2. Discover validators for each field
3. Generate an **uncurried** function: `(p1, p2, ..., pn) => validateWith(...)(constructor)`
4. Use Scala's `.curried` to convert to curried form at runtime

#### Key Insight - Runtime Currying

Instead of generating nested lambdas at compile time (which causes scoping issues), we:
1. Generate a simple multi-parameter function
2. Let the standard library curry it for us
3. This avoids complex macro tree construction

```scala
// Generated code (conceptually):
val uncurried = (i: Int, op: String) => 
  ZValidation.validateWith(
    ZValidation.succeed[Int](i),
    ZValidation.fromEither(Opaque.Op.apply(op))
  )((vi, vop) => SimpleValidated(vi, vop))

uncurried.curried // (Int) => (String) => Validation[String, SimpleValidated]
```

## Usage Example

```scala
case class SimpleValidated(
  i: Int,
  op: Opaque.Op  // Has smart constructor: Op.apply(String): Either[String, Op]
)

object SimpleValidated {
  val validator = ValidatedBuilderGenerator.derived[SimpleValidated]
}

// Usage:
SimpleValidated.validator().i(42).op("Op")
// => Success(SimpleValidated(42, Op))

SimpleValidated.validator().i(42).op("Invalid")
// => Failure("'Invalid' is not Op.")
```

## Design Decisions

### Error Type Unification
- All validators use `String` as the error type
- Plain values without validation return `ZValidation[Nothing, Nothing, T]`
- We widen to `ZValidation[Nothing, String, T]` using `Typed()`

### Method Discovery Priority
1. `companion.apply` returning `Validation[E, T]`
2. `companion.make` returning `Validation[E, T]`
3. `companion.apply` returning `Either[E, T]`
4. No validation (identity)

### Limitations
- Currently supports up to 4 fields (limited by `Function4.curried`)
- All error types unified to `String`
- No support for pre-validated inputs yet (like `TripleValVal`)

## Technical Challenges Solved

### 1. Path-Dependent Types in Data Structures
**Problem**: Can't store `quotes.reflect.TypeRepr` in case classes  
**Solution**: Use `Any` as storage type, cast back in macro context

### 2. ZValidation Type Parameters
**Problem**: `Validation[E, A]` is alias for `ZValidation[Nothing, E, A]`  
**Solution**: Work with `ZValidation` directly, apply correct type parameters

### 3. validateWith Arity Discovery
**Problem**: Multiple overloads with 3 parameter lists `[types][values][function]`  
**Solution**: Match on second parameter list length

### 4. Nested Lambda Scoping
**Problem**: Inner lambdas can't reference outer parameters after tree construction  
**Solution**: Generate uncurried function, curry at runtime

## Future Enhancements

1. **Support more fields**: Use tuples or custom currying for >4 parameters
2. **Better error types**: Support error type inference/unification
3. **Pre-validated inputs**: Like `TripleValVal` that accepts both primitives and Validations
4. **Named tuple integration**: Better integration with Scala 3 named tuples
5. **Opaque type detection**: Better handling of opaque types and newtypes

## Testing

```scala
sbt withPrelude/test
```

Tests verify:
- Manual `TripleValO` implementation works
- Generated builder for `SimpleValidated` compiles and runs

## Files Modified/Created

### Created
- `with_zio_prelude/src/main/scala/api/SmartConstructor.scala`
- `with_zio_prelude/src/main/scala/api/ValidatorInfo.scala`
- `with_zio_prelude/src/main/scala/api/SmartConstructorDiscovery.scala`
- `with_zio_prelude/src/main/scala/api/ValidatedBuilderGenerator.scala`
- `with_zio_prelude/src/test/scala/api/ValidatedBuilderTest.scala`
- `with_zio_prelude/src/test/scala/models/SimpleValidated.scala`

### Lessons Learned

1. **Macro scoping is tricky**: References created in macros must be carefully managed
2. **Runtime solutions can be simpler**: Don't do everything at compile time
3. **Standard library helps**: Use existing functions like `.curried` when possible
4. **Type parameters matter**: ZIO Prelude has 3 type params (W, E, A), not just 2
5. **Debugging macros is hard**: Use simple test cases and `report.errorAndAbort` for debugging
