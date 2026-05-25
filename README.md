# Builders

This repository contains the builders projects and documentation. The CI builds across multiple JDKs and Scala versions and publishes artifacts to GitHub Packages.

Configuration Artifact (Scalafix)
---------------------------------

The repository now includes a dedicated `configuration` subproject (`builders-configuration`).
This module contains only user-facing annotation and enum configuration APIs for builder generation.

Design intent:
- End users should depend on the configuration artifact plus the Scalafix rules artifact.
- End users should not need to depend on the runtime builder generator modules to configure generation.
- The annotation model uses enums (not string literals) to keep configuration explicit and type-safe.

Current implementation snapshot:
- `builders.configuration.GenerateBuilder` annotation added.
- Option enums added for style/policies/merge/stale-check behavior, including simple optional-value strategy controls.
- `remove_fix` is now a Scala 2.13 Scalafix rule module (required by available Scalafix artifacts).
- `remove_fix` contains:
  - `GenerateBuildersRule` (semantic Scalafix rule entrypoint)
  - option decoder with exhaustive option-decoding tests
  - golden and option-matrix rewrite tests for `simple`, `validating`, and `effect` generation styles

Docs
-----

Check [builders_docs/target/mdoc/readme.md](builders_docs/target/mdoc/readme.md), in case it does not exist, generate it with:

```sbt
--no-colors docs/mdoc
```

You can check the non-evaluated documentation at [docs/readme.md](docs/readme.md) as well.

Notes and recent changes
------------------------

- The macros now support `zio.prelude.Newtype` / `zio.prelude.Subtype` (and the `NewtypeCustom`/`SubtypeCustom` variants) in addition to `opaque type` wrappers. That means builders will discover and invoke `make`/`apply` on zio.prelude-style wrappers where appropriate.

- Semantics summary (short):
  - Generated validating/effect companions now use direct named-tuple step chains (for example `(id: IdInput => AfterStep1)`) rather than the previous `ValidatedBuilderSelectable` wrapper structure.
  - Generated smart-constructor givens in validating/effect companions now use explicit `apply` implementations instead of inline function-value aliases. This avoids Scala 3's E174 warning about inline given aliases increasing generated code size.
  - The renderer now prefers typed `mapError` widening over broad `asInstanceOf` casts in validating/effect paths. This keeps generated code safer and easier to reason about.
  - New validating/effect options are emitted and decoded:
    - `combineErrors` controls error-channel combination strategy.
    - `effectFailureMode` controls how effect-style builders handle validation failures (`Propagate`, `OrDie`, `OrElseProvided`).
    - `effectExecutionMode` controls effect-style composition strategy (`Sequential`, `Parallel`).
  - Effect-style generated builders now return `zio.ZIO` (instead of `zio.prelude.ZValidation`).
    - Smart constructors returning `ZValidation` are converted to `ZIO`.
    - Smart constructors returning `Either` are converted via `ZIO.fromEither`.
    - Smart constructors already returning `ZIO` are used directly.
    - Final effect environment is inferred from required field environments (combined as an intersection type, which represents the union of required capabilities).
  - Generated code shape is configurable via `generatedCodeShape`:
    - `Readable` favors explicit `apply`-based givens and typed `mapError` widening.
    - `Performance` favors inline function-value givens and cast-based widening.
  - `builderNoAllow` and `builderAllow` expect primitive inputs for fields that have discovered smart-constructors (they invoke the smart constructor such as `make`/`apply` to validate). In short: these builders validate primitive inputs.
  - `builderAllow` may accept already-wrapped values without re-validating only when the wrapped type has a distinct runtime representation (for example some wrapper classes/objects). For zio.prelude `Newtype`/`Subtype` (and other cases where the wrapped type is erased to the primitive at runtime) we treat them as primitives for validation purposes and do not bypass validation by passing the wrapped value.
  - Optional-like fields are supported in validated builders:
    - `Option[T]` fields accept raw `T`; `builderAllow` also accepts wrapped `Option[T]`.
    - Java optional fields (`java.util.Optional[T]`, `OptionalInt`, `OptionalLong`, `OptionalDouble`) accept raw values; `builderAllow` also accepts wrapped optional values.
    - Trailing optional-like fields can be completed with `.!`.
    - `.!` is compile-time rejected if required fields still remain.
  - Validated builders now also support an opt-in path-aware mode via `ValidationPathConfig`. In that mode the error channel becomes `ValidationPathError[E]` for builders that actually validate fields, and nested validated smart constructors prepend outer path segments while preserving a single path wrapper.
  - Path-aware validation now includes collection-aware segments for validated collection fields (`Seq`/`List`/`Set`/`Vector`/`Map`): `Field` + `Index`, and `Named` segments when available.
  - Collection setters accept both pre-built and pre-validated inputs (for example `Seq[A]` and `Seq[ZValidation[Nothing, E, A]]`).
  - In mixed-validator builders, `E` can be a union type (for example `String | Throwable`), resulting in `ValidationPathError[String | Throwable]`.
  - Path configuration is a no-op for all-plain case classes whose error type is already `Nothing`.

- A lightweight JMH run is executed in CI on merges to `main` (short benchmark smoke check). See `jmh/README.md` for local-run instructions.

  - Note: When running JMH locally, you might want to use the `-no-colors` option for better readability in your terminal.
