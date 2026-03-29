# Builders

This repository contains the builders projects and documentation. The CI builds across multiple JDKs and Scala versions and publishes artifacts to GitHub Packages.

Docs
-----

Check [builder_docs/target/mdoc/readme.md](builders_docs/target/mdoc/readme.md), in case it does not exist, generate it with:

```sbt
docs/mdoc
```

You can check the non-evaluated documentation at [docs/readme.md](docs/readme.md) as well.

Notes and recent changes
------------------------

- The macros now support `zio.prelude.Newtype` / `zio.prelude.Subtype` (and the `NewtypeCustom`/`SubtypeCustom` variants) in addition to `opaque type` wrappers. That means builders will discover and invoke `make`/`apply` on zio.prelude-style wrappers where appropriate.

- Semantics summary (short):
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
