## Plan: Configurable Validation Path Errors

Implement an opt-in path-aware error mode for validated builders that preserves current behavior by default. In default mode, errors stay as E. In configured mode, errors become (Seq[ValidationPathPart], E), and include an optional custom prefix plus field-name segments.

**Steps**
1. Phase 1 - Public API and path model (blocks all later work):
1. Add a library-owned ValidationPathPart ADT in the api package with cases for Custom(prefix), Name(name), and Index(zeroIndex) (index-ready, field-first implementation).
2. Add configuration overloads for builder/derived entry points so current no-arg calls remain unchanged and source-compatible.
3. Keep default overload inference as ZValidation[Nothing, E, T].

2. Phase 2 - Macro plumbing for config and error typing (depends on 1):
1. Thread path configuration through macro entry points into analysis/build stages.
2. Keep computeUnifiedErrorType unchanged in default mode.
3. In path mode, transform unified error EU into (Seq[ValidationPathPart], EU).
4. Reuse existing fieldName metadata from validator discovery to build Name(...) segments.

3. Phase 3 - Field-level error enrichment (depends on 2):
1. In generated per-field validator closures, map failures E to (pathSegments, E) only in path mode.
2. Ensure both FromEither and FromValidation branches produce the same enriched shape.
3. Build segments as optional Custom(prefix) followed by Name(fieldName).

4. Phase 4 - Keep combine/chain strategy stable (depends on 3 shape finalization):
1. Retain existing runtime accumulation strategy (map / zipWithPar / foldLeft).
2. Limit changes to error-channel typing to minimize regression risk.

5. Phase 5 - Tests and docs (depends on 3 and 4):
1. Add behavioral test for configured path mode using nested Dummy-like case to verify right-side failure path.
2. Add compile-time type checks for both default and path-enabled modes.
3. Document new overloads and example output in docs.

**Relevant files**
- with_zio_prelude/src/main/scala/api/ValidatedBuilderGenerator.scala - entry-point overloads, config threading, error-type transformation, field-level mapError wrapping.
- with_zio_prelude/src/main/scala/api/ValidatorInfo.scala - path-part modeling placement and metadata compatibility.
- with_zio_prelude/src/main/scala/api/SmartConstructorDiscovery.scala - verify metadata sufficiency; adjust only if needed for path-mode typing.
- with_zio_prelude/src/test/scala/api/ValidatedBuilderTest.scala - behavior tests for configured prefix + Name(right)-style path.
- with_zio_prelude/src/test/scala/api/ValidationErrorTypeTests.scala - compile-time type assertions for compatibility and path mode.
- README.md and docs/readme.md - API and usage examples for path-enabled builder configuration.

**Verification**
1. Run targeted tests in with_zio_prelude for new path-mode behavior plus existing regression checks.
2. Confirm compile-time assertions:
1. Default mode infers ZValidation[Nothing, E, T].
2. Path mode infers ZValidation[Nothing, (Seq[ValidationPathPart], E), T].
3. Run -no-colors +test across supported Scala versions.
4. If docs updated, run -no-colors docs/mdoc.

**Decisions captured**
- Apply configuration to builder and derived families.
- Path ADT is library-owned in api.
- Backward compatibility is preserved by default.
- Design supports index parts, but this implementation populates field names first.

I saved this plan to /memories/session/plan.md and can refine it if you want different API ergonomics (for example, a dedicated config case class vs a simple optional prefix overload).
