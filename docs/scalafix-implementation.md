# Scalafix Builder Generation - Decisions and Implementation

## Decisions

1. Separate configuration artifact
- Annotations and enums live in the `configuration` module (`builders-configuration`).
- Consumers are expected to depend on:
  - configuration artifact
  - Scalafix rules artifact
- Consumers are not expected to depend on runtime builder generator modules.

2. Annotation model
- Single annotation: `GenerateBuilder`
- Enum-based parameters (no free-form string config)
- Additional generation controls:
  - `builderMethodName` (default `"builder"`)
  - `generateExtraVariants` (default `true`)
  - `simpleOptionalValues` (default `ExplicitOptionalValues`)
- Default profile aligns with previous planning decisions:
  - explicit generation strategy
  - no applyDynamic API style
  - primitive+wrapped inputs when conversion is derivable/generated
  - check-only stale detection workflow in CI

3. Initial scope
- Start with schema + decoding + exhaustive option tests.
- Keep rule integration incremental after schema stability.

4. Scalafix runtime compatibility
- The Scalafix rule module is compiled with Scala 2.13.
- The configuration module remains Scala 3 and is consumed by user code.
- The rule module intentionally decodes option names by symbol/value strings to avoid forcing a Scala 3 compile-time dependency in the rule artifact.

## Implemented Modules

1. `configuration`
- Houses user-facing annotation/enums/options defaults:
  - `BuilderStyle`
  - `PrimitivePolicy`
  - `PathMode`
  - `EffectMode`
  - `ConversionMode`
  - `StaleCheckMode`
  - `MergeMode`
  - `SimpleOptionalValues`
  - `GenerateBuilder`
  - `GenerateBuilderOptions.default`

2. `remove_fix`
- Scala 2.13 Scalafix rules module.
- Contains:
  - `GenerateBuildersRule` (semantic rule entrypoint)
  - `GenerateBuilderAnnotationMatcher` (symbol-aware annotation matching with fallback)
  - `GenerateBuilderAnnotationDecoder`
  - `DecodedGenerateBuilder`
  - `GenerateBuilderCompanionRenderer`
  - `GenerateBuilderTextRewriter`
- Decoder behavior:
  - applies defaults for missing arguments
  - decodes all supported enum values
  - supports qualified names (`Type.Value`) and unqualified names (`Value`)
  - reports diagnostics for unknown keys and unsupported values
- Initial rewrite behavior:
  - discovers `@GenerateBuilder` on case classes
  - decodes style/policy arguments
  - appends generated companion skeletons when no companion exists
  - supports simple and validating style golden scenarios
  - emits generated fields for all supported option dimensions (style, primitivePolicy, pathMode, effectMode, conversionMode, staleCheckMode, mergeMode, simpleOptionalValues)
  - emits style-specific builder API expression:
    - simple -> configured public entry method name (default `builder`)
    - validating -> `api.ValidatedBuilderGenerator.builder[T]`
    - effect -> currently mapped to validating builder API as the extension seam
  - emits style-specific generated builder members:
    - simple -> inline typed step-chain aliases/methods (`Builder`, `AfterStepN`, `stepN`) with two modes:
      - explicit mode (`ExplicitOptionalValues`): strict linear field progression
      - optional-omission modes (`OptionalValuesFromDefaults`, `OptionalValuesWithEmptyDefaults`): generated state methods with selective skipping and `build()` completion for optional-like fields
    - validating -> `type Builder`, public `builder`/`builderAllow`/`builderNoAllow`, `derived*`, private builder refs, and typed tuple-chain helpers (`AfterStepN`, `fieldStepN*`, `buildValidationFromValues`)
      - terminal validated type is generated as `zio.prelude.ZValidation[Nothing, ?, T]` (wildcard error type, concrete target type)
    - effect -> `type Builder`, public `builder`/`builderEffect`, `derived`, private builder refs, and typed tuple-chain helpers (`AfterStepN`, `fieldStepN*`, `buildEffectFromValues`)
      - terminal effect-chain type is generated as `zio.prelude.ZValidation[Nothing, ?, T]` while effect mode reuses the validated backend seam
  - generated members are non-public helper members in the companion (`private ...`)
  - `transparent inline` methods are intentionally avoided in generated output

## Test Coverage

Current tests intentionally cover all supported options at schema/decode level and include golden rewrite checks:

1. Schema tests (`configuration`)
- Validate exact enum value sets.
- Validate default options stability.
- Validate annotation defaults mirror options defaults.

2. Decoder tests (`remove_fix`)
- Validate default decode with empty argument map.
- Validate decoding for every value in every enum.
- Validate qualified enum decoding.
- Validate diagnostics for unknown keys and invalid values.

3. Golden rewrite tests (`remove_fix`)
- Validate simple style fixture rewrite output.
- Validate validating style fixture rewrite output.
- Validate effect style fixture rewrite output.
- Normalize line endings for stable cross-platform assertions.

4. Option-matrix rewrite tests (`remove_fix`)
- Validate propagation of non-default option combinations into generated output.
- Validate that existing companions are not regenerated.
- Validate custom builder method naming and extra-variant toggling.
- Validate validating-style smart-constructor seams (`builder/allow/noAllow` plus `derived*` links) remain generated.
- Validate simple optional omission strategies, including defaults-based completion paths.

5. Text rewriter robustness (`remove_fix`)
- Top-level parser handles nested annotation arguments and field defaults reliably.
- Field parsing supports nested default expressions such as `Some(99)`.

## Next Implementation Steps

1. Extend generated companion content from typed private pipelines to full field-level explicit builder API generation.
2. Add stale-check command path and idempotent regeneration flow.
3. Extend golden coverage to complete option combinations and merge/stale modes.
4. Add semantic regression tests using Scalafix input/output rule test harness.

For hands-on usage, see [scalafix-usage.md](scalafix-usage.md).
