This `jmh` subproject contains JMH benchmarks comparing generated validated builders, manual validated builders, and the simple builders.

Run benchmarks via sbt from the repository root. Examples (use `-no-colors` in CI/local runs for consistent output):

  sbt -no-colors "jmh/jmh:run -i 10 -wi 5 -f 1 bench.Bench_GeneratedValidated_vs_ManualValidated"
  sbt -no-colors "jmh/jmh:run -i 10 -wi 5 -f 1 bench.Bench_SimpleBuilder_vs_Validated"

Notes:
- The `jmh` project depends on `with_zio_prelude` and `simple_builders` so it can reference their builders.
- JVM options are configured in `build.sbt` for reasonable benchmarking memory settings.
- CI performs a short, lightweight JMH smoke run on merges to `main` (not the full long-running benchmarks).
