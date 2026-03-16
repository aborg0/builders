This `jmh` subproject contains JMH benchmarks comparing generated validated builders, manual validated builders, and the simple builders.

Run benchmarks via sbt from the repository root. Examples:

  sbt jmh/jmh:run -i 10 -wi 5 -f 1 bench.Bench_GeneratedValidated_vs_ManualValidated
  sbt jmh/jmh:run -i 10 -wi 5 -f 1 bench.Bench_SimpleBuilder_vs_Validated

Notes:
- The `jmh` project depends on `with_zio_prelude` and `simple_builders` so it can reference their builders.
- JVM options are configured in `build.sbt` for reasonable benchmarking memory settings.

