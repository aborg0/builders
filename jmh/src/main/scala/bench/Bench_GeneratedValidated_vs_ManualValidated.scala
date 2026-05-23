package bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import zio.prelude.ZValidation
import api.ValidatedBuilderGenerator
import playground._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class Bench_GeneratedValidated_vs_ManualValidated {

  // Use local small validated model to avoid SequenceNumber subtype complications
  val gen = ValidatedBuilderGenerator.builderAllow[SmallValidated]
  val manual = ManualBuilders.ManualSmallValidated.builder()

  // Success-case inputs
  val goodOp = "Op"
  val goodV = "Op"

  @Benchmark
  def generated_success(blackhole: Blackhole): Unit = {
    val zv = gen.i(42).op(goodOp).v(goodV)
    blackhole.consume(zv)
  }

  @Benchmark
  def manual_success(blackhole: Blackhole): Unit = {
    val v = manual.i(42).op(goodOp).v(goodV)
    blackhole.consume(v)
  }

  // Failure-case benchmarks
  val badOp = "NotOp"

  @Benchmark
  def generated_fail(blackhole: Blackhole): Unit = {
    val zv = gen.i(42).op(badOp).v(goodV)
    blackhole.consume(zv)
  }

  @Benchmark
  def manual_fail(blackhole: Blackhole): Unit = {
    val v = manual.i(42).op(badOp).v(goodV)
    blackhole.consume(v)
  }

}
