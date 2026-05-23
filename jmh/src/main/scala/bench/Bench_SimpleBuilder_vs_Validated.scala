package bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import playground._
import api.ValidatedBuilderGenerator

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class Bench_SimpleBuilder_vs_Validated {

  // Simple builder for a small model in this jmh project
  val simple = SimpleSmallBuilder()

  // Validated builder for SmallValidated
  val validated = ValidatedBuilderGenerator.builderAllow[SmallValidated]

  @Benchmark
  def simple_complete(blackhole: Blackhole): Unit = {
    val res = SimpleSmallBuilder().i(1).op("A").v("B")
    blackhole.consume(res)
  }

  @Benchmark
  def validated_complete(blackhole: Blackhole): Unit = {
    val zv = validated.i(1).op("Op").v("Op")
    blackhole.consume(zv)
  }

}
