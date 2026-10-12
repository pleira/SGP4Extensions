package predict4s
package bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import predict4s.coord.SGPElemsConversions
import predict4s.coord.SGP72Constants
import predict4s.coord.TLE
import predict4s.sgp.BrouwerLaneSecularCorrections
import predict4s.sgp.TLE00005
import predict4s.sgp.TLE06251
import predict4s.sgp.ref.SGP4Lara
import predict4s.sgp.ref.SGP4PN
import predict4s.sgp.ref.SGP4Vallado
import predict4s.sgp.ref.SGP4ValladoLong

/**
 * Measures one full propagation (secular + long/short period corrections to
 * Cartesian TEME) of each SGP4 algorithm variant, with the propagator built
 * once in the setup. Also measures the cost of rebuilding the propagator on
 * every call, which is the common misuse to avoid.
 *
 * Run with allocation profiling to also get per-op allocation:
 *   benchmarks/jmh:run -prof gc
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(1)
class SGP4PropagationBench {

  import spire.std.any.DoubleAlgebra
  implicit val wgs = SGP72Constants.tleDoubleConstants

  @Param(Array("00005", "06251"))
  var tleId: String = null

  val t = 720.0 // minutes from epoch

  var tle: TLE = null
  var va: SGP4Vallado[Double] = null
  var pn: SGP4PN[Double] = null
  var vl: SGP4ValladoLong[Double] = null
  var la: SGP4Lara[Double] = null

  @Setup
  def setup(): Unit = {
    tle = tleId match {
      case "06251" => TLE06251.tle
      case _       => TLE00005.tle
    }
    va = SGP4Vallado.build(tle, wgs).get
    pn = SGP4PN.build(tle, wgs).get
    vl = SGP4ValladoLong.build(tle, wgs).get
    la = SGP4Lara.build(tle, wgs).get
  }

  @Benchmark
  def valladoPropagate(bh: Blackhole): Unit =
    bh.consume(va.propagate(t))

  @Benchmark
  def polarNodalsPropagate(bh: Blackhole): Unit =
    bh.consume(pn.propagate(t))

  @Benchmark
  def valladoLongPropagate(bh: Blackhole): Unit =
    bh.consume(vl.propagate(t))

  @Benchmark
  def laraPropagate(bh: Blackhole): Unit =
    bh.consume(la.propagate(t))

  @Benchmark
  def buildAndValladoPropagate(bh: Blackhole): Unit =
    bh.consume(SGP4Vallado.build(tle, wgs).get.propagate(t))
}
