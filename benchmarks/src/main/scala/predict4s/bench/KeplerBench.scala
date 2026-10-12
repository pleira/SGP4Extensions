package predict4s
package bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import predict4s.sgp.ref.SimpleKeplerEq

/**
 * Compares the generic (spire type classes, F = Double) Kepler equation
 * solver used by SGP4PN against an equivalent hand-written primitive Double
 * Newton loop. The generic code is @specialized for Double and spire's
 * Double type class instances use primitive JVM signatures, so after JIT
 * warmup both run at the same speed; this benchmark documents that.
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(1)
class KeplerBench {

  object kepler extends SimpleKeplerEq

  // M = 90 deg: an above-average iteration count for the solver
  val e = 0.02
  val M = math.toRadians(90.0)

  @Benchmark
  def genericSpireKepler(bh: Blackhole): Unit = {
    import spire.std.any.DoubleAlgebra
    bh.consume(kepler.solveKeplerEq[Double](e, M))
  }

  @Benchmark
  def primitiveKepler(bh: Blackhole): Unit = {
    var E = M
    var remaining = 10
    var res = (E, math.cos(E), math.sin(E), 0.0, 0.0)
    var done = false
    while (!done) {
      val sinE = math.sin(E)
      val cosE = math.cos(E)
      val ecosE = e * cosE
      val esinE = e * sinE
      val fdot = 1 - ecosE
      val f = M - (E - esinE)
      val incr = f / fdot
      if (math.abs(incr) < 1e-12) {
        res = (E, cosE, sinE, ecosE, esinE)
        done = true
      } else if (remaining < 0) {
        done = true
      } else {
        E += incr
        remaining -= 1
      }
    }
    bh.consume(res)
  }
}
