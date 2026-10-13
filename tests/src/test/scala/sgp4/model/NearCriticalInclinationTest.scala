package sgp4
package model

import org.scalatest.funsuite.AnyFunSuite
import sgp4.coord.CartesianElems
import sgp4.earth_gravity.SGP72Constants
import sgp4.io.TLE

/** The critical inclination, where the secular rate of the argument of perigee vanishes (1 - 5cos²I = 0 at I =
  * 63.43494882... deg), is not a singularity of the propagators: the long period corrections are computed in Lyddane's
  * and Lara's non singular variables, and the vanishing factor appears only in the numerator of ωdot. These tests
  * propagate TLEs at the inclinations closest to the critical one that the TLE format can express, with 4 decimal
  * places, and check that all the algorithms remain well conditioned and keep their usual mutual agreement.
  */
class NearCriticalInclinationTest extends AnyFunSuite {

  import spire.std.any.DoubleAlgebra

  private val line1 = "1 00005U 58002B   00179.78495062  .00000023  00000-0  28098-4 0  4753"
  private def line2(inclination: String) =
    s"2 00005  $inclination 348.7242 1859667 331.7664  19.3264 10.82419157413667     0.00      4320.0        360.00"

  private val wgs = SGP72Constants.tleDoubleConstants

  // the usual mutual agreement envelope of the algorithms over several days, as measured in the cross-check tests
  private val envelopePosTol = 5.0 // km
  private val envelopeVelTol = 5.0e-3 // km/s
  private val times = List(0.0, 720.0, 1440.0, 4320.0)

  private def propagators(tle: TLE): List[SGP4[Double]] = {
    val vallado = SGP4Vallado.fromTLE(tle, wgs).get
    List(vallado, SGP4ValladoLong(vallado.sec), SGP4PN(vallado.sec), SGP4Lara(vallado.sec))
  }

  private def positions(tle: TLE): List[List[CartesianElems[Double]]] =
    propagators(tle).map { sgp4 =>
      times.map { t =>
        sgp4.propagate(t) match {
          case org.scalactic.Good(propCtx) => propCtx.pv
          case org.scalactic.Bad(e)        => fail(s"propagation failed at t=$t: $e")
        }
      }
    }

  test("SGP4 propagates a TLE at the inclination closest to the critical one without error") {
    val tle = TLE.parseLines(List(line1, line2("63.4349"))).head
    assert(positions(tle).forall(_.size == times.size))
  }

  test("all the algorithms keep their usual mutual agreement at the critical inclination") {
    val tle = TLE.parseLines(List(line1, line2("63.4349"))).head
    val positionsByAlgo = positions(tle)
    val vallado = positionsByAlgo.head
    positionsByAlgo.tail.foreach { algo =>
      algo.zip(vallado).zip(times).foreach { case ((a, v), t) =>
        val dp =
          math.max(math.abs(a.x - v.x), math.max(math.abs(a.y - v.y), math.abs(a.z - v.z)))
        val dv =
          math.max(math.abs(a.vx - v.vx), math.max(math.abs(a.vy - v.vy), math.abs(a.vz - v.vz)))
        assert(dp < envelopePosTol, s"position difference $dp km at t=$t min")
        assert(dv < envelopeVelTol, s"velocity difference $dv km/s at t=$t min")
      }
    }
  }

  test("adjacent TLE inclination quanta around the critical inclination propagate smoothly") {
    val inclinations = List("63.4348", "63.4349", "63.4350")
    val positionsByInclination =
      inclinations.map(i => TLE.parseLines(List(line1, line2(i))).head).map(positions(_).head)
    positionsByInclination.zipWithIndex.foreach { case (positions, idx) =>
      val next = positionsByInclination.lift(idx + 1)
      next.foreach { nextPositions =>
        positions.zip(nextPositions).foreach { case (a, b) =>
          val dp =
            math.max(math.abs(a.x - b.x), math.max(math.abs(a.y - b.y), math.abs(a.z - b.z)))
          // adjacent inclination quanta of the TLE format differ by 1e-4 deg: the physical difference is smooth,
          // no instability of the propagation near the critical inclination
          assert(dp < 1.0, s"position difference $dp km between adjacent inclination quanta")
        }
      }
    }
  }
}
