package sgp4
package model

import org.scalactic.Or
import org.scalatest.funsuite.AnyFunSuite
import sgp4.coord.{CartesianElems, ErrorMessage}
import sgp4.earth_gravity.SGP72Constants
import sgp4.io.TLE

import scala.io.Source

/** Cross-checks the propagators against the python3-sgp4 library on *current* TLEs downloaded from celestrak.org,
  * complementing PythonSgp4CrossCheck which uses the 2006 era reference TLEs.
  *
  * The resources are a snapshot: scripts/gen-celestrak-reference.py downloads a handful of near-Earth satellites from
  * several celestrak groups (ISS and other stations, OneWeb, Iridium NEXT, Planet, Orbcomm, Spire; varied inclinations
  * and drag), propagates them with the system python3-sgp4 over a 6 day grid, and writes celestrakTLEs.txt +
  * celestrakReference.csv. Re-run it any time to refresh the snapshot with current data.
  *
  * Tolerances: SGP4Vallado ports Vallado's algorithm, the same one python3-sgp4 implements, so it is asserted tightly
  * (measured agreement on this data: ~1e-9 km). The other three variants compute the periodic corrections on different
  * routes and legitimately differ from the reference algorithm; they are asserted within their expected agreement
  * envelope (measured on this data: up to ~0.5 km over the 6 day grid).
  */
class CelestrakTLEsCrossCheck extends AnyFunSuite {

  import spire.std.any.DoubleAlgebra
  private val wgs = SGP72Constants.tleDoubleConstants

  private val tles: Map[Int, TLE] =
    Source
      .fromInputStream(getClass.getResourceAsStream("/celestrakTLEs.txt"))
      .getLines()
      .toList
      .grouped(3)
      .flatMap(g => TLE.parseLines(List(g(1), g(2))).map(t => (t.satelliteNumber, t)))
      .toMap

  private val reference: Map[(Int, Double), CartesianElems[Double]] =
    Source
      .fromInputStream(getClass.getResourceAsStream("/celestrakReference.csv"))
      .getLines()
      .drop(1)
      .map { row =>
        val c = row.split(",").map(_.toDouble)
        (c(0).toInt, c(1)) -> CartesianElems(c(2), c(3), c(4), c(5), c(6), c(7))
      }
      .toMap

  private val referenceBySat: Map[Int, List[(Double, CartesianElems[Double])]] =
    reference.toList.groupBy(_._1._1).view.mapValues(_.map { case ((_, t), pv) => (t, pv) }).toMap

  private val tightPosTol = 1.0e-5 // km, SGP4Vallado vs python: measured ~1e-9
  private val tightVelTol = 1.0e-8 // km/s, SGP4Vallado vs python: measured ~1e-11
  private val envelopePosTol = 5.0 // km, periodic correction variants: measured up to ~0.5
  private val envelopeVelTol = 5.0e-3 // km/s, periodic correction variants: measured ~4e-4

  private def check(name: String, build: TLE => SGP4[Double] Or ErrorMessage, posTol: Double, velTol: Double): Unit =
    for ((satnum, grid) <- referenceBySat.toList.sortBy(_._1)) {
      test(f"$name: TLE $satnum%d vs python3-sgp4 (${grid.size} points)") {
        val propagator = build(tles(satnum)) match {
          case org.scalactic.Good(p) => p
          case org.scalactic.Bad(e)  => fail(s"fromTLE failed for TLE $satnum: $e")
        }
        var maxPosDev = 0.0
        var maxVelDev = 0.0
        for ((t, expected) <- grid) {
          val pv = propagator.propagate(t) match {
            case org.scalactic.Good((pv, _, _)) => pv
            case org.scalactic.Bad(e)           => fail(s"propagation failed for TLE $satnum at t=$t: $e")
          }
          val dp =
            math.max(math.abs(pv.x - expected.x), math.max(math.abs(pv.y - expected.y), math.abs(pv.z - expected.z)))
          val dv = math.max(
            math.abs(pv.vx - expected.vx),
            math.max(math.abs(pv.vy - expected.vy), math.abs(pv.vz - expected.vz))
          )
          maxPosDev = math.max(maxPosDev, dp)
          maxVelDev = math.max(maxVelDev, dv)
        }
        withClue(f"max position deviation $maxPosDev%.9f km (tolerance $posTol) at 6-day grid: ") {
          assert(maxPosDev < posTol)
        }
        withClue(f"max velocity deviation $maxVelDev%.12f km/s (tolerance $velTol): ") {
          assert(maxVelDev < velTol)
        }
      }
    }

  check("SGP4Vallado", tle => SGP4Vallado.fromTLE(tle, wgs), tightPosTol, tightVelTol)
  check("SGP4ValladoLong", tle => SGP4ValladoLong.fromTLE(tle, wgs), envelopePosTol, envelopeVelTol)
  check("SGP4PN", tle => SGP4PN.fromTLE(tle, wgs), envelopePosTol, envelopeVelTol)
  check("SGP4Lara", tle => SGP4Lara.fromTLE(tle, wgs), envelopePosTol, envelopeVelTol)
}
