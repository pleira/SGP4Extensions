package predict4s
package sgp
package ref

import org.scalatest.funsuite.AnyFunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality

import predict4s.coord.SGP72Constants
import predict4s.sgp._

/* Near earth examples from Vallado's */
//trait NearTLEs extends TLE00005 with TLE06251 with TLE28057 {
//  def tles = List(tle00005, tle06251, tle28057)
//  def lines = List(lines00005,lines06251,lines28057)
//}

trait ValladoNearTLEsTest
    extends TLE00005
    with TLE06251
    with TLE28057
    with ValladoNearTLEsCheck[Double]
    with ValladoNearTLEsPVCheck[Double] { self: AnyFunSuite =>

  implicit val wgs: SGP72Constants[Double] = SGP72Constants.tleDoubleConstants

  // Propagators for all TLEs
  def propags: List[SGP4Vallado[Double]]

  def sgpImpl: String

  val tles = List(tle00005, tle06251, tle28057)

  def results = propags.zip(tles).zip(tlesTimes).toList map {
    case ((sgp4, tle), times) => // propagator, tle and its propagation times
      val res = for (t <- times) yield (sgp4.propagate(t), t)
      (sgp4, res, tle)
  }

}

class HardcodedValladoCheck
    extends AnyFunSuite
    with TLE00005
    with TLE06251
    with TLE28057
    with ValladoNearTLEsCheck[Double]
    with ValladoNearTLEsPVCheck[Double] {

  val wgs = SGP72Constants.tleDoubleConstants
  val toMinus9: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(1e-9)
  val tles = List(tle00005, tle06251, tle28057)
  def propags: List[SGP4Vallado[Double]] = tles map { tle =>
    import spire.std.any.DoubleAlgebra
    SGP4Vallado.fromTLE[Double](tle, wgs).get
  }
  def sgpImpl: String = "Vallado SGP4"

  val sgps = propags

  def sgp00005 = sgps(0)
  def sgp06251 = sgps(1)
  def sgp28057 = sgps(2)

  val results00005 = for (t <- times00005) yield Sgp4ValladoResult(sgps(0), sgp00005.propagate(t).get, tle00005, t)
  val results06251 = for (t <- times06251) yield Sgp4ValladoResult(sgps(1), sgp06251.propagate(t).get, tle06251, t)
  val results28057 = for (t <- times28057) yield Sgp4ValladoResult(sgps(2), sgp28057.propagate(t).get, tle28057, t)

  test(s"${sgpImpl}: compare Intermediate Propagation Results with Vallado's cpp implementation for near TLEs") {
    // call the checks for the corresponding result
    check00005 zip results00005 foreach { p => p._1(p._2)(toMinus9) }
    check06251 zip results06251 foreach { p => p._1(p._2)(toMinus9) }
    check28057 zip results28057 foreach { p => p._1(p._2)(toMinus9) }
  }

  test(s"${sgpImpl}: compare Position/Velocity Propagation Results with Vallado's cpp implementation for near TLEs") {
    // call the checks for the corresponding result
    pvCheck00005 zip results00005 foreach { p => p._1(p._2)(toMinus9) }
    pvCheck06251 zip results06251 foreach { p => p._1(p._2)(toMinus9) }
    pvCheck28057 zip results28057 foreach { p => p._1(p._2)(toMinus9) }
  }
}
