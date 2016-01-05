package predict4s.sgp.lara

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import predict4s.sgp._
import predict4s.coord._
import org.scalatest.Ignore

//@Ignore
class HardcodedLaraCheck extends FunSuite with NearTLEs with ValladoNearTLEsCheck[Double] with ValladoNearTLEsPVCheck[Double] {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants
  val toMinus9 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(1)

  def propags : List[SGP4Lara[Double]] = tles map {tle => 
    import spire.std.any.DoubleAlgebra
    SGP4Lara[Double](tle, wgs)
  }
  def sgpImpl : String = "Lara SGP4"
  
  val sgps     = propags
  
  def sgp00005 = sgps(0)
  def sgp06251 = sgps(1)
  def sgp28057 = sgps(2)
  
  val results00005 = times00005.map(sgp00005.propagate2Cartesian(_))
  
//  val results00005 = for (t <- times00005)  yield Sgp4LaraResult(sgps(0), sgp00005.propagate(t), tle00005, t)
//  val results06251 = for (t <- times06251)  yield Sgp4LaraResult(sgps(1), sgp06251.propagate(t), tle06251, t)
//  val results28057 = for (t <- times28057)  yield Sgp4LaraResult(sgps(2), sgp28057.propagate(t), tle28057, t)
//
  implicit def cartesianToPosVel(pv : CartesianElems[Double]) = new PosVel[Double] {
    def x = pv.x
    def y = pv.y
    def z = pv.z
    def xdot = pv.X
    def ydot = pv.Y    
    def zdot = pv.Z   
    def error = 0
  }
  
  test(s"${sgpImpl}: compare Position/Velocity Propagation Results with Vallado's cpp implementation for near TLEs") {
    // call the checks for the corresponding result
    pvCheck00005 zip results00005 foreach { p => p._1(p._2)(toMinus9) }
//    pvCheck06251 zip results06251 foreach { p => p._1(p._2)(toMinus9) }
//    pvCheck28057 zip results28057 foreach { p => p._1(p._2)(toMinus9) }   
  }
} 


