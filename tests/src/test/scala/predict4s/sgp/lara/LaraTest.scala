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
  val toMinus : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(2.2)

  def propags : List[SGP4Lara[Double]] = tles map {tle => 
    import spire.std.any.DoubleAlgebra
    SGP4Lara[Double](tle, wgs)
  }
  def sgpImpl : String = "Lara SGP4"
  
  val sgps     = propags
  
  def sgp00005 = sgps(0)
  def sgp06251 = sgps(1)
  def sgp28057 = sgps(2)
  
//   No SPN intermediaries, results converted from Lara's non singular to cartesian
// FIXME  
//  val results00005c = times00005.map(sgp00005.propagate2Cartesian(_))
//  val results06251c = times06251.map(sgp06251.propagate2Cartesian(_))
//  val results28057c = times28057.map(sgp28057.propagate2Cartesian(_))
  
  val results00005 = times00005.map(sgp00005.propagate(_)._1) // just return the first CartesianElem inside the map
  val results06251 = times06251.map(sgp06251.propagate(_)._1)
  val results28057 = times28057.map(sgp28057.propagate(_)._1)
   
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
    pvCheck00005 zip results00005 foreach { p => p._1(p._2)(toMinus) }
    pvCheck06251 zip results06251 foreach { p => p._1(p._2)(toMinus) }
    pvCheck28057 zip results28057 foreach { p => p._1(p._2)(toMinus) }   
  }
  
//  test(s"${sgpImpl}: compare native Lara to Cartesian Position/Velocity Propagation Results with Vallado's cpp implementation for near TLEs") {
//    // call the checks for the corresponding result
//    pvCheck00005 zip results00005c foreach { p => p._1(p._2)(toMinus) }
//    pvCheck06251 zip results06251c foreach { p => p._1(p._2)(toMinus) }
//    pvCheck28057 zip results28057c foreach { p => p._1(p._2)(toMinus) }   
//  }  
} 


