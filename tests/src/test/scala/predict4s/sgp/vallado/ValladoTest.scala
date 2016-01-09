package predict4s.sgp.vallado

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import predict4s.sgp.{NearTLEs}
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.coord.SGPElems
import predict4s.coord.SGPElemsConversions

trait ValladoNearTLEsTest extends NearTLEs with ValladoNearTLEsCheck[Double] with ValladoNearTLEsPVCheck[Double] { self : FunSuite => 

  implicit val wgs = SGP72Constants.tleDoubleConstants
  
  // Propagators for all TLEs
  def propags : List[SGP4Vallado[Double]]
  
  def sgpImpl : String

  // List with List of Propagation results for each TLE and all propagation times 
  def results : List[IndexedSeq[Sgp4ValladoResult]] = ((propags zip tles) zip tlesTimes) map { p  =>  // pair of (pair propagator with tle) with the propagation times   
    val sgp4 = p._1._1 ; val tle = p._1._2; val times = p._2
    for (t <- times) yield Sgp4ValladoResult(sgp4, sgp4.propagate(t), tle, t)
  }
  
  test(s"${sgpImpl}: compare Position/Velocity Propagation Results with Vallado's cpp implementation for near TLEs") {
    // call the checks for the corresponding result 
    (pvNearChecks zip results) foreach  { l =>  // pair with the lists containing the checks and the results
      (l._1 zip l._2) foreach { p => p._1(p._2) } 
    }
  }
}


class HardcodedValladoCheck extends FunSuite with NearTLEs with ValladoNearTLEsCheck[Double] with ValladoNearTLEsPVCheck[Double] {
 
  val wgs = SGP72Constants.tleDoubleConstants
  val toMinus9 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(1E-9)

  def propags : List[SGP4Vallado[Double]] = tles map {tle => 
    import spire.std.any.DoubleAlgebra
    SGP4Vallado[Double](tle, wgs)
  }
  def sgpImpl : String = "Vallado SGP4"
  
  val sgps     = propags
  
  def sgp00005 = sgps(0)
  def sgp06251 = sgps(1)
  def sgp28057 = sgps(2)
  
  val results00005 = for (t <- times00005)  yield Sgp4ValladoResult(sgps(0), sgp00005.propagate(t), tle00005, t)
  val results06251 = for (t <- times06251)  yield Sgp4ValladoResult(sgps(1), sgp06251.propagate(t), tle06251, t)
  val results28057 = for (t <- times28057)  yield Sgp4ValladoResult(sgps(2), sgp28057.propagate(t), tle28057, t)

//  test(s"${sgpImpl}: compare Intermediate result t=0") {
//    checkIntl5(results00005(0))(toMinus9)
//    checkSgp4Init5(results00005(0))(toMinus9)
//    checkIntl6251(results06251(0))(toMinus9)
//    checkSgp4Init6251(results06251(0))(toMinus9)
//  }
  
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


