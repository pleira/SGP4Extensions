package predict4s.sgp.lara

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import predict4s.sgp.{NearTLEs}
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.coord.SGPElems
import predict4s.coord.SGPElemsConversions
import org.scalatest.Ignore


trait LaraNearTLEsTest extends NearTLEs with ValladoNearTLEsCheck[Double] with ValladoNearTLEsPVCheck[Double] { self : FunSuite => 

  implicit val wgs = SGP72Constants.tleDoubleConstants
  
  // Propagators for all TLEs
  def propags : List[SGP4Lara[Double]]
  
  def sgpImpl : String

  // List with List of Propagation results for each TLE and all propagation times 
//  def results : List[IndexedSeq[Sgp4LaraResult]] = ((propags zip tles) zip tlesTimes) map { p  =>  // pair of (pair propagator with tle) with the propagation times   
//    val sgp4 = p._1._1 ; val tle = p._1._2; val times = p._2
//    for (t <- times) yield Sgp4LaraResult(sgp4, sgp4.propagate(t), tle, t)
//  }
//  
//  test(s"${sgpImpl}: compare Position/Velocity Propagation Results with Vallados's cpp implementation for near TLEs") {
//    // call the checks for the corresponding result 
//    (pvNearChecks zip results) foreach  { l =>  // pair with the lists containing the checks and the results
//      (l._1 zip l._2) foreach { p => p._1(p._2) } 
//    }
//  }
}

//class CheckNearTLEsLara extends FunSuite with LaraResultCheck {
//  override def sgpImpl : String = "Lara SGP4"  
//  override def propags : List[SGP4Lara[Double]]  = tles map {tle => 
//    import spire.std.any.DoubleAlgebra
//    val elem0 = SGPElems(tle); 
//    SGP4Lara[Double](elem0)
//  }
//}

@Ignore
class HardcodedLaraCheck extends FunSuite with NearTLEs with ValladoNearTLEsCheck[Double] with ValladoNearTLEsPVCheck[Double] {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants
  val toMinus9 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(1)
//
//  def propags : List[SGP4Lara[Double]] = tles map {tle => 
//    import spire.std.any.DoubleAlgebra
//    val elem0AndCtx = SGPElemsFactory.sgpElemsAndContext(tle)
//    SGP4Lara[Double](BrouwerLaneSecularCorrections(elem0AndCtx))
//  }
  def sgpImpl : String = "Lara SGP4"
//  
//  val sgps     = propags
//  
//  def sgp00005 = sgps(0)
//  def sgp06251 = sgps(1)
//  def sgp28057 = sgps(2)
//  
//  val results00005 = for (t <- times00005)  yield Sgp4LaraResult(sgps(0), sgp00005.propagate(t), tle00005, t)
//  val results06251 = for (t <- times06251)  yield Sgp4LaraResult(sgps(1), sgp06251.propagate(t), tle06251, t)
//  val results28057 = for (t <- times28057)  yield Sgp4LaraResult(sgps(2), sgp28057.propagate(t), tle28057, t)
//
//  test(s"${sgpImpl}: compare Intermediate result t=0") { 
//    checkIntl5(results00005(0))
//    checkSgp4Init5(results00005(0))
//    checkIntl6251(results06251(0))
//    checkSgp4Init6251(results06251(0))
//  }
//  
//  test(s"${sgpImpl}: compare Intermediate Propagation Results with Vallado's cpp implementation for near TLEs") {
//    // call the checks for the corresponding result
//    check00005 zip results00005 foreach { p => p._1(p._2)(toMinus9) }
//    check06251 zip results06251 foreach { p => p._1(p._2)(toMinus9) }
//    check28057 zip results28057 foreach { p => p._1(p._2)(toMinus9) }   
//  }
//
//  test(s"${sgpImpl}: compare Position/Velocity Propagation Results with Vallado's cpp implementation for near TLEs") {
//    // call the checks for the corresponding result
//    pvCheck00005 zip results00005 foreach { p => p._1(p._2)(toMinus9) }
//    pvCheck06251 zip results06251 foreach { p => p._1(p._2)(toMinus9) }
//    pvCheck28057 zip results28057 foreach { p => p._1(p._2)(toMinus9) }   
//  }
} 


