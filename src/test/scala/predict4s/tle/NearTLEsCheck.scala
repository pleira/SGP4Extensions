package predict4s.tle

import org.scalatest.FunSuite

import org.scalautils.TolerantNumerics

import spire.algebra._
import spire.math._
import spire.implicits._

trait NearTLEsCheck extends NearTLEs  {
  def propags : List[SGP4[Double]]
  def sgpImpl : String
}

class HardcodedBrouwerNearTLEsCheck extends FunSuite with NearTLEs with ValladoNearTLEsCheck with ValladoNearTLEsPVCheck {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants

  def propags : List[SGP4Brouwer[Double]] = tles map (tle => SGP4Brouwer[Double](tle) )
  def sgpImpl : String = "Brouwer"
  
  val sgps     = propags
  
  def sgp00005 = sgps(0)
  def sgp06251 = sgps(1)
  def sgp28057 = sgps(2)
  
  val results00005 = for (t <- times00005)  yield Sgp4Result(sgp00005.propagate(t), sgp00005.state0, tle00005)
  val results06251 = for (t <- times06251)  yield Sgp4Result(sgp06251.propagate(t), sgp06251.state0, tle06251)
  val results28057 = for (t <- times28057)  yield Sgp4Result(sgp28057.propagate(t), sgp28057.state0, tle28057)

  test(s"${sgpImpl}: compare Intermediate Propagation Results with Vallado's cpp implementation for near TLEs") ({
    
    // call the checks for the corresponding result
    check00005 zip results00005 foreach { p => p._1(p._2) }
    check06251 zip results06251 foreach { p => p._1(p._2) }
    check28057 zip results28057 foreach { p => p._1(p._2) }   
   
  })
  
  test(s"${sgpImpl}: compare Position/Velocity Propagation Results with Vallado's cpp implementation for near TLEs") ({
    
    // call the checks for the corresponding result
    pvCheck00005 zip results00005 foreach { p => p._1(p._2) }
    pvCheck06251 zip results06251 foreach { p => p._1(p._2) }
    pvCheck28057 zip results28057 foreach { p => p._1(p._2) }   
   
  })
  
} 

class LaraNearTLEsCheck extends FunSuite with NearTLEsCheck with ValladoNearTLEsCheck with ValladoNearTLEsPVCheck  {
  
  implicit val wgs = SGP72Constants.tleDoubleConstants
  override def sgpImpl : String = "Lara"
  
  // Lara's Propagators for all TLEs
  val propags : List[SGP4Lara[Double]]  = tles map (tle => SGP4Lara[Double](tle) )
  
  def proc(sgp4 : SGP4Lara[Double], tle: TLE, times: IndexedSeq[Int]) = for (t <- times) yield Sgp4Result(sgp4.propagate(t), sgp4.state0, tle)
    
  // List with List of Propagation results for each TLE and all propagation times 
  val results : List[IndexedSeq[Sgp4Result]] = ((propags zip tles) zip tlesTimes) map { p  =>  // pair of (pair propagator with tle) with the propagation times   
    val sgp4 = p._1._1 ; val tle = p._1._2; val times = p._2
    proc(sgp4, tle, times)
  }
  
  test(s"${sgpImpl}: compare Position/Velocity Propagation Results with Vallado's cpp implementation for near TLEs") ({
    // call the checks for the corresponding result 
    (pvNearChecks zip results) foreach  { l =>  // pair with the lists containing the checks and the results
      (l._1 zip l._2) foreach { p => p._1(p._2) } 
    }
  })
  
}
