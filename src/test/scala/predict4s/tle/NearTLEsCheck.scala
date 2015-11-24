package predict4s.tle

import org.scalatest.FunSuite

import org.scalautils.TolerantNumerics

import spire.algebra._
import spire.math._
import spire.implicits._

trait NearTLEsCheck extends NearTLEs with ValladoNearTLEsCheck with ValladoNearTLEsPVCheck { self : FunSuite => 

  implicit val wgs = SGP72Constants.tleDoubleConstants
  
  // Propagators for all TLEs
  def propags : List[SGP4[Double]]
  
  def sgpImpl : String

  // List with List of Propagation results for each TLE and all propagation times 
  def results : List[IndexedSeq[Sgp4Result]] = ((propags zip tles) zip tlesTimes) map { p  =>  // pair of (pair propagator with tle) with the propagation times   
    val sgp4 = p._1._1 ; val tle = p._1._2; val times = p._2
    for (t <- times) yield Sgp4Result(sgp4.propagate(t), sgp4.state0, tle)
  }
  
  test(s"${sgpImpl}: compare Position/Velocity Propagation Results with Vallado's cpp implementation for near TLEs") ({
    // call the checks for the corresponding result 
    (pvNearChecks zip results) foreach  { l =>  // pair with the lists containing the checks and the results
      (l._1 zip l._2) foreach { p => p._1(p._2) } 
    }
  })
}

class HardcodedValladoNearTLEsCheck extends FunSuite with NearTLEs with ValladoNearTLEsCheck with ValladoNearTLEsPVCheck {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants

  def propags : List[SGP4Vallado[Double]] = tles map (tle => SGP4Vallado[Double](tle) )
  def sgpImpl : String = "Vallado"
  
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

class LaraNearTLEsCheck extends FunSuite with NearTLEsCheck {
  override def sgpImpl : String = "Lara"  
  override def propags : List[SGP4Lara[Double]]  = tles map (tle => SGP4Lara[Double](tle) )
}


class LaraBNearTLEsCheck extends FunSuite with NearTLEsCheck {
  override def sgpImpl : String = "LaraB"
  override def propags : List[SGP4LaraB[Double]]  = tles map (tle => SGP4LaraB[Double](tle) )
}




