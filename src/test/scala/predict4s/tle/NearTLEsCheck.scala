package predict4s.tle

import org.scalatest.FunSuite

import org.scalautils.TolerantNumerics

import spire.algebra._
import spire.math._
import spire.implicits._

class NearTLEsCheck extends FunSuite with NearTLEs with ValladoNearTLEsCheck with ValladoNearTLEsPVCheck {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants

  def propags : List[SGP4[Double]] = tles map (tle => SGP4Brouwer[Double](tle) )
  def sgpImpl : String = "Brouwer"
  
  val sgps     = propags
  
  val tle5     = tles(0) ;  val sgp5      = sgps(0)
  val tle6251  = tles(1) ;  val sgp6251   = sgps(1)
  val tle28057 = tles(2) ;  val sgp28057  = sgps(2)
  
  val results5     = for (t <- times5)      yield Sgp4Result(sgp5.propagate(t), tle5)
  val results6251  = for (t <- times6251)   yield Sgp4Result(sgp6251.propagate(t), tle6251)
  val results28057 = for (t <- times28057)  yield Sgp4Result(sgp28057.propagate(t), tle28057)

  test(s"${sgpImpl}: compare Intermediate Propagation Results with Vallado's cpp implementation for near TLEs") ({
    
    // call the checks for the corresponding result
    check5     zip results5     foreach { p => p._1(p._2) }
    check6251  zip results6251  foreach { p => p._1(p._2) }
    check28057 zip results28057 foreach { p => p._1(p._2) }   
   
  })
  
  test(s"${sgpImpl}: compare Position/Velocity Propagation Results with Vallado's cpp implementation for near TLEs") ({
    
    // call the checks for the corresponding result
    pvCheck5     zip results5     foreach { p => p._1(p._2) }
    pvCheck6251  zip results6251  foreach { p => p._1(p._2) }
    pvCheck28057 zip results28057 foreach { p => p._1(p._2) }   
   
  })
  
} 

class LaraNearTLEsCheck extends NearTLEsCheck {
   override def propags : List[SGP4[Double]]  = tles map (tle => SGP4Lara[Double](tle) )
   override def sgpImpl : String = "Lara"
}
