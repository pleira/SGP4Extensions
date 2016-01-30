package predict4s.sgp.algo

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.coord.SGPElems
import predict4s.coord.SGPElemsConversions


trait ValladoNearTLEsTest extends NearTLEs with ValladoNearTLEsCheck[Double] with ValladoNearTLEsPVCheck[Double] { self : FunSuite => 

  implicit val wgs = SGP72Constants.tleDoubleConstants
  
  // Propagators for all TLEs
  def propags : List[SGP4Vallado[Double]]
  
  def sgpImpl : String

  def results = (propags,tles, tlesTimes).zipped.toList map { p  =>  // tuple of propagator,  tle and the propagation times   
    val sgp4 = p._1 ; val tle = p._2; val times = p._3
    val res = for (t <- times) yield (sgp4.propagate(t), t)
    (sgp4, res, tle)
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
  
  val results00005 = for (t <- times00005)  yield Sgp4ValladoResult(sgps(0), sgp00005.propagate(t).get, tle00005, t)
  val results06251 = for (t <- times06251)  yield Sgp4ValladoResult(sgps(1), sgp06251.propagate(t).get, tle06251, t)
  val results28057 = for (t <- times28057)  yield Sgp4ValladoResult(sgps(2), sgp28057.propagate(t).get, tle28057, t)
  
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


