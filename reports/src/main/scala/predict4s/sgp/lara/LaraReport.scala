package predict4s.sgp.lara

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import predict4s.sgp.{NearTLEs}
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.report.LaraFileReport
import predict4s.coord.SGPElems
import predict4s.coord.SGPElemsConversions

class HardcodedLaraReport extends FunSuite with NearTLEs with ValladoNearTLEsCheck[Double] with ValladoNearTLEsPVCheck[Double] {
 
//  implicit val wgs = SGP72Constants.tleDoubleConstants
//  val toMinus9 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(1E-9)
//
//  def propags : List[SGP4Lara[Double]] = tles map {tle => 
//    import spire.std.any.DoubleAlgebra
//    SGP4Lara[Double](tle, wgs)
//  }
  def sgpImpl : String = "Lara SGP4"
//  
//  val sgps     = propags
//  
//  def sgp00005 = sgps(0)
//  def sgp06251 = sgps(1)
//  def sgp28057 = sgps(2)
//  
//  // results with Lara's variables
//  val lresults00005 = for (t <- times00005) yield sgp00005.propagate2PolarNodalContext(t)
//  val lresults06251 = for (t <- times06251) yield sgp06251.propagate2PolarNodalContext(t)
//  val lresults28057 = for (t <- times28057) yield sgp28057.propagate2PolarNodalContext(t)
  
  // transform Lara's variables to SpecialPolarNodals
//  LaraFileReport.save(results00005, tle00005, lines(0), times00005)
//  LaraFileReport.save(results06251, tle06251, lines(1), times06251)
//  LaraFileReport.save(results28057, tle28057, lines(2), times28057)
} 


