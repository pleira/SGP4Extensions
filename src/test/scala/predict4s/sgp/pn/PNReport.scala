package predict4s.sgp.pn

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import predict4s.sgp.{NearTLEs}
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.report.PolarNodalFileReport
import predict4s.coord.SGPElems
import predict4s.coord.SGPElemsFactory
import org.scalatest.Ignore

@Ignore
class HardcodedPNReport extends FunSuite with NearTLEs with ValladoNearTLEsCheck[Double] with ValladoNearTLEsPVCheck[Double] {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants

  def propags : List[SGP4PN[Double]] = tles map {tle => 
    import spire.std.any.DoubleAlgebra
    val elem0AndCtx = SGPElemsFactory.sgpElemsAndContext(tle)
    SGP4PN[Double](BrouwerLaneSecularCorrections(elem0AndCtx))
  }
  def sgpImpl : String = "PN SGP4"
  
  val sgps     = propags
  
  def sgp00005 = sgps(0)
  def sgp06251 = sgps(1)
  def sgp28057 = sgps(2)
  
  val results00005 = for (t <- times00005) yield sgp00005.propagate2PolarNodalContext(t)
  val results06251 = for (t <- times06251) yield sgp06251.propagate2PolarNodalContext(t)
  val results28057 = for (t <- times28057) yield sgp28057.propagate2PolarNodalContext(t)
  
  PolarNodalFileReport.save(results00005, tle00005, lines(0), times00005)
  PolarNodalFileReport.save(results06251, tle06251, lines(1), times06251)
  PolarNodalFileReport.save(results28057, tle28057, lines(2), times28057)
} 


