package predict4s.sgp.vallado

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import predict4s.sgp.{NearTLEs}
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.report.ValladoFileReport
import predict4s.coord.SGPElems
import predict4s.coord.SGPElemsConversions
import org.scalatest.Ignore

@Ignore
class HardcodedValladoReport extends FunSuite with NearTLEs with ValladoNearTLEsCheck[Double] with ValladoNearTLEsPVCheck[Double] {
 
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
  
  val results00005 = for (t <- times00005) yield sgp00005.propagate2PolarNodalContext(t)
  val results06251 = for (t <- times06251) yield sgp06251.propagate2PolarNodalContext(t)
  val results28057 = for (t <- times28057) yield sgp28057.propagate2PolarNodalContext(t)
  
  ValladoFileReport.save(results00005, tle00005, lines(0), times00005)
  ValladoFileReport.save(results06251, tle06251, lines(1), times06251)
  ValladoFileReport.save(results28057, tle28057, lines(2), times28057)
} 


