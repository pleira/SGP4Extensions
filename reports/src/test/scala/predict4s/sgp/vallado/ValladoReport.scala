package predict4s.sgp.vallado

import predict4s.sgp._
import predict4s.report.ValladoFileReport
import predict4s.coord._
/*
object HardcodedValladoReport extends App with NearTLEs {
 
  val wgs = SGP72Constants.tleDoubleConstants
  val times00005 = for (i <- 0 until 13; j = i*360) yield j // times in minutes 
  val times06251 = for (i <- 0 until 25; j = i*120) yield j // times in minutes
  val times28057 = for (i <- 0 until 25; j = i*120) yield j // times in minutes

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

*/