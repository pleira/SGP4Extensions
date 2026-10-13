package sgp4.report

import sgp4.earth_gravity._
import sgp4.model._

object HardcodedLyddaneLongReport extends App with TLE00005 with TLE06251 with TLE28057 {

  val wgs = SGP72Constants.tleDoubleConstants
  val times00005 = for (i <- 0 until 13; j = i * 360) yield j // times in minutes
  val times06251 = for (i <- 0 until 25; j = i * 120) yield j // times in minutes
  val times28057 = for (i <- 0 until 25; j = i * 120) yield j // times in minutes
  val tles = List(tle00005, tle06251, tle28057)
  def propags: List[SGP4ValladoLong[Double]] = tles map { tle =>
    import spire.std.any.DoubleAlgebra
    SGP4ValladoLong.fromTLE[Double](tle, wgs).get
  }
  val sgps = propags

  def sgp00005 = sgps(0)
  def sgp06251 = sgps(1)
  def sgp28057 = sgps(2)

  val results00005 = for (t <- times00005) yield sgp00005.propagate2SPNContext(t)
  val results06251 = for (t <- times06251) yield sgp06251.propagate2SPNContext(t)
  val results28057 = for (t <- times28057) yield sgp28057.propagate2SPNContext(t)

}
