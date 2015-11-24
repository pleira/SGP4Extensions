package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
  
import predict4s._

/**
 *  True equator, mean equinox (TEME)
 *  
 *  Transformations done in this reference system.
 */
object TEME extends ReferenceSystem {
     
  def sgpElems[F: Field: Trig](tle: TLE) :  SGPElems[F] = { 
    val e0 = tle.eccentricity.toDouble.as[F]
    val i0 = tle.inclination.toDouble.toRadians.as[F]
    val pa = tle.argumentOfPeriapsis.toDouble.toRadians.as[F]
    val raan = tle.rightAscension.toDouble.toRadians.as[F]
    val meanAnomaly =  tle.meanAnomaly.toDouble.toRadians.as[F]
    def meanMotion = tle.meanMotion.toDouble.as[F]
    val year = tle.year
    val bStar = tle.atmosphericDragCoeficient.toDouble.as[F]
    // in julian days
    val epoch : F = {
      val mdhms = days2mdhms(tle.year, tle.epoch.toDouble)
      (jday(tle.year, mdhms._1, mdhms._2, mdhms._3, mdhms._4, mdhms._5) - 2433281.5).as[F]
    }
    val ω0 = pa
    val Ω0 = raan
    val M0 = meanAnomaly    
    val radpm0 = revPerDay2RadPerMin(meanMotion)
    SGPElems[F](radpm0,e0,i0,ω0,Ω0,M0, 0.as[F], bStar,epoch)
  }

  // FIXME
//  def dpState[F: Field](tle: TLE)(implicit wgs: SGPConstants[F]) :  DpTransform.DpState[F] = 
//    DpTransform.dpState(sgpElems(tle))
//
//  def geoState[F: Field](tle: TLE)(implicit wgs: SGPConstants[F]) : GeoPotentialState[F] =
//    GeoPotentialState(dpState(tle))
}