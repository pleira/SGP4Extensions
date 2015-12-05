package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
  
import predict4s._


case class SGPElems[F](
        n : F, // mean motion 
        e : F, // eccentricity
        I : F, // inclination
        ω : F, // argument Of perigee
        Ω : F, // right ascension ascending node
        M : F, // mean anomaly
        a : F, // semimajor axis (apogee)
        bStar : F, // atmospheric Drag Coeficient
        epoch : F) // epoch time in days from jan 0, 1950. 0 hr 

/**
 * Builds SGPElems with original mean motion (n0'', n0dp) and semimajor axis (a0'' , a0dp).
 * 
 */        
object SGPElems {

  def sgpElemsAndContext[F: Field: Trig: NRoot: Order](tle: TLE)(implicit wgs: SGPConstants[F]) :  (SGPElems[F], Context0[F]) = { 
    val e0 = tle.eccentricity.toDouble.as[F]
    val i0 = tle.inclination.toDouble.toRadians.as[F]
    val pa = tle.argumentOfPeriapsis.toDouble.toRadians.as[F]
    val raan = tle.rightAscension.toDouble.toRadians.as[F]
    val meanAnomaly =  tle.meanAnomaly.toDouble.toRadians.as[F]
    val meanMotionPerDay = tle.meanMotion.toDouble.as[F]
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
    val radPerMin0 = revPerDay2RadPerMin(meanMotionPerDay)
    val context0 = Context0(i0, e0)
    val (n0, a0) : (F,F) = calcOriginalMotionAndSemimajorAxis(radPerMin0, context0, wgs)
    (SGPElems[F](n0,e0,i0,ω0,Ω0,M0,a0,bStar,epoch), context0)
  }

  private def calcOriginalMotionAndSemimajorAxis[F: Field: NRoot : Order: Trig](n: F, context0: Context0[F], wgs: SGPConstants[F]) 
      : (F, F) = {
    import context0._ 
    import wgs.{KE,J2}
    
    val a1   = (KE / n) fpow (2.0/3.0).as[F]  // (Ke / n0) pow 1.5   
    val tval = (3.0/4.0) * J2 * x3thm1 / β0to3  // 3 * k2 * (3*`cos²I0` - 1) / ((1-`e0²`) pow 1.5) / 4 
    val δ1   = tval / (a1*a1)
    val a0   = a1 * (1 - δ1 * (1.0/3.0 + δ1 * (1 + 134 * δ1 / 81)))
    val δ0   = tval / (a0 * a0)  
    val n0dp = n   / (1 + δ0) 
    val a0dp = (KE / n0dp) fpow (2.0/3.0).as[F]  // a0   / (1 - δ0)
    (n0dp, a0dp)
  }
}

case class Context0[F: Field: NRoot : Trig](I : F, e : F) {
    // some constants here, that differ with the aE scale from Vallado's 
//    import wgs.aE,wgs.J2,wgs.J3,wgs.J4,wgs.MU
    
//    val k2         = J2 * aE * aE / 2
//    val k4         = -3 * J4 * aE**4 / 8
//    val Ke         = MU // sqrt(GM) where G is Newton’s universal gravitational constant and M is the mass of the Earth
//    val A30        = - J3 * aE**3
    val cosI0      = cos(I)
    val `cos²I0`   = cosI0 * cosI0
    val θ          = cosI0
    val `θ²`       = `cos²I0`
    val `θ³`       = cosI0 * `cos²I0`
    val `θ⁴`       = `cos²I0` * `cos²I0`
    def θsq        = `cos²I0`
    val sinI0      = sin(I)
    def sinio      = sinI0
    def x3thm1     = 3*`θ²` - 1
    def con41      = x3thm1
    def con42      = 1 - 5*`θ²`
    def x1mth2     = 1 - `θ²`
    
    val `e0²`      = e*e
    val `β0²`      = 1 - `e0²`
    val β0         = `β0²`.sqrt
    val `β0³`      = β0 * `β0²`
    val `β0⁴`      = `β0²`*`β0²`
    def e0sq       = e*e
    def β0sq       = `β0²`
    def β0to3      = `β0³`
    def β0to4      = `β0⁴`
    // def rteosq     = β0sq
    def omeosq     = β0sq
    def polyδ1 = poly"-134/81x^3 - x^2 - 1/3x + 1" 
   
}

/**
 *  True equator, mean equinox (TEME)
 *  
 *  Transformations done in this reference system.
 */
object TEME extends ReferenceSystem 