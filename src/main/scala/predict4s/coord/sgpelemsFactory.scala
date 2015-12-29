package predict4s.coord
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._


object SGPElemsFactory {
 
  /**
   * Builds SGPElems with original mean motion (n0'', n0dp) and semimajor axis (a0'' , a0dp).
   * 
   */  
  def sgpElemsAndContext[F: Field: Trig: NRoot: Order](tle: TLE)(implicit wgs: SGPConstants[F]) 
      :  (SGPElems[F], Context0[F]) = { 
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
      val mdhms = TimeUtils.days2mdhms(tle.year, tle.epoch.toDouble)
      (TimeUtils.jday(tle.year, mdhms._1, mdhms._2, mdhms._3, mdhms._4, mdhms._5) - 2433281.5).as[F]
    }
    val ω0 = pa
    val Ω0 = raan
    val M0 = meanAnomaly    
    val radPerMin0 = TimeUtils.revPerDay2RadPerMin(meanMotionPerDay)
    //val context0 = Context0(i0, e0)
    val elem0Ctx = calcContextAndOriginalMotionAndSemimajorAxis(e0,i0,ω0,Ω0,M0, bStar,epoch, radPerMin0, wgs)
    elem0Ctx
    // (SGPElems[F](n0,e0,i0,ω0,Ω0,M0,a0,bStar,epoch), context0)
  }

  private def calcContextAndOriginalMotionAndSemimajorAxis[F: Field: NRoot : Order: Trig](
      e: F,I: F,ω: F,Ω: F,M: F, bStar: F,epoch: F, radPerMin: F, wgs: SGPConstants[F]) 
    : (SGPElems[F], Context0[F]) = {
    val `e²` : F = e*e
    val s : F = sin(I)
    val c : F = cos(I)
    val `c²` : F = c*c
    val x3thm1 = 3*`c²` - 1
    val `β0²` = 1 - `e²`
    val β0 = `β0²`.sqrt
    val `β0³` = β0 * `β0²`
    val (n, a) = calcOriginalMotionAndSemimajorAxis(radPerMin,x3thm1,`β0³`,wgs)
    val context0 = Context0(a, `e²`,s,c,`c²`, x3thm1,β0,`β0²`,`β0³`, wgs)
    (SGPElems[F](n,e,I,ω,Ω,M,a,bStar,epoch), context0)
  }
  
  private def calcOriginalMotionAndSemimajorAxis[F: Field: NRoot : Order: Trig](n: F, x3cos2Im1: F, `β0³`: F, wgs: SGPConstants[F]) 
      : (F, F) = {
    import wgs.{KE,J2}
    
    val a1   = (KE / n) fpow (2.0/3.0).as[F]  // (Ke / n0) pow 1.5   
    val tval = 3 * J2 * x3cos2Im1 / `β0³` / 4  // 3 * k2 * (3*`cos²I0` - 1) / ((1-`e0²`) pow 1.5) / 4 
    val δ1   = tval / (a1*a1)
    val a0   = a1 * (1 - δ1 * (1.as[F]/3.as[F] + δ1 * (1 + 134 * δ1 / 81)))
    val δ0   = tval / (a0 * a0)  
    val n0dp = n   / (1 + δ0) 
    val a0dp = (KE / n0dp) fpow (2.as[F]/3.as[F])  // a0   / (1 - δ0)
    (n0dp, a0dp)
  }
}

case class Context0[F: Field: NRoot : Trig](
    a0: F, `e²`: F,s: F,c: F,`c²`: F, x3thm1: F,β0: F,`β0²`: F,`β0³`: F, private val wgs: SGPConstants[F]) {

  def cosI0      = c 
  def `cos²I0`   = `c²`
  def θ          = c
  def `θ²`       = `cos²I0`
  def `θ³`       = cosI0 * `cos²I0`
  def `θ⁴`       = `cos²I0` * `cos²I0`
  def θsq        = `cos²I0`
  def sinI0      = s
  // def sinio      = s

  val `β0⁴`      = `β0²`*`β0²`
  def e0sq       = e*e
  def β0sq       = `β0²`
  def β0to3      = `β0³`
  def β0to4      = `β0⁴`
  // def rteosq     = β0sq
  def omeosq     = β0sq
  def polyδ1 = poly"-134/81x^3 - x^2 - 1/3x + 1" 
    
  val `s²` : F = s*s
  val p : F = a0 * `β0²` // a0 * (1 - `e²`) // semilatus rectum , which also is G²/μ, with G as the Delauney's action, the total angular momentum
  val `p²` = p*p
  val `p⁴` = `p²`*`p²`
  val `1/p²` = 1/`p²`
  val `1/p⁴` = 1/`p⁴`
  import wgs.{aE=>α,J2,`J2/J3`,`J3/J2`}
  val `α/p` : F = α/p
//  val ϵ2 : F = -J2*(`α/p`**2) / 4
//  val ϵ3 : F = (`J3/J2`)*`α/p` / 2      // or (`C30/C20`)*`α/p` / 2   
// FIXME  val η : F = β0  // (1 - `e²`).sqrt           // eccentricity function G/L, with G as the Delauney's action, the total angular momentum , and L = √(μ a)
//  val x3thm1     = 3*`c²` - 1
  val con41      = x3thm1
  val con42      = 1 - 5*`c²`
  val x1mth2     = 1 - `c²`
  val x7thm1 : F = 7*`c²` - 1     
}
