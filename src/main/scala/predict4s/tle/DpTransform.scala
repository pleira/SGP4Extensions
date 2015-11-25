package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._


object DpTransform {
 
  // this class should not be constructed elsewhere
  case class DpState[F: Field: Order] private[DpTransform] (val elem: TEME.SGPElems[F], val ctx: Context0[F]) {
    import elem.{a,e,n},ctx.wgs.aE
  
    // radius of perigee
    val rp     = a*(1-e)
    assert (rp > aE)
      
    // perigee height, altitude relative to the earth's surface, so perige instead of perigee 
    val perige =  rp - aE
    
    // The SDP4 algorithm should be used if deep space
    def isDeepSpace = (2*pi / n) >= 225
    def useSDP4     = isDeepSpace
   
    // FIXME
    val isImpacting : Boolean    = rp < (220/aE + 1)
    def isImpactingBis : Boolean = perige < 220/aE 
    
  }

  case class Context0[F: Field: NRoot : Trig](i0 : F, e0 : F)(implicit val wgs: SGPConstants[F]) {
    // some constants here, that differ with the aE scale from Vallado's 
    import wgs.aE,wgs.J2,wgs.J3,wgs.J4,wgs.MU
    
    val k2         = J2 * aE * aE / 2
    val k4         = -3 * J4 * aE**4 / 8
    val Ke         = MU // sqrt(GM) where G is Newton’s universal gravitational constant and M is the mass of the Earth
    val A30        = - J3 * aE**3
    val cosi0      = cos(i0)
    val `cos²i0`   = cosi0 * cosi0
    val θ          = cosi0
    val `θ²`       = `cos²i0`
    val `θ³`       = cosi0**3
    val `θ⁴`       = cosi0**4
    def θsq        = `cos²i0`
    val sini0      = sin(i0)
    def sinio      = sini0
    def x3thm1     = 3*`θ²` - 1
    def con41      = x3thm1
    def con42      = 1 - 5*`θ²`
    def x1mth2     = 1 - `θ²`
    
    val `e0²`      = e0*e0
    val `β0²`      = 1-`e0²`
    val β0         = `β0²`.sqrt
    val `β0³`      = β0 * `β0²`
    val `β0⁴`      = `β0²`*`β0²`
    def e0sq       = e0*e0
    def β0sq       = `β0²`
    def β0to3      = `β0³`
    def β0to4      = `β0⁴`
    def rteosq     = β0sq
    
    def polyδ1 = poly"-134/81x^3 - x^2 - 1/3x + 1" 
   
  }      

  /**
   * Recover original mean motion (n0'', n0dp) and semimajor axis (a0'' , a0dp).
   * 
   * The state returned contains a reference to the constants.
   */
  def dpState[F: Field: NRoot : Order: Trig](ini : TEME.SGPElems[F])(implicit wgs: SGPConstants[F]) :  DpState[F] = {
    import ini.{i => i0,e => e0,n => n0,ω => ω0,Ω => Ω0,M =>M0, bStar}
    val ctx = Context0(i0, e0)

    import ctx.`cos²i0`, ctx.`e0²`,ctx.k2,ctx.Ke
    val a1   = (Ke / n0) pow 1.5   
    val tval = 3 * k2 * (3*`cos²i0` - 1) / ((1-`e0²`) pow 1.5) / 4 
    val δ1   = tval / (a1*a1)
    val a0   = a1 * (1 - δ1 * (1.0/3.0 + δ1 * (1 + 134 * δ1 / 81)))
    val δ0   = tval / (a0 * a0)  
    val n0dp = n0   / (1 + δ0) 
    val a0dp = a0   / (1 - δ0)
    import ini._
    val dpState0 : DpState[F] = DpState(TEME.SGPElems(n0dp, e0, i0, ω0, Ω0, M0, a0dp, bStar, epoch), ctx)
    dpState0
  }
}
