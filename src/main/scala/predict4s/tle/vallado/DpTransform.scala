package predict4s.tle.vallado

import spire.algebra._
import spire.math._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._
import predict4s.tle.SGPConstants
import predict4s.tle.TEME

object DpTransform {
 
  case class DpState[F: Field: Order] private[DpTransform] (val elem: TEME.SGPElems[F], val ctx: Context0[F]) {
    import elem.{a,e,n},ctx.wgs.aE
    import spire.implicits._
  
    // radius of perigee
    val rp     = a*(1-e)
    assert (rp > 1)
      
    // perigee height, altitude relative to the earth's surface, so perige instead of perigee 
    val perige =  (rp - 1) * aE
    
    // The SDP4 algorithm should be used if deep space
    def isDeepSpace = (2*pi / n) >= 225
    def useSDP4     = isDeepSpace
   
    // FIXME
    val isImpacting : Boolean    = rp < (220/aE + 1)
    def isImpactingBis : Boolean = perige < 220/aE 
    
  }

//  case class DelaunayElems[F](
//        ℓ : F, L : F,  // mean anomaly and its conjugate momentum L = √µ a ,
//        g : F, G : F,  // the argument of the perigee g = ω and its conjugate momentum G = L√(1 − e*e)
//                       // (the total angular momentum), where e is the orbital eccentricity, 
//        h : F, H : F   // the argument of the node h and its conjugate momentum H = G cosI (the polar component of the angular momentum).       
//  )
  
// def classical2DelaunayElems[F: Field: Trig: NRoot]( oe : ClassicalElems[F], MU : F) : DelaunayElems[F] = {
////  def classical2DelaunayElems[F: Field: Trig: NRoot]( oe : ClassicalElems[F])(implicit wgs: WGSConstants[F]) : DelaunayElems[F] = {
//    import oe._
//    val L = (MU*a).sqrt; val G = L*(1 - e*e).sqrt; val H = G*cos(i)
//    DelaunayElems(M, L, ω, G, Ω, H)
//  }
  
/**
 *  Delaunay elements at epoch can be obtained. In pseudo-code:
 *   val L = (MU*a).sqrt; val G = L*(1 - e*e).sqrt; val H = G*cos(i)
 *     DelaunayElems(M, L, ω, G, Ω, H)
 *  Note that the Delaunay actions, viz. L, G, and H, are never computed in Vallado's SGP4, 
 *  which uses n, e, and I instead.
*/
  case class Context0[F: Field: NRoot : Trig](I0 : F, e0 : F)(implicit val wgs: SGPConstants[F]) {
    // some constants here, that differ with the aE scale from Vallado's 
//    import wgs.aE,wgs.J2,wgs.J3,wgs.J4,wgs.MU
    import spire.implicits._
    
//    val k2         = J2 * aE * aE / 2
//    val k4         = -3 * J4 * aE**4 / 8
//    val Ke         = MU // sqrt(GM) where G is Newton’s universal gravitational constant and M is the mass of the Earth
//    val A30        = - J3 * aE**3
    val cosI0      = cos(I0)
    val `cos²I0`   = cosI0 * cosI0
    val θ          = cosI0
    val `θ²`       = `cos²I0`
    val `θ³`       = cosI0 * `cos²I0`
    val `θ⁴`       = `cos²I0` * `cos²I0`
    def θsq        = `cos²I0`
    val sinI0      = sin(I0)
    def sinio      = sinI0
    def x3thm1     = 3*`θ²` - 1
    def con41      = x3thm1
    def con42      = 1 - 5*`θ²`
    def x1mth2     = 1 - `θ²`
    
    val `e0²`      = e0*e0
    val `β0²`      = 1 - `e0²`
    val β0         = `β0²`.sqrt
    val `β0³`      = β0 * `β0²`
    val `β0⁴`      = `β0²`*`β0²`
    def e0sq       = e0*e0
    def β0sq       = `β0²`
    def β0to3      = `β0³`
    def β0to4      = `β0⁴`
    // def rteosq     = β0sq
    def omeosq     = β0sq
    def polyδ1 = poly"-134/81x^3 - x^2 - 1/3x + 1" 
   
  }      

  /**
   * Recover original mean motion (n0'', n0dp) and semimajor axis (a0'' , a0dp).
   * 
   * The state returned contains a reference to the constants.
   */
  def dpState[F: Field: NRoot : Order: Trig](ini : TEME.SGPElems[F])(implicit wgs: SGPConstants[F]) :  DpState[F] = {
    import ini.{i => I0,e => e0,n => n0,ω => ω0,Ω => Ω0,M =>M0, bStar}
    val ctx = Context0(I0, e0)
    import spire.implicits._
    import wgs.{KE,J2}
    import ctx._ // ctx.`cos²I0`, ctx.`e0²`,ctx.k2,ctx.Ke
    val a1 : F   = (KE / n0) fpow (2.0/3.0).as[F]  // (Ke / n0) pow 1.5   
    val tval : F = (3.0/4.0) * J2 * x3thm1 / β0to3  // 3 * k2 * (3*`cos²I0` - 1) / ((1-`e0²`) pow 1.5) / 4 
    val δ1   = tval / (a1*a1)
    val a0   = a1 * (1 - δ1 * (1.0/3.0 + δ1 * (1 + 134 * δ1 / 81)))
    val δ0   = tval / (a0 * a0)  
    val n0dp = n0   / (1 + δ0) 
    val a0dp = (KE / n0dp) fpow (2.0/3.0).as[F]  // a0   / (1 - δ0)
    import ini._
    val dpState0 : DpState[F] = DpState(TEME.SGPElems(n0dp, e0, I0, ω0, Ω0, M0, a0dp, bStar, epoch), ctx)
    dpState0
  }
}
