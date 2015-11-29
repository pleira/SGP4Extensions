package predict4s.tle
import spire.algebra._
import spire.math._
import spire.implicits._

class Context0[F: Field: NRoot : Trig](I : F, e : F, val wgs: SGPConstants[F]) {
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

object Context0 {
 
  def apply[F: Field: NRoot : Order: Trig](elem: TEME.SGPElems[F])(implicit wgs: SGPConstants[F]) = new Context0(elem.I, elem.e, wgs)
 
}