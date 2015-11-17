package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._

import spire.syntax.primitives._

/**
 * Contains literal Functions from SGP4
 * all equations below are taken from SPACETRACK Report #3 by Hoots and Roehrich
 *  
 * (Reference http://aero.tamu.edu/sites/default/files/faculty/alfriend/S6.1%20Hoots.pdf)
 */
//case class SGP4TIF[F: Field: NRoot : Order: Trig](val dp: DpState[F]) {
//
//}
case class IlCoefs[F: Field](gpState : GeoPotentialState[F]) {
   // coeficients for IL
   import gpState._
   import gcof._,dps._,gctx._
   val `C1²` = C1*C1
   val t2cof = 3*C1/2
   val t3cof = D2 + 2*`C1²`
   val t4cof = (3*D3 + C1*(12*D2 + 10 * `C1²`))/4
   val t5cof = (3*D4 + 12*C1*D3 + 6*D2*D2 + 15*`C1²`*(2*D2+`C1²`))/5
}

case class HootsOtherCoefs[F : Field: NRoot : Order: Trig](gpState : GeoPotentialState[F]) {
//case class HootsOtherCoefs[F : Field: NRoot : Order: Trig](elem: TEME.SGPElems[F],  ctx : Context0[F], 
//    ctx1 : Context1[F], coeff : GeoPotentialCoefs[F]) {
    
  import gpState._
  import gcof._,dps._,gctx._
  import ctx.wgs._,ctx._
  import elem.{n => n0,a => a0,ω => ω0, M => M0,_}
  
  // val gsto = gstime(epoch + 2433281.5) 
  
  val po    = a0*`β0²`
  val posq  = po*po
  
  // other derived coeficients and variables that can be used
  val Mcof  = if (e0 > 0.0001.as[F]) - 2*q0ms_ξ__to4 * bStar / e0η / 3 else 0.as[F]
  def xmcof  = Mcof
  val ωcof   = bStar*C3*cos(ω0)
  val pinvsq = 1 / posq
  val temp1  = 3 * J2 * pinvsq * n0 / 2
  val temp2  = temp1 * J2 * pinvsq / 2
  val temp3  = -0.46875 * J4 * pinvsq * pinvsq * n0
  val xhdot1 : F = - temp1*θ 
  val Ωcof   = 7 * `β0²` * xhdot1 * C1 / 2
  def nodecf = Ωcof
  val delM0   = (1+η*cos(M0))**3
  // sgp4fix for divide by zero with inco = 180 deg, // FIXME: not valid for deep space
  val xlcof =  if (abs(θ+1) > 1.5e-12.as[F]) - J3/J2 * sinio * (3 + 5*θ) / (1 + θ) / 4
               else                          - J3/J2 * sinio * (3 + 5*θ) / 1.5e-12 / 4
  val aycof   = - J3/J2 * sinio / 2  // FIXME: not valid for deep space
  val sinM0  = sin(M0)
  def sinmao  = sinM0
  val x7thm1  = 7*θsq - 1
   
//   val t2cof = 3*C1 / 2
//   val C1sq  = C1*C1
//   val t3cof = D2 + 2*C1sq
//   val t4cof = (3*D3 + C1*(12*D2 + 10 * C1sq))/4
//   val t5cof = (3*D4 + 12*C1*D3 + 6*D2*D2 + 15*C1sq*(2*D2+C1sq))/5
 
  val twopi : F = 2.as[F]*pi
   
 // val ehSecEffects = ehSecularEffects 
   
  // all quantities on the right hand side of equations are understood to be double prime mean elements.
  // that is, follow the Brower convention, and therefore, the names use n0 and a0 but
  // they refer to the double prime quantities

//         temp1  = 1.5 * j2 * pinvsq * satrec.no;
//         temp2  = 0.5 * temp1 * j2 * pinvsq;
//         temp3  = -0.46875 * j4 * pinvsq * pinvsq * satrec.no;

//         satrec.mdot     = satrec.no + 0.5 * temp1 * rteosq * satrec.con41 + 0.0625 *
//                            temp2 * rteosq * (13.0 - 78.0 * cosio2 + 137.0 * cosio4);
//         satrec.argpdot  = -0.5 * temp1 * con42 + 0.0625 * temp2 *
//                             (7.0 - 114.0 * cosio2 + 395.0 * cosio4) +
//                             temp3 * (3.0 - 36.0 * cosio2 + 49.0 * cosio4);
//         xhdot1            = -temp1 * cosio;
//         satrec.nodedot = xhdot1 + (0.5 * temp2 * (4.0 - 19.0 * cosio2) +
//                              2.0 * temp3 * (3.0 - 7.0 * cosio2)) * cosio;
  
 // def ehSecularEffects : TEME.OrbitalElements[F] = {
//      // derivative of the (MeanMotion?)
//      val Ṁ = n0*3*K2*((-1+3*θsq)/(2*a0sq*β0to3) + K2*(13-78*θsq + 137*θsq*θsq)/(16*a0to4*β0to4*β0to3)) 
//      // derivative of the perigee argument
//      val ωdot = n0*(3*K2*((-1+5*θsq)/(2*a0sq*β0to4) + K2*(7 - 114*θsq + 395*θsq*θsq)/(16*a0to4*β0to4*β0to4))
//          + 5*K4*(3-36*β0sq+49*β0to4)/(4*a0to4*β0to4*β0to4))
//      // derivative of the raan  
//      val Ωdot = n0*( 3*K2*θ/(a0sq*β0to4) + 3*K2*K2*θ*(4 - 19*θsq)/(2*a0to4*β0to4*β0to4)
//          + 5*K4*θ*(3-7*θsq)/(2*a0to4*β0to4*β0to4))
  val (_Mdot, ωdot, omegadot) : (F,F,F) = if (n0 >= 0.as[F] || `β0²` >= 0.as[F])
      ( 
        n0 + 0.5.as[F] * temp1 * β0 * con41 + 0.0625.as[F] * temp2 * β0 * (13 - 78 * `θ²` + 137 * `θ⁴`),
        //  n0dp + temp1 * β0sq * con41 / 2 + 0.0625 * temp2 * β0sq * (13 - 78*`θ²` + 137*`θ⁴`) / 16, 
      //val Ṁ = n0*3*K2*((-1+3*`θ²`)/(2*a0sq*β0to3) + K2*(13-78*`θ²` + 137*`θ⁴`)/(16*a0to4*β0to4*β0to3)) 
      
      // derivative of the perigee argument
      - temp1 * con42 /2 + temp2*(7 - 114*`θ²` + 395*`θ⁴`)/16 + temp3*(3 - 36*`θ²` + 49*`θ⁴`),
//      val ωdot = n0*(3*K2*((-1+5*`θ²`)/(2*a0sq*β0to4) + K2*(7 - 114*`θ²` + 395*`θ⁴`)/(16*a0to4*β0to4*β0to4))
//          + 5*K4*(3-36*`θ²`+49*`θ⁴`)/(4*a0to4*β0to4*β0to4))
      // derivative of the raan
      xhdot1 + (temp2 * (4 - 19*`θ²`)/2 + 2*temp3 * (3 - 7*`θ²`))*θ
      )
//      val Ωdot = n0*( 3*K2*θ/(a0sq*β0to4) + 3*K2*K2*θ*(4 - 19*`θ²`)/(2*a0to4*β0to4*β0to4)
//          + 5*K4*θ*(3-7*`θ²`)/(2*a0to4*β0to4*β0to4))
     else (0.as[F],0.as[F],0.as[F])
     
   def Ṁ = _Mdot  
  def Ωdot = omegadot
  def mdot = _Mdot

}
/*
case class InclFunctions[F: Field: Trig](val i0: F) {
    val θ      = cos(i0)
    def cosio  = θ
    def cosi0  = θ
    val θsq    = θ * θ
    val θto4   = θsq * θsq
    val sinio  = sin(i0)
    def sini0  = sinio
    val x3thm1 = 3*θsq - 1
    def con41  = x3thm1
    val con42  = 1 - 5*θsq
    def x1mth2 = 1 - θsq
  }

case class EccentricityFunctions[F: Field: NRoot](val e0: F) {
    val e0sq       = e0*e0
    val β0sq       = 1-e0sq
    lazy val β0    = β0sq.sqrt
    lazy val β0to3 = β0sq * β0
    val β0to4      = β0sq * β0sq
    def rteosq     = β0sq
}

case class BrowerMeanMotion[F: Field: Order: NRoot](n0k: F, i0f : InclFunctions[F], e0f : EccentricityFunctions[F])(implicit wgs: SGPConstants[F])  {
  import wgs._
  import i0f._
  import e0f._
  
    // recovery of the Brouwer mean motion from the Kozai mean motion
  // Hoots a1
  val a1  = (KE / n0k) fpow (2.0/3.0).as[F] // KE here contains Earth radius to 3/2
  // val a1 = aE * (MU/radpm0) fpow (TWO_THIRD)   // but in Hoots, KE is just sqrt(G*Mass_earth) * (E_radius/60) fpow (3/2)
  
  // 3 theta2 minus 1
  //val x3thm1 = 3 * theta2 - 1
    
    
  // Hoots δ0, δ1, original mean motion n0'' (double prime), original a0'' , semimajor axis a0 double prime
  // val tval_ = 3 * K2 * x3thm1 / β0to3 / 2
  val tval = 3 * J2 * x3thm1 / β0to3 / 4 // what happens to aE*aE? is the formula in page 10 of Spacetrack #3 correct?
  val δ1   = tval/(a1 * a1) 
  val a2   = a1 * (1 - δ1 * (1.0/3.0 + δ1 * (1 + 134 * δ1 / 81)))    //def a0 = a2
  val δ0   = tval/(a2 * a2)  
  val n0dp = n0k /(1 + δ0) 

  // mean semimajor axis 
  val a0dp = (KE / n0dp) fpow (2.0/3.0).as[F]
 
  val a0    = a0dp
  val a0sq  = a0*a0
  lazy val a0to4 = a0sq*a0sq
  val n0    = n0dp
  
  // use deep space
  def isDeepSpace = (2*pi / n0) >= 225
  
  // radius of perigee (note no aE term present in Vallado's)
  val rp      = a0*(1-e0) 
  
  def isImpacting : Boolean = rp < (220/aE + 1)
}

case class ScalcFunctions[F: Field: NRoot: Order: Trig](e0f : EccentricityFunctions[F], bmmf: BrowerMeanMotion[F])
  (implicit wgs: SGPConstants[F])  {
  import wgs._
  // import i0f._
  import e0f._
  import bmmf._
  
  val po    = a0*β0sq
  val posq  = po*po

  
  // perigee height, altitude relative to the earth's surface, so perige instead of perigee 
  val perige = (rp - 1)*aE  
  
  // The parameter q0 is the geocentric reference altitude, 
  // a constant equal to 120 km plus one Earth radius 
  val q0   = 1 + 120/aE 
  val s    = fittingAtmosphericParameter
  val q0ms_to4 = (q0 - s)**4 
    
  val ξ    = 1/(a0 - s)  // tsi
  val ξsq  = ξ*ξ
  lazy val ξto3 = ξsq*ξ
  val ξto4 = ξsq*ξsq
  val ξto5 = ξto4*ξ

  val η    = a0*e0*ξ   // eta
  val ηsq  = η*η       // etasq
  lazy val ηto3 = ηsq*η
  val ηto4  = ηsq*ηsq
  val e0η   = e0*η      // eeta 
  val psisq = abs[F](1-ηsq)  // Vallado's uses fabs
  // q0 minus s ξ  all to 4 
  val q0ms_ξ__to4 = q0ms_to4*(ξ**4)

 
  //val coef1  = coef / (psisq pow 3.5)
  def S_above156       =  (1 + 78/aE)
  def hs               =  perige - aE - 78   // interpolation, being a number bigger than 20, and smaller that 78
  def S_between_98_156 =  (1 + hs/aE)
  def S_below98        =  (1 + 20/aE)

  /* the parameter s is a fitting parameter in density representation.
   * It is determined based of epoch perigee
   * height above a spherical Earth. If perigee height is greater than or equal 156 km, the value of s is
   * fixed to be 78 km plus one Earth radius. For altitudes greater than or equal to 98 km but less
   * than 156 km, s is defined to be perigee height minus 78 km plus one Earth radius. 
   * For altitudes below 98 km, s is 20 km plus one Earth radius.
   */
  private def fittingAtmosphericParameter : F = 
     if (perige >= 156)       S_above156
     else if (perige >= 98)   S_between_98_156
     else                     S_below98  
  
}

case class CoefFunctions[F: Field: NRoot: Order: Trig](ω0: F, bStar: F, i0f : InclFunctions[F], e0f : EccentricityFunctions[F], 
    bmmf: BrowerMeanMotion[F], sf: ScalcFunctions[F])(implicit wgs: SGPConstants[F])  {
  import wgs._
  import i0f._
  import e0f._
  import bmmf._
  import sf._
//val         coef  = q0ms_ξ__to4
//val         coef1 = coef / psisq**3.5
//         cc2   = coef1 * satrec.no * (ao * (1.0 + 1.5 * etasq + eeta *
//                        (4.0 + etasq)) + 0.375 * j2 * tsi / psisq * satrec.con41 *
//                        (8.0 + 3.0 * etasq * (8.0 + etasq)));
 //         cc2   = coef1 * satrec.no * (ao * (1.0 + 1.5 * etasq + eeta *
  val coef1 = q0ms_ξ__to4 / (psisq** 3.5)
  val C2 : F = coef1 * n0*(a0 * (1 + 1.5*ηsq + e0η*(4 + ηsq)) + 0.375*J2*ξ / psisq * (3*θsq - 1) * (8 + 3 * ηsq * (8 + ηsq)))

// Vallado's form is with J2, not K2, so aE**2 is missing
//  val C2 = q0ms_ξ__to4 * n0 * (1 - ηsq)** -3.5 * 
//    (a0 * (1 + 3*ηsq/2 + e0η*(4 + ηsq)) + 
//        3*K2*ξ/(2*(1-ηsq)) * (3*θsq - 1)/2 * (8 + 24*ηsq + 3*ηto4)) 
  
  val C1 : F = bStar * C2
  val C1sq   = C1*C1
  
  // Vallado's 
   // cc3 = -2.0 * coef * tsi * j3oj2 * satrec.no * sinio / satrec.ecco;
  // uses -2*j3oj2 instead of A30 * aE / K2 which gives -2*J3*aE**2/J2 -> the aE*aE term is also missing 
  // 
  // val C3 =  if (e0 > 0.0001.as[F]) q0ms_ξ__to4 * ξ * A30 * n0 * aE * sini0 / (K2 * e0) else 0.as[F]
  val C3 =  if (e0 > 0.0001.as[F]) -2 * q0ms_ξ__to4 * ξ * (J3/J2) * n0 * sinio / e0  else 0.as[F]
  
//           satrec.cc4    = 2.0* satrec.no * coef1 * ao * omeosq *
//                           (satrec.eta * (2.0 + 0.5 * etasq) + satrec.ecco *
//                           (0.5 + 2.0 * etasq) - j2 * tsi / (ao * psisq) *
//                           (-3.0 * satrec.con41 * (1.0 - 2.0 * eeta + etasq *
//                           (1.5 - 0.5 * eeta)) + 0.75 * satrec.x1mth2 *
//                           (2.0 * etasq - eeta * (1.0 + etasq)) * cos(2.0 * satrec.argpo)));
//         satrec.cc5 = 2.0 * coef1 * ao * omeosq * (1.0 + 2.75 *
//                        (etasq + eeta) + eeta * etasq);
  val aterm = 3*(1-3*θsq)*(1 + 3*ηsq/2 - 2*e0η - e0η*ηsq/2) + 3*(1-θsq)*(2*ηsq - e0*η - e0η*ηsq)*cos(2*ω0)/4

//  val C4 = 2*n0*a0*β0sq*coef1* ((2*η*(1+e0*η) + (e0 + ηto3)/2) - 2*K2*ξ*aterm/(a0*(1-ηsq)))
//val C4    = 2*n0* coef1 * a0 * β0sq *
//                           (η * (2 + 0.5 * ηsq) + e0 *
//                           (0.5 + 2.0 * ηsq) - J2 * ξ / (a0 * psisq) *
//                           (-3 * con41 * (1 - 2 * e0η + ηsq *
//                           (1.5 - 0.5 * e0η)) + 0.75 * x1mth2 *
//                           (2 * ηsq - e0η * (1 + ηsq)) * cos(2*ω0)))  
  val C4 = 2*a0*β0sq*coef1*n0*((2*η*(1+e0η) + (e0 + ηto3)/2) - J2*ξ*aterm/(a0*psisq))
  val C5 = 2*a0*β0sq*coef1*(1 + 11*(ηsq+e0η)/4 + e0η*ηsq)
   
  val D2 = 4*a0*C1*C1*ξ
  val D3 = D2*(17*a0+s)*C1*ξ/3
  val D4 = D2*D2*ξ*(221*a0+31*s)/24
}

case class ILCoefs[F: Field](cf : CoefFunctions[F]) {
   // coeficients for IL
  import cf._
   val t2cof = 3*C1/2
   val t3cof = D2 + 2*C1sq
   val t4cof = (3*D3 + C1*(12*D2 + 10 * C1sq))/4
   val t5cof = (3*D4 + 12*C1*D3 + 6*D2*D2 + 15*C1sq*(2*D2+C1sq))/5
}
*/

//object SGP4TIF {
  
//  def apply[F: Field: NRoot : Order: Trig](ini : TEME.SGPElems[F])(implicit wgs: SGPConstants[F]) : SGP4TIF[F] = {
//    import ini._
//    val i0f  = InclFunctions(i0)
//    val e0f  = EccentricityFunctions(e0)
//    val bmmf = BrowerMeanMotion(n0,i0f,e0f)
//    val sf   = ScalcFunctions(e0f,bmmf)
//    val coeff = CoefFunctions(ω0, bStar,i0f,e0f,bmmf, sf)
//    val ilf  = ILCoefs(coeff) 
//    val ocf  = OtherCoefs(ini, i0f,e0f,bmmf,sf,coeff) 
//   
//  new SGP4TIF() // ini,i0f,e0f,bmmf,sf,coeff,ilf,ocf)
//  }
//}
