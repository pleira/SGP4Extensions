package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._

import spire.syntax.primitives._
/*
case class SGP4TimeIndepFunctions[F: Field: NRoot : Order: Trig] (ini : TEME.SGPElems[F])(implicit wgs: WGSConstants[F]) {
  import ini._
  import wgs._
  
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
    val e0sq       = e0*e0
    val β0sq       = 1-e0sq
    lazy val β0    = β0sq.sqrt
    lazy val β0to3 = β0sq * β0
    val β0to4      = β0sq * β0sq
    def rteosq     = β0sq
  
    // recovery of the Brouwer mean motion from the Kozai mean motion
  // Hoots a1
  val a1  = (KE / M0) fpow (2.0/3.0).as[F] // KE here contains Earth radius to 3/2
  // val a1 = aE * (MU/radpm0) fpow (TWO_THIRD)   // but in Hoots, KE is just sqrt(G*Mass_earth) * (E_radius/60) fpow (3/2)
  
  // 3 theta2 minus 1
  //val x3thm1 = 3 * theta2 - 1
    
    
  // Hoots δ0, δ1, original mean motion n0'' (double prime), original a0'' , semimajor axis a0 double prime
  // val tval_ = 3 * K2 * x3thm1 / β0to3 / 2
  val tval = 3 * J2 * x3thm1 / β0to3 / 4 // what happens to aE*aE? is the formula in page 10 of Spacetrack #3 correct?
  val δ1   = tval/(a1 * a1) 
  val a2   = a1 * (1 - δ1 * (1.0/3.0 + δ1 * (1 + 134 * δ1 / 81)))    //def a0 = a2
  val δ0   = tval/(a2 * a2)  
  val n0dp = M0 /(1 + δ0) 
  val a0dp = (KE / n0dp) fpow (2.0/3.0).as[F]
  
  // other derived coeficients and variables that can be used
  val Mcof  = if (e0 > 0.0001.as[F]) - 2*q0ms_ξ__to4 * bStar / e0η / 3 else 0.as[F]
  def xmcof  = Mcof
  val ωcof   = bStar*C3*cos(ω0)
  val pinvsq = 1 / (a0sq*β0to4)
  val temp1  = 3 * J2/2 * pinvsq * n0
  val temp2  = temp1 * J2 * pinvsq / 2
  val temp3  = -0.46875 * J4 * pinvsq * pinvsq * n0
  val xhdot1 = -temp1 * θ
  val Ωcof   = 7 * β0sq * xhdot1 * C1 / 2
  def nodecf = Ωcof
  val delM0   = (1+η*cos(M0))**3
  // sgp4fix for divide by zero with inco = 180 deg, 
  val xlcof =  if (abs(θ+1) > 1.5e-12.as[F]) - J3/J2 * sinio * (3 + 5*θ) / (1 + θ) / 4
               else                          - J3/J2 * sinio * (3 + 5*θ) / 1.5e-12 / 4
   val aycof   = - J3/J2 * sinio / 2
   val sinM0  = sin(M0)
   def sinmao  = sinM0
   val x7thm1  = 7*θsq - 1
//   val twopi : F = 2.as[F]*pi
  
  def originalMeanMotionAndSemimajorAxis() = (n0dp, a0dp)
   val (_Mdot, ωdot, omegadot) : (F,F,F) = if (n0 >= 0.as[F] || β0sq >= 0.as[F])
      ( 
          n0 + temp1 * β0sq * con41 / 2 + temp2 * β0sq * (13 - 78*θsq + 137*θto4) / 16, 
      - temp1 * con42 /2 + temp2*(7 - 114*θsq + 395*θto4)/16 + temp3*(3 - 36*θsq + 49*θto4),
      xhdot1 + (temp2 * (4 - 19*θsq)/2 + 2*temp3 * (3 - 7*θsq))*θ
      )
     else (0.as[F],0.as[F],0.as[F])
     
   def Ṁ = _Mdot
      // TEME.ElemTimeDerivative(0,0,0, ωdot, Ωdot, Ṁ)
 //     TEME.OrbitalElements[F](0.as[F],0.as[F],0.as[F], ωdot, Ωdot, Mdot)
 // }
  
  def Ωdot = omegadot
  def mdot = _Mdot
  
  
  val a0    = a0dp
  val a0sq  = a0*a0
  lazy val a0to4 = a0sq*a0sq
  val n0    = n0dp
  
  val po    = a0*β0sq
  val posq  = po*po

  // radius of perigee (note no aE term present in Vallado's)
  val rp      = a0*(1-e0) 
  
  // perigee height, altitude relative to the earth's surface, so perige instead of perigee 
  val perige = (rp - 1)*aE  
  
  // The parameter q0 is a constant equal to 120 km plus one Earth radius 
  val q0   = 1 + 120/aE 
  val s    = calculateS
  val q0ms_to4 = (q0 - s)**4 
    
  val ξ    = 1/(a0 - s)  // tsi
  val ξsq  = ξ*ξ
  lazy val ξto3 = ξsq*ξ
  val ξto4 = ξsq*ξsq
  val ξto5 = ξto4*ξ

  val η    = a0*e0*ξ   // eta
  val ηsq  = η*η       // etasq
  lazy val ηto3 = ηsq*η
  val ηto4 = ηsq*ηsq
  val e0η   = e0*η      // eeta 
  val psisq = abs[F](1-ηsq)  // Vallado's uses fabs
  // q0 minus s ξ  all to 4 
  val q0ms_ξ__to4 = q0ms_to4*(ξ**4)

 
  //val coef1  = coef / (psisq pow 3.5)
  def S_above156       =  (1 + 78/aE)
  def hs               =  rp - aE - 78   // interpolation, being a number bigger than 20, and smaller that 78
  def S_between_98_156 =  (1 + hs/aE)
  def S_below98        =  (1 + 20/aE)

  /* the parameter s is determined based of epoch perigee
   * height above a spherical Earth. If perigee height is greater than or equal 156 km, the value of s is
   * fixed to be 78 km plus one Earth radius. For altitudes greater than or equal to 98 km but less
   * than 156 km, s is defined to be perigee height minus 78 km plus one Earth radius. 
   * For altitudes below 98 km, s is 20 km plus one Earth radius.
   */
  private def calculateS : F = 
     if (perige >= 156)       S_above156
     else if (perige >= 98)   S_between_98_156
     else                     S_below98  
     
  def isImpacting : Boolean = rp < (220/aE + 1)
  val coef1 = q0ms_ξ__to4 / (psisq** 3.5)
  val C2 : F = coef1 * n0*(a0 * (1 + 1.5*ηsq + e0η*(4 + ηsq)) + 0.375*J2*ξ / psisq * (3*θsq - 1) * (8 + 3 * ηsq * (8 + ηsq))) 
  val C1 : F = bStar * C2
  val C1sq  = C1*C1
  val C3 =  if (e0 > 0.0001.as[F]) -2 * q0ms_ξ__to4 * ξ * (J3/J2) * n0 * sinio / e0  else 0.as[F]
  val aterm = 3*(1-3*θsq)*(1 + 3*ηsq/2 - 2*e0η - e0η*ηsq/2) + 3*(1-θsq)*(2*ηsq - e0*η - e0η*ηsq)*cos(2*ω0)/4
  val C4 = 2*a0*β0sq*coef1*n0*((2*η*(1+e0η) + (e0 + ηto3)/2) - J2*ξ*aterm/(a0*psisq))
  val C5 = 2*a0*β0sq*coef1*(1 + 11*(ηsq+e0η)/4 + e0η*ηsq)
   
  val D2 = 4*a0*C1*C1*ξ
  val D3 = D2*(17*a0+s)*C1*ξ/3
  val D4 = D2*D2*ξ*(221*a0+31*s)/24
   val t2cof = 3*C1/2
   val t3cof = D2 + 2*C1sq
   val t4cof = (3*D3 + C1*(12*D2 + 10 * C1sq))/4
   val t5cof = (3*D4 + 12*C1*D3 + 6*D2*D2 + 15*C1sq*(2*D2+C1sq))/5
}

//object SGP4TimeIndepFunctions {
//  
//  def apply[F: Field: NRoot : Order: Trig](ini : TEME.SGPElems[F])(implicit wgs: WGSConstants[F]) : SGP4TimeIndepFunctions[F] = {
//   
//   SGP4TimeIndepFunctions(ini)
//  }
//}
*/