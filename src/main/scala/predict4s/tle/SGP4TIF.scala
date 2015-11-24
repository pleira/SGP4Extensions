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

