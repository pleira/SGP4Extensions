package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._

case class OtherCoefs[F : Field: NRoot : Order: Trig](elem: TEME.SGPElems[F], ctx0: Context0[F], gcof : GeoPotentialCoefs[F], gctx : GeoPotentialContext[F]) {
 
  import gcof._,gctx._,ctx0._
  import ctx0.wgs._
  import elem.{e => e0,n => n0,a => a0,ω => ω0, M => M0,_}
  
  val gsto : F = predict4s.tle.gstime(epoch + 2433281.5) 
  
  val po   : F   = a0*`β0²`
  val posq : F   = po*po
  
  // other derived coeficients and variables that can be used
  val Mcof : F   = if (e0 > 0.0001.as[F]) - 2*q0ms_ξ__to4 * bStar / e0η / 3 else 0.as[F]
  def xmcof : F   = Mcof
  val ωcof  : F   = bStar*C3*cos(ω0)
  val pinvsq : F  = 1 / posq
  val temp1  : F  = 3 * J2 * pinvsq * n0 / 2
  val temp2 : F   = temp1 * J2 * pinvsq / 2
  val temp3  : F  = -0.46875 * J4 * pinvsq * pinvsq * n0
  val xhdot1 : F = - temp1*θ 
  val Ωcof : F    = 7 * `β0²` * xhdot1 * C1 / 2
  def nodecf  : F = Ωcof
  val delM0  : F   = (1+η*cos(M0))**3
  // sgp4fix for divide by zero with inco = 180 deg, // FIXME: not valid for deep space
  val xlcof  : F  =  if (abs(θ+1) > 1.5e-12.as[F]) - J3/J2 * sinio * (3 + 5*θ) / (1 + θ) / 4
               else                          - J3/J2 * sinio * (3 + 5*θ) / 1.5e-12 / 4
  val aycof  : F   = - J3/J2 * sinio / 2  // FIXME: not valid for deep space
  val sinM0  : F  = sin(M0)
  def sinmao  : F  = sinM0
  val x7thm1   : F = 7*θsq - 1
   
  val (_Mdot, ωdot, omegadot) : (F,F,F) = 
    if (n0 >= 0.as[F] || `β0²` >= 0.as[F])
      (// derivative of M 
        n0 + 0.5.as[F] * temp1 * β0 * con41 + 0.0625.as[F] * temp2 * β0 * (13 - 78 * `θ²` + 137 * `θ⁴`),
      // derivative of the perigee argument
      - temp1 * con42 /2 + temp2*(7 - 114*`θ²` + 395*`θ⁴`)/16 + temp3*(3 - 36*`θ²` + 49*`θ⁴`),
      // derivative of the raan
      xhdot1 + (temp2 * (4 - 19*`θ²`)/2 + 2*temp3 * (3 - 7*`θ²`))*θ
      )
    else (0.as[F],0.as[F],0.as[F])
     
  def Ṁ  : F = _Mdot  
  def Ωdot : F  = omegadot
  def mdot : F = _Mdot

}
