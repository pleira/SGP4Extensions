package predict4s.sgp.vallado

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._
import predict4s.sgp.GeoPotentialCoefs
import predict4s.sgp._
import predict4s.sgp.LaneCoefs
import predict4s.coord.CartesianElems
import predict4s.coord.SpecialPolarNodal
import predict4s.coord.CoordTransformation._
import predict4s.coord.SGPElems

  
// n: mean motion, I: inclination, a: semimajor axis, Ω: ascending node argument
// the 
case class LyddaneLongPeriodPeriodicState[F](n: F, I: F, a: F, Ω: F, axnl: F, aynl: F, xl: F) {
  def `C´´` = axnl; def `S´´` = aynl ; def `F´´` = xl
}

class SGP4Vallado[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4(sec) with TwoTermsKeplerEq {
  
  type ShortPeriodCorrections = SpecialPolarNodal[F]
  type ShortPeriodState = (SpecialPolarNodal[F], ShortPeriodCorrections) // final values, corrections ShortPeriodPolarNodalContext
  type LongPeriodState = (SpecialPolarNodal[F], F, F, F, F, F, F) // final values, context variables
  type EccentricAState = EccentricAnomalyState[F]

  
  override def periodicCorrections(secularElemt : SGPElems[F])
      :  (FinalState, ShortPeriodState, LongPeriodState, EccentricAState) = {
    val lylppState = lylppCorrections(secularElemt)
    val eaState = solveKeplerEq(lylppState)
    val lppPNContext = lyddane2SpecialPolarNodal(eaState, lylppState)
    val sppPolarNodalContext = sppCorrections(lppPNContext)
    val finalPNState = sppPolarNodalContext._1
    (finalPNState, sppPolarNodalContext, lppPNContext, eaState)
  }
  
  def lylppCorrections(secularElem : SGPElems[F]) : LyddaneLongPeriodPeriodicState[F] = {
    import secularElem._
    import sec.dragCoefs.{aycof,xlcof}
    
    // Brouwer long-period gravitational corrections are reformulated in Lyddane’s (F,S,C,a,h,I).
    // At the precision of SGP4, there are only corrections for F and S.
    
    // C´´ = e´´ * cos(g´´), there is no long period correction for Lyddane's C term, which is defined as e*cosω
    val axnl = e * cos(ω)
    val p = a * (1 - e * e)   // 1/p´´ = 1/ (a´´ * (1 − e´´²))
    
    // Lyddane S' = e*sinω + (-`J3/J2`*sinI0/2)/p´´ =  e*sinω - `J3/J2`/p/2 * s = e*sinω - ϵ3*s
    val aynl : F = e * sin(ω) + aycof / p
    // Lyddane F' = (M´´ + g´´ + h´´) +  1/p´´*(-`J3/J2`*sinI0*(3 + 5*cosI0)/(1 + cosI0)/4)*e*cosω
    val xl: F = M + ω + Ω + xlcof * axnl / p
    
    // no more corrections, that is, L’= L",  I’= I", h’= h"
    
    LyddaneLongPeriodPeriodicState(n, I, a, Ω, axnl, aynl, xl)
  }
  
  def lyddane2SpecialPolarNodal(eaState: EccentricAnomalyState[F], lylppState: LyddaneLongPeriodPeriodicState[F]) = {
    import eaState._ 
    import lylppState._

    // It follows the usual transformation to polar-nodal variables
    // (r, θ, R, Θ) −→ (F, C, S, a)  with C' = e'cosg and  S' = e'sing
    // Note: Vallado's SGP4 uses rθdot = Θ/r instead of Θ
    // here the l probably means long period, not Lyddane
    val `el²` = axnl*axnl + aynl*aynl
    val pl = a*(1 - `el²`)  // semilatus rectum , as MU=1, p=Z²
    if (pl < 0.as[F]) throw new Exception("pl: " + pl)
      
    val rl     = a * (1 - ecosE)          // r´        
    val rdotl  = sqrt(a) * esinE/rl       // R´
    val rvdotl = sqrt(pl) / rl            // Θ’/r’ that is Θ/r 
    val βl     = sqrt(1 - `el²`)          // y’
    val temp0  = esinE / (1 + βl)         
     
    // θ is the argument of the latitude measured from the ascending node
    val sinθ = a / rl * (sinE - aynl - axnl * temp0)
    val cosθ = a / rl * (cosE - axnl + aynl * temp0)
    val θ = atan2(sinθ, cosθ)
    val sin2θ = 2 * cosθ * sinθ // FIXME
    val cos2θ = 1 - 2 * sinθ * sinθ
    (SpecialPolarNodal(I, θ, Ω, rl, rdotl, rvdotl), `el²`, pl, βl, sin2θ, cos2θ, n) 
  }    

  def sppCorrections(lppState: LongPeriodState) : ShortPeriodState = {
    import lppState.{_1 => lppPN,_3 => pl,_4 => βl,_5 => sin2θ,_6 => cos2θ, _7 => n}
    import lppPN._
    import sec.wgs.{J2,KE}
    import sec.ctx0.{c,s,`7c²-1`,`1-c²`,`3c²-1`}
 
    val `J2/p/2` = J2 / pl / 2
    val ϵ2 = - `J2/p/2` / pl / 2
    val δI = - 3 * ϵ2 * c * s * cos2θ
    val δθ =       ϵ2 * `7c²-1` * sin2θ / 2
    val δΩ = - 3 * ϵ2 * c * sin2θ
    val δr =   3 * ϵ2 * r * βl * `3c²-1` + `J2/p/2` * `1-c²` * cos2θ / 2
    val δR = - n * `J2/p/2` * `1-c²` * sin2θ / KE  // rdot, angular velocity
    val δrvdot = n * `J2/p/2` * (`1-c²` * cos2θ + 1.5 * `3c²-1`) / KE 
    val δspp = SpecialPolarNodal(δI,δθ,δΩ,δr,δR,δrvdot)
    val finalPN = lppPN + δspp
    (finalPN, δspp)
  }

}

object SGP4Vallado  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) :  SGP4Vallado[F] = new SGP4Vallado(sec)
  
}
