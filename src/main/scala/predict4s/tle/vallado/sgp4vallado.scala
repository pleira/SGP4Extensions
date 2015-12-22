package predict4s.tle.vallado

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._
import predict4s.tle.GeoPotentialCoefs
import predict4s.tle._
import predict4s.tle.LaneCoefs
import predict4s.coord.CartesianElems
import predict4s.coord.SpecialPolarNodal
import predict4s.coord.CoordTransformation._
    
class SGP4Vallado[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4(sec) {
  
  type ShortPeriodCorrections = SpecialPolarNodal[F]
  type FinalState = SpecialPolarNodal[F]
  type ShortPeriodState = (SpecialPolarNodal[F], ShortPeriodCorrections) // final values, corrections ShortPeriodPolarNodalContext
  type LongPeriodState = (SpecialPolarNodal[F], F, F, F, F, F, F) // final values, context variables
  type EccentricAState = EccentricAnomalyState[F]

  
  override def periodicCorrections(secularElemt : SGPElems[F])
      :  (FinalState, ShortPeriodState, LongPeriodState, EccentricAState) = {
    val lylppState = lylppCorrections(secularElemt)
    val eaState = solveKeplerEq(lylppState)
    val lppPNContext = lyddane2SpecialPolarNodal(eaState, lylppState)
    val sppPolarNodalContext = sppCorrections(lppPNContext)
    val (finalPNState, _) = sppPolarNodalContext
    (finalPNState, sppPolarNodalContext, lppPNContext, eaState)
  }
  
  override def propagate2CartesianContext(t: Minutes) = {
    val ((finalPolarNodal, sppState, lppState, eaState), secularElemt) = propagate2PolarNodalContext(t)
    import finalPolarNodal._
    val uPV: CartesianElems[F] = polarNodal2UnitCartesian(I, su, Ω)
    val (p, v) = convertAndScale2UnitVectors(uPV.pos, uPV.vel, mrt, mvt, rvdot)
    val posVel = CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
    (posVel, uPV, finalPolarNodal, sppState, lppState, eaState)    
  }
  
  // n: mean motion, I: inclination, a: semimajor axis, Ω: ascending node argument
  // the 
  case class LyddaneLongPeriodPeriodicState(n: F, I: F, a: F, Ω: F, axnl: F, aynl: F, xl: F) {
    def `C´´` = axnl; def `S´´` = aynl ; def `F´´` = xl
  }
 
  /**
   * Solve Kepler's equation expressed in Lyddane's variables 
   * 		U = Ψ + S'cosΨ − C'sinΨ
   * where U = F' − h' to compute the anomaly Ψ = E'+g', where E is the eccentric anomaly.
   * The Newton-Raphson iterations start from Ψ0 = U.
   */
  def solveKeplerEq(lylppState: LyddaneLongPeriodPeriodicState) : EccentricAnomalyState[F]  = {
    import sec.wgs.twopi, lylppState._   
    // U = F' - h' = M" + g"; 
    val u = Field[F].mod(xl - Ω, twopi.as[F])  

    def loop(E: F, remainingIters: Int) : EccentricAnomalyState[F] = {
      val sinE = sin(E)
      val cosE = cos(E)
      val ecosE = axnl * cosE + aynl * sinE
      val esinE = axnl * sinE - aynl * cosE
      val fdot = 1 - ecosE
      val f = (u + esinE - E)
      val tem : F = f / fdot  
      val incr =
        if (abs(tem) >= 0.95.as[F]) {
          if (tem > 0.as[F]) 0.95.as[F] else -0.95.as[F]
        } else tem
      val En = E+incr
      if (remainingIters <= 0 || abs(incr) < 1e-12.as[F]) {
        EccentricAnomalyState(En,cosE,sinE,ecosE,esinE)   
      } else {
        loop(En, remainingIters - 1)
      }
    }
    loop(u, 10)
  }
  
  def lylppCorrections(secularElem : SGPElems[F]) : LyddaneLongPeriodPeriodicState = {
    import secularElem._
    import sec.dragCoefs.{aycof,xlcof}
    
    // Brouwer long-period gravitational corrections are reformulated in Lyddane’s (F,S,C,a,h,I).
    // At the precision of SGP4, there are only corrections for F and S.
    
    // C´´ = e´´ * cos(g´´), there is no long period correction for Lyddane's C term, which is defined as e*cosω
    val axnl = e * cos(ω)
    val temp = 1 / (a * (1 - e * e))   // 1/p´´ = 1/ (a´´ * (1 − e´´²))
    
    // Lyddane S' = e*sinω + 1/p´´ * (- `J3/J2`*sinI0/2)     
    val aynl : F = e * sin(ω) + temp * aycof
    // Lyddane F' = (M´´ + g´´ + h´´) +  1/p´´*(-`J3/J2`*sinI0*(3 + 5*cosI0)/(1 + cosI0)/4)*e*cosω
    val xl: F = M + ω + Ω + temp * xlcof * axnl
    
    // no more corrections, that is, L’= L",  I’= I", h’= h"
    
    LyddaneLongPeriodPeriodicState(n, I, a, Ω, axnl, aynl, xl)
  }
  
  def lyddane2SpecialPolarNodal(eaState: EccentricAnomalyState[F], lylppState: LyddaneLongPeriodPeriodicState) = {
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
     
    // TBC: u is the argument of the latitude
    val sinu = a / rl * (sinE - aynl - axnl * temp0)
    val cosu = a / rl * (cosE - axnl + aynl * temp0)
    val u = atan2(sinu, cosu)
    val sin2u = 2 * cosu * sinu
    val cos2u = 1 - 2 * sinu * sinu
    (SpecialPolarNodal(I, u, Ω, rl, rdotl, rvdotl), `el²`, pl, βl, sin2u, cos2u, n) 
  }    

  def sppCorrections(lppState: LongPeriodState) : ShortPeriodState = {
    import lppState.{_1 => lppPN,_3 => pl,_4 => βl,_5 => sin2u,_6 => cos2u, _7 => n}
    import lppPN._
    import sec.wgs.{J2,KE}
    import sec.ctx0._
 
    val temp1  = J2 / pl / 2
    val temp2  = temp1 / pl 
    val δI = 1.5 * temp2 * c * s * cos2u
    val δsu = - temp2 * x7thm1 * sin2u / 4
    val δΩ = 1.5 * temp2 * c * sin2u
    val δr =  - r * 1.5 * temp2 * βl * con41 + temp1 * x1mth2 * cos2u / 2
    val δrdot = - n * temp1 * x1mth2 * sin2u / KE
    val δrvdot = n * temp1 * (x1mth2 * cos2u + 1.5 * con41) / KE 
    val δspp = SpecialPolarNodal(δI,δsu,δΩ,δr,δrdot,δrvdot)
    val finalPN = lppPN + δspp
    (finalPN, δspp)
  }

}

object SGP4Vallado  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) :  SGP4Vallado[F] = new SGP4Vallado(sec)
  
}
