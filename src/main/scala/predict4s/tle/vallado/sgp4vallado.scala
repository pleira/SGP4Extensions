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
   
  type FinalState = SpecialPolarNodal[F]
  type ShortPeriodState = (SpecialPolarNodal[F], SpecialPolarNodal[F]) // final values, corrections ShortPeriodPolarNodalContext
  type LongPeriodState = (SpecialPolarNodal[F], F, F, F, F, F) // final values, context variables
  type EccentricAState = EccentricAnomalyState

 
  case class LyddaneLongPeriodPeriodicState(axnl: F, aynl: F, xl: F) {
    def `C´´` = axnl; def `S´´` = aynl ; def `F´´` = xl
  }
  
  override def periodicCorrections(secularElemt : SGPElems[F])
      :  (FinalState, ShortPeriodState, LongPeriodState, EccentricAState) = {
    val lppState = lppCorrections(secularElemt, sec.dragCoefs)
    val eaState = solveKeplerEq(secularElemt, lppState)
    val lppPolarNodalContext = lydanne2SpecialPolarNodal(eaState, lppState, secularElemt)
    val sppPolarNodalContext = sppCorrections(sec.ctx0, lppPolarNodalContext, secularElemt)
    val (finalPNState, _) = sppPolarNodalContext
    (finalPNState, sppPolarNodalContext, lppPolarNodalContext, eaState)
  }

  override def propagate2CartesianContext(t: Minutes) = {
    val ((finalPolarNodal, sppState, lppState, eaState), secularElemt) = propagate2PolarNodalContext(t)
    import finalPolarNodal._
    val uPV: CartesianElems[F] = polarNodal2UnitCartesian(I, su, Ω)
    val (p, v) = convertAndScale2UnitVectors(uPV.pos, uPV.vel, mrt, mvt, rvdot)
    val posVel = CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
    (posVel, uPV, finalPolarNodal, sppState, lppState, eaState)    
  }

  /**
   * Solve Kepler's equation expressed in Lyddane's variables 
   * 		U = Ψ + S'cosΨ − C'sinΨ
   * where U = F' − h' to compute the anomaly Ψ = E'+g', where E is the eccentric anomaly.
   * The Newton-Raphson iterations start from Ψ0 = U.
   */
  def solveKeplerEq(elem : SGPElems[F], lppState: LyddaneLongPeriodPeriodicState): EccentricAnomalyState = {
       
    import elem.{e,Ω,ω,M,a}, sec.wgs.twopi, lppState._
         
    // Nodep (or Ω) is the ascending node, E is the eccentric anomaly, and e is the eccentricity.
    var ktr : Int = 1
    
    // U = F' - h' = M" + g"; 
    val u = Field[F].mod(xl - Ω, twopi.as[F])  
    
    /*
     *  Ψ = E"+g"
     *  F’-h" = (E"+g") + S’*cos(E"+g") - C’*sin(E"+g")
     *  U = Ψ + S’*cosΨ - C’*sinΨ
     *  fun(Ψ) = U - Ψ - S’*cosΨ + C’*sinΨ = 0
     *  fun(Ψ0 + (Ψ-Ψ0)) = fun(Ψ0)+(Ψ-Ψ0)*dfu(Ψ0) = 0
     *                 fdot = D[fun,Ψ] = -1 + S’*sinΨ + C’*cosΨ
     *  Ψ1 = Ψ0 - fun(Ψ0)/dfu(Ψ0)
     *  Ψ(n+1) = Ψn - fun(Ψn)/dfu(Ψn)
     */
    var E  = u   
    var tem5 : F = 9999.9.as[F]     //   sgp4fix for kepler iteration
    var ecosE : F = 0.as[F]
    var esinE : F = 0.as[F]
    var cosE : F = 0.as[F]
    var sinE : F = 0.as[F]
     
    //   the following iteration needs better limits on corrections
    while ((abs(tem5) >= 1e-12.as[F]) && (ktr <= 10)) {
      sinE = sin(E)
      cosE = cos(E)
      ecosE = axnl * cosE + aynl * sinE
      esinE = axnl * sinE - aynl * cosE
      
      // (in reality) -fdot = 1 - (S’*sinΨ + C’*cosΨ)
      val fdot = 1 - ecosE
      // f = U + (C’*sinΨ - S’*cosΨ) - Ψ0  
      val f = (u + esinE - E)
      tem5 = f / fdot  // delta value
      if(abs(tem5) >= 0.95.as[F]) {
          tem5 = if (tem5 > 0.as[F]) 0.95.as[F]  else -0.95.as[F]
      }
      // Ψ1 = Ψ0 - delta 
      E = E + tem5
      ktr = ktr + 1
    }
    EccentricAnomalyState(E,cosE,sinE,ecosE,esinE)   
  }
  
  def lppCorrections(secularElem : SGPElems[F], secularDragCoefs: DragSecularCoefs[F]) : LyddaneLongPeriodPeriodicState = {
    import secularElem._
    import secularDragCoefs.{aycof,xlcof}
    
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
    
    LyddaneLongPeriodPeriodicState(axnl, aynl, xl)
  }
  
  def lydanne2SpecialPolarNodal(eaState: EccentricAnomalyState, lppState: LyddaneLongPeriodPeriodicState, secularElem: SGPElems[F]) = {
    import eaState._ 
    import lppState._
    import secularElem._ // {n,e,I,ω,Ω,M,a}

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
    val sinu   = a / rl * (sinE - aynl - axnl * temp0)
    val cosu   = a / rl * (cosE - axnl + aynl * temp0)
    val su0    = atan2(sinu, cosu)
    val sin2u  = 2 * cosu * sinu
    val cos2u  = 1 - 2 * sinu * sinu
    (SpecialPolarNodal(I, su0, Ω, rl, rdotl, rvdotl), `el²`, pl, βl, sin2u, cos2u) 
  }    

  def sppCorrections(ctx: Context0[F], lppState: LongPeriodState, secularElem: SGPElems[F]) 
      : ShortPeriodState = {
    import lppState.{_1 => lppPN,_3 => pl,_4 => βl,_5 => sin2u,_6 => cos2u}
    import lppPN._
    import secularElem.n 
    import sec.wgs.{J2,KE}
 
    val temp1  = J2 / pl / 2
    val temp2  = temp1 / pl 

    /* -------------- update for short period gravitational periodics ------------ */

    import ctx._
    val mrt = r * (1 - 1.5 * temp2 * βl * con41) + temp1 * x1mth2 * cos2u / 2
    val δsu = - temp2 * x7thm1 * sin2u / 4
    val sup = su + δsu
    val δΩ = 1.5 * temp2 * c * sin2u
    val node = Ω + δΩ
    val δI = 1.5 * temp2 * c * s * cos2u
    val inc = I + δI 
    val δrdot = - n * temp1 * x1mth2 * sin2u / KE
    val mvt = rdot0 + δrdot
    val δrvdot = n * temp1 * (x1mth2 * cos2u + 1.5 * con41) / KE  
    val rvdot = rvdot0 + δrvdot
    (SpecialPolarNodal(inc, sup, node, mrt, mvt, rvdot) , SpecialPolarNodal(δI, δsu, δΩ, 0.as[F], δrdot, δrvdot))
  }

}

object SGP4Vallado  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) :  SGP4Vallado[F] = new SGP4Vallado(sec)
  
}
