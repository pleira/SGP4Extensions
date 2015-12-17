package predict4s.tle.vallado

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._
import predict4s.tle.GeoPotentialCoefs
import predict4s.tle._
import TEME._   
import predict4s.tle.LaneCoefs

    
class SGP4Vallado[F : Field : NRoot : Order : Trig](
    elem0: SGPElems[F],
    wgs: SGPConstants[F],
    ctx0: Context0[F],
    geoPot: GeoPotentialCoefs[F],
    gctx: GeoPotentialContext[F],
    laneCoefs : LaneCoefs[F],
    secularTerms : (SecularFrequencies[F], DragSecularCoefs[F]),
    isImpacting: Boolean,
    rp: F
  ) extends SGP4(elem0, wgs, ctx0, geoPot, gctx, laneCoefs, secularTerms, isImpacting, rp) {
   
  type FinalState = SpecialPolarNodal
  type ShortPeriodState = (SpecialPolarNodal, SpecialPolarNodal) // final values, corrections ShortPeriodPolarNodalContext
  type LongPeriodState = (SpecialPolarNodal, F, F, F, F, F) // final values, context variables
  type EccentricAState = EccentricAnomalyState
  
  case class SpecialPolarNodal(I: F, su: F, Ω: F, mrt: F, mvt: F, rvdot: F) {
    def R = su; def su0 = su; def r = mrt; def rdot0 = mvt; def rvdot0 = rvdot;
  }
 
  case class LyddaneLongPeriodPeriodicState(axnl: F, aynl: F, xl: F) {
    def `C´´` = axnl; def `S´´` = aynl ; def `F´´` = xl
  }
  
  override def periodicCorrections(secularElemt : SGPElems[F], secularDragCoefs: DragSecularCoefs[F])
      :  (SpecialPolarNodal, ShortPeriodState, LongPeriodState, EccentricAState) = {
    val lppState = lppCorrections(secularElemt, secularTerms._2) // dragSecularCoefs
    val eaState = solveKeplerEq(secularElemt, lppState)
    val lppPolarNodalContext = lydanne2SpecialPolarNodal(eaState, lppState, secularElemt)
    val sppPolarNodalContext = sppCorrections(ctx0, eaState, lppPolarNodalContext, secularElemt)
    val (finalPNState, _) = sppPolarNodalContext
    (finalPNState, sppPolarNodalContext, lppPolarNodalContext, eaState)
  }

  override def propagate2CartesianContext(t: Minutes) = {
    val ((finalPolarNodal, sppState, lppState, eaState), secularElemt) = propagate2PolarNodalContext(t)
    import finalPolarNodal._
    val uPV: TEME.CartesianElems[F] = TEME.polarNodal2UnitCartesian(I, R, Ω)
    val (p, v) = convertAndScale2UnitVectors(uPV.pos, uPV.vel, mrt, mvt, rvdot)
    val posVel = TEME.CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
    (posVel, uPV, finalPolarNodal, sppState, lppState, eaState)    
  }

  /**
   * Solve Kepler's equation expressed in Lyddane's variables 
   * 		U = Ψ + S'cosΨ − C'sinΨ
   * where U = F' − h' to compute the anomaly Ψ = E'+g', where E is the eccentric anomaly.
   * The Newton-Raphson iterations start from Ψ0 = U.
   */
  def solveKeplerEq(elem : SGPElems[F], lppState: LyddaneLongPeriodPeriodicState): EccentricAnomalyState = {
       
    import elem.{e,Ω,ω,M,a}, wgs.twopi, lppState._
         
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
    var eo1  = u   
    var tem5 : F = 9999.9.as[F]     //   sgp4fix for kepler iteration
    var ecosE : F = 0.as[F]
    var esinE : F = 0.as[F]
    var coseo1 : F = 0.as[F]
    var sineo1 : F = 0.as[F]
     
    //   the following iteration needs better limits on corrections
    while ((abs(tem5) >= 1e-12.as[F]) && (ktr <= 10)) {
      sineo1 = sin(eo1)
      coseo1 = cos(eo1)
      ecosE = axnl * coseo1 + aynl * sineo1
      esinE = axnl * sineo1 - aynl * coseo1
      
      // (in reality) -fdot = 1 - (S’*sinΨ + C’*cosΨ)
      val fdot = 1 - ecosE
      // f = U + (C’*sinΨ - S’*cosΨ) - Ψ0  
      val f = (u + esinE - eo1)
      tem5 = f / fdot  // delta value
      if(abs(tem5) >= 0.95.as[F]) {
          tem5 = if (tem5 > 0.as[F]) 0.95.as[F]  else -0.95.as[F]
      }
      // Ψ1 = Ψ0 - delta 
      eo1 = eo1 + tem5
      ktr = ktr + 1
    }
    EccentricAnomalyState(eo1,coseo1,sineo1,ecosE,esinE)   
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
     
    // u is the true anomaly that can be defined immediately as the polar angle θ = (Ox, OS), x along the semimajor axis, S sat position
    val sinu   = a / rl * (sineo1 - aynl - axnl * temp0)             // sinu
    val cosu   = a / rl * (coseo1 - axnl + aynl * temp0)             // cosu
    val su0    = atan2(sinu, cosu)                                   // u, that is θ
    val sin2u  = 2 * cosu * sinu
    val cos2u  = 1 - 2 * sinu * sinu
    (SpecialPolarNodal(I, su0, Ω, rl, rdotl, rvdotl), `el²`, pl, βl, sin2u, cos2u) 
    // LongPeriodPolarNodalContext(rl, su0, rdotl, rvdotl, `el²`, pl, βl, sin2u, cos2u)
  }    

  def sppCorrections(ctx: Context0[F], eaState: EccentricAnomalyState, lppState: LongPeriodState, secularElem: SGPElems[F]) 
      : ShortPeriodState = { // PolarNodalElems[F]) = {
    import eaState._ 
    import lppState.{_1 => lpp,_3 => pl,_4 => βl,_5 => sin2u,_6 => cos2u}
    import lpp._
    import secularElem.n // {n,e,I,ω,Ω,M,a}
    import wgs.{J2,KE}
 
    val temp0  = esinE / (1 + βl)         
    val temp1  = J2 / pl / 2
    val temp2  = temp1 / pl 

    /* -------------- update for short period gravitational periodics ------------ */

    import secularTerms._
    import ctx._
    val mrt = r * (1 - 1.5 * temp2 * βl * con41) + temp1 * x1mth2 * cos2u / 2
    val δsu = - temp2 * x7thm1 * sin2u / 4
    val su = su0 + δsu
    val δΩ = 1.5 * temp2 * c * sin2u
    val node = Ω + δΩ
    val δI = 1.5 * temp2 * c * s * cos2u
    val inc = I + δI 
    val δrdot = - n * temp1 * x1mth2 * sin2u / KE
    val mvt = rdot0 + δrdot
    val δrvdot = n * temp1 * (x1mth2 * cos2u + 1.5 * con41) / KE  
    val rvdot = rvdot0 + δrvdot
    (SpecialPolarNodal(inc, su, node, mrt, mvt, rvdot) , SpecialPolarNodal(δI, δsu, δΩ, 0.as[F], δrdot, δrvdot))
  }

}

object SGP4Vallado extends SGP4Factory {
  
  def apply[F : Field : NRoot : Order : Trig](elem0Ctx0: (SGPElems[F], Context0[F]))(implicit wgs0: SGPConstants[F]) :  SGP4Vallado[F] = {
    val (elem, wgs, ctx0, geoPot, gctx, laneCoefs, secularFreqs, isImpacting, rp) = from(elem0Ctx0)
    new SGP4Vallado(elem, wgs, ctx0, geoPot, gctx, laneCoefs, secularFreqs, isImpacting, rp)
  }
  
}
