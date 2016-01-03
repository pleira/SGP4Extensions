package predict4s.sgp.vallado

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord._

  
// n: mean motion, I: inclination, a: semimajor axis, Ω: ascending node argument
case class LyddaneLongPeriodPeriodicState[F](n: F, I: F, a: F, Ω: F, ecosω: F, aynl: F, xl: F) {
  def `C´´` = axnl; def `S´´` = aynl ; def `F´´` = xl; def axnl = ecosω
}

trait LyddaneLongPeriodCorrections[F] extends TwoTermsKeplerEq {
   
  // val dragCoefs : DragSecularCoefs[F]
  val xlcof: F
  val aycof: F
  // val delM0: F
  
  def lppCorrections(secularElemt : SGPElems[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]) 
      : (SpecialPolarNodal[F], LongPeriodContext[F]) = {
    // long period corrections in Lyddane's coordinates
    val lylppState = lylppCorrections(secularElemt)
    // To transform to Special Polar Nodals, get the eccentric anomaly
    val eaState = solveKeplerEq(lylppState)
    lyddane2SpecialPolarNodal(eaState, lylppState)
  }
  
  def lylppCorrections(secularElem : SGPElems[F])(implicit ev: Field[F], trig: Trig[F], nr: NRoot[F]) : LyddaneLongPeriodPeriodicState[F] = {
    import secularElem._
    
    // Brouwer long-period gravitational corrections are reformulated in Lyddane’s (F,S,C,a,h,I).
    // At the precision of SGP4, there are only corrections for F and S.
    
    // C´´ = e´´ * cos(g´´), there is no long period correction for Lyddane's C term, which is defined as e*cosω
    val ecosω = e * cos(ω)  // axnl
    val p = a * (1 - e * e)   // 1/p´´ = 1/ (a´´ * (1 − e´´²))
    // Lyddane S' = e*sinω + (-`J3/J2`*sinI0/2)/p´´ =  e*sinω - `J3/J2`/p/2 * s = e*sinω - ϵ3*s
    //val ϵ3 = `J3/J2`/p/2 
    
    val aynl : F = e * sin(ω) + aycof / p
    // val aynl = e*sin(ω) - ϵ3*s
    // val `esinω-ϵ3*s` = e*sin(ω) - ϵ3*s
    
    // Lyddane F' = (M´´ + g´´ + h´´) +  1/p´´*(-`J3/J2`*sinI0*(3 + 5*cosI0)/(1 + cosI0)/4)*e*cosω
    val xl: F = M + ω + Ω + xlcof * ecosω / p
    
    // no more corrections, that is, L’= L",  I’= I", h’= h"
    
    LyddaneLongPeriodPeriodicState(n, I, a, Ω, ecosω, aynl, xl)
  }
  
  def lyddane2SpecialPolarNodal(eaState: AnomalyState[F], lylppState: LyddaneLongPeriodPeriodicState[F]) 
      (implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]) 
      : (SpecialPolarNodal[F], LongPeriodContext[F]) = {
    import eaState._ 
    import lylppState._

    // It follows the usual transformation to polar-nodal variables
    // (r, θ, R, Θ) −→ (F, C, S, a)  with C' = e'cosg and  S' = e'sing
    // Note: Vallado's SGP4 uses rθdot = Θ/r instead of Θ
    // here the l probably means Lyddane, and the E is not the eccentric anomaly.
    // the relation is U = E + ω
    val `el²` = ecosω*ecosω + aynl*aynl
    val pl = a*(1 - `el²`)  // semilatus rectum , as MU=1, p=Z²
    if (pl < 0.as[F]) throw new Exception("pl: " + pl)
      
    val rl     = a * (1 - ecosE)          // r´        
    val rdotl  = sqrt(a) * esinE/rl       // R´
    val rvdotl = sqrt(pl) / rl            // Θ’/r’ that is Θ/r 
    val βl     = sqrt(1 - `el²`)          // y’
    val temp0  = esinE / (1 + βl)         
     
    // θ is the argument of the latitude measured from the ascending node
    val sinθ = a / rl * (sinE - aynl - ecosω * temp0)
    val cosθ = a / rl * (cosE - ecosω + aynl * temp0)
    val θ = atan2(sinθ, cosθ)
    val sin2θ = 2 * cosθ * sinθ
    val cos2θ = 1 - 2 * sinθ * sinθ

    (SpecialPolarNodal(I, θ, Ω, rl, rdotl, rvdotl), LongPeriodContext(`el²`, pl, βl, sin2θ, cos2θ, n)) 
  }    
  
}