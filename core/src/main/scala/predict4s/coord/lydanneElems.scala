package predict4s.coord

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
  
// n: mean motion, I: inclination, a: semimajor axis, Ω: ascending node argument
case class LyddaneElems[F: Field](I: F, a: F, Ω: F, C: F, S: F, F: F) {
  def xl = F; def `C´` = C; def `S´` = S ; def `F´` = xl; def h = Ω; def axnl = C; def ecosω = C; def aynl = S ; 
  def +(o: LyddaneElems[F]) = LyddaneElems(I + o.I, a - o.a,Ω + o.Ω,C + o.C,S + o.S, F + o.F)
  def -(o: LyddaneElems[F]) = LyddaneElems(I - o.I,a - o.a,Ω - o.Ω,C - o.C,S - o.S, F - o.F)  
}


case class LongPeriodContext[F](`el²`: F, pl: F, `√pl`: F, βl: F, sin2θ: F, cos2θ: F)

object LyddaneConversions {
  
  
  def lyddane2SpecialPolarNodal[F: Field: NRoot: Order: Trig](eaState: AnomalyState[F], lylppState: LyddaneElems[F]) 
      : (SpecialPolarNodal[F], LongPeriodContext[F]) = {
    import eaState._ 
    import lylppState._

    // It follows the usual transformation to polar-nodal variables
    // (r, θ, R, Θ) −→ (F, C, S, a)  with C' = e'cosg and  S' = e'sing
    // Note: Vallado's SGP4 uses rθdot = Θ/r instead of Θ
    // Note: the U here has the relation U = E + ω with E the eccentric anomaly
    
    val `el²` =  `C´`* `C´` + `S´`*`S´` 
    val pl = a*(1 - `el²`)  // semilatus rectum , as MU=1, p=Z²
    if (pl < 0.as[F]) throw new Exception("pl: " + pl)
    val `√pl`  = sqrt(pl)
    val rl     = a * (1 - ecosU)          // r´        
    val rdotl  = sqrt(a) * esinU/rl       // R´
    val rvdotl = `√pl` / rl            // Θ’/r’ that is Θ/r 
    val βl     = sqrt(1 - `el²`)          // y’
    val `esinU/(1+βl)` = esinU / (1 + βl)         
     
    // θ is the argument of the latitude measured from the ascending node
    val sinθ = a / rl * (sinU - `S´` - `C´` * `esinU/(1+βl)`)
    val cosθ = a / rl * (cosU - `C´` + `S´` * `esinU/(1+βl)`)
    val θ = atan2(sinθ, cosθ)
    val sin2θ = 2 * cosθ * sinθ
    val cos2θ = 1 - 2 * sinθ * sinθ

    (SpecialPolarNodal(I, θ, Ω, rl, rdotl, rvdotl), LongPeriodContext(`el²`, pl, `√pl`, βl, sin2θ, cos2θ)) 
  }    
  
}