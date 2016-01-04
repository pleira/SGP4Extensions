package predict4s.coord

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
  
// n: mean motion, I: inclination, a: semimajor axis, Ω: ascending node argument
case class LyddaneElems[F](n: F, I: F, a: F, Ω: F, ecosω: F, aynl: F, xl: F) {
  def `C´` = axnl; def `S´` = aynl ; def `F´` = xl; def axnl = ecosω
}


case class LongPeriodContext[F](`el²`: F, pl: F, βl: F, sin2θ: F, cos2θ: F, n: F)

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
      
    val rl     = a * (1 - ecosU)          // r´        
    val rdotl  = sqrt(a) * esinU/rl       // R´
    val rvdotl = sqrt(pl) / rl            // Θ’/r’ that is Θ/r 
    val βl     = sqrt(1 - `el²`)          // y’
    val temp0  = esinU / (1 + βl)         
     
    // θ is the argument of the latitude measured from the ascending node
    val sinθ = a / rl * (sinU - `S´` - `C´` * temp0)
    val cosθ = a / rl * (cosU - `C´` + `S´` * temp0)
    val θ = atan2(sinθ, cosθ)
    val sin2θ = 2 * cosθ * sinθ
    val cos2θ = 1 - 2 * sinθ * sinθ

    (SpecialPolarNodal(I, θ, Ω, rl, rdotl, rvdotl), LongPeriodContext(`el²`, pl, βl, sin2θ, cos2θ, n)) 
  }    
  
}