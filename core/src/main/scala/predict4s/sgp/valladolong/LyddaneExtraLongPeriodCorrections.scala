package predict4s.sgp.valladolong

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.coord._
import predict4s.sgp.vallado.TwoTermsKeplerEq
import predict4s.coord.LyddaneElems

trait LyddaneExtraLongPeriodCorrections[T] extends TwoTermsKeplerEq {
   
  val wgs: SGPConstants[T]
  val ctx0: Context0[T]
  
  def lppCorrections(secularElemt : SGPElems[T])(implicit ev: Field[T], trig: Trig[T], or: Order[T], nr: NRoot[T]) 
      : (SpecialPolarNodal[T], LongPeriodContext[T]) = {
    // long period corrections in Lyddane's coordinates
    val lylppState = lylppCorrections(secularElemt)
    // To transform to Special Polar Nodals, get the eccentric anomaly
    val eaState = solveKeplerEq(lylppState)
    lyddane2SpecialPolarNodal(eaState, lylppState)
  }
  
  def lylppCorrections(secularElem : SGPElems[T])(implicit ev: Field[T], trig: Trig[T], or: Order[T], nr: NRoot[T]) : LyddaneElems[T] = {
    import secularElem._,wgs.`J3/J2`,ctx0.{c,s}
    
    val `η²` = (1 - e*e)
    val η = sqrt(`η²`)
    val p = a * `η²`          // p´´ = a´´*(1 − e´´²)
    val ϵ3 = `J3/J2`/p/2 

    val esinω = e * sin(ω)    // S´´ = e´´*sin(g´´) , g´´ = ω
    val ecosω = e * cos(ω)    // C´´ = e´´ * cos(g´´)
    val `ecosω²` = ecosω**2
    
    // Then, apply the long-period corrections to compute the "prime" variables

    // a similar fix as Vallado's fix for divide by zero with I = 180 deg 
    val Fcof = 
      if ((c+1) > 1.5e-12.as[T]) 
          (1+2*c)/(1+c) + `η²`/(1+η)
      else
          (1+2*c)/1.5e-12 + `η²`/(1+η)
          
    val Scof = (`η²` + 2*`ecosω²`)*s - `ecosω²`/s
    
    val F = M + ω + Ω - ϵ3 * ecosω * Fcof * s    // Lyddane F' = (M´´ + g´´ + h´´) +  ...
    val S = esinω - ϵ3*Scof
    val C = ecosω*(1 - ϵ3*esinω*(1/s - 2*s))
    val h = Ω - ϵ3*ecosω*c/s
    val Il = I +  ϵ3* esinω * c  
    
    //LyddaneLongPeriodPeriodicState(n, I, a, Ω, ecosω, aynl, F)
    LyddaneElems(n, Il, a, h, C, S, F)
  }
  
  def lyddane2SpecialPolarNodal(eaState: AnomalyState[T], lylppState: LyddaneElems[T]) 
      (implicit ev: Field[T], trig: Trig[T], or: Order[T], nr: NRoot[T]) 
      : (SpecialPolarNodal[T], LongPeriodContext[T]) = {
    import eaState._ 
    import lylppState._

    // It follows the usual transformation to polar-nodal variables
    // (r, θ, R, Θ) −→ (F, C, S, a)  with C' = e'cosg and  S' = e'sing
    // Note: Vallado's SGP4 uses rθdot = Θ/r instead of Θ
    // Note: the U here has the relation U = E + ω with E the eccentric anomaly
    
    val `el²` =  `C´`* `C´` + `S´`*`S´` 
    val pl = a*(1 - `el²`)  // semilatus rectum , as MU=1, p=Z²
    if (pl < 0.as[T]) throw new Exception("pl: " + pl)
      
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