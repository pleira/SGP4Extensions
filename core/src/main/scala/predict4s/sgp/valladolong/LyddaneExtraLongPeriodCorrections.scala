package predict4s.sgp.valladolong

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.coord._
import predict4s.sgp.vallado.TwoTermsKeplerEq
import predict4s.coord.LyddaneElems

/*
trait LyddaneExtraLongPeriodCorrections[T] extends TwoTermsKeplerEq {
   
  val wgs: SGPConstants[T]
  val ictx: InclinationCtx[T]
  
  def lppCorrections(secularElemt : SGPElems[T])(implicit ev: Field[T], trig: Trig[T], or: Order[T], nr: NRoot[T]) 
      : (SpecialPolarNodal[T], LongPeriodContext[T]) = {
    // long period corrections in Lyddane's coordinates
    val lylppState = lylppCorrections(secularElemt)
    // To transform to Special Polar Nodals, get the eccentric anomaly
    val eaState = solveKeplerEq(lylppState)
    LyddaneConversions.lyddane2SpecialPolarNodal(eaState, lylppState)
  }
  
  def lylppCorrections(secularElem : SGPElems[T])(implicit ev: Field[T], trig: Trig[T], or: Order[T], nr: NRoot[T]) : LyddaneElems[T] = {
    import secularElem._,wgs.`J3/J2`,ictx.{c,s}
    
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
    LyddaneElems(Il, a, h, C, S, F)
  }
  
}
*/