package predict4s.sgp.algo

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.coord._
import predict4s.coord.LyddaneElems

trait LyddaneExtraLongPeriodCorrections[F] extends LongPeriodCorrections[F] with TwoTermsKeplerEq {
   
  val wgs: SGPConstants[F]
  val ictx: InclinationCtx[F]
  
  override def lppCorrections(secularElemt : SGPElems[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]) 
      : (SpecialPolarNodal[F], LongPeriodContext[F]) = {
    // long period corrections in Lyddane's coordinates
    val lylppState = lylppCorrections(secularElemt)
    // To transform to Special Polar Nodals, get the eccentric anomaly
    val eaState = solveKeplerEq(lylppState)
    LyddaneConversions.lyddane2SpecialPolarNodal(eaState, lylppState)
  }
  
  def lylppCorrections(secularElem : SGPElems[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]) : LyddaneElems[F] = {
    import secularElem._,wgs.`J3/J2`,ictx.{c,s}
    
    val `η²` = (1 - e*e)
    val η = sqrt(`η²`)
    val p = a * `η²`          // p´´ = a´´*(1 − e´´²)
    val ϵ3 = `J3/J2`/p/2 

    val esinω = e * sin(ω)    // S´´ = e´´*sin(g´´) , g´´ = ω
    val ecosω = e * cos(ω)    // C´´ = e´´ * cos(g´´), here, we have corrections for C
    val `ecosω²` = ecosω**2
    
    // Then, apply the long-period corrections to compute the "prime" variables

    // a similar fix as Vallado's fix for divide by zero with I = 180 deg 
    // sgp4fix for divide by zero with I = 180 deg
    val `1+c` : F = 
      if (abs(1+c) > 1.5e-12.as[F]) (1 + c) else 1.5e-12.as[F]
    
    // Lyddane elements including 2nd order corrections // FIXME: note the singularity for s = 0 
    val Il = I + ϵ3*esinω*c      
    val h =  Ω - ϵ3*ecosω*c/s
    val C = ecosω*(1 - ϵ3*esinω*(1/s - 2*s))
    val S = esinω - ϵ3*((`η²` + 2*`ecosω²`)*s - `ecosω²`/s)
    val F = M + ω + Ω - ϵ3 * s * ecosω * ((1+2*c)/`1+c` + `η²`/(1+η))    // Lyddane F' = (M´´ + g´´ + h´´) +  ...

    LyddaneElems(Il, a, h, C, S, F)
  }
  
}