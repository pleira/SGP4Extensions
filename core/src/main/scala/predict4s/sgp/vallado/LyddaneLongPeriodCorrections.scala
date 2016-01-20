package predict4s.sgp.vallado

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord._


trait LyddaneLongPeriodCorrections[F] extends TwoTermsKeplerEq {
   
  val wgs: SGPConstants[F]
  val ictx : InclinationCtx[F]
  
  def lppCorrections(secularElemt : SGPElems[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]) 
      : (SpecialPolarNodal[F], LongPeriodContext[F]) = {
    // long period corrections in Lyddane's coordinates
    val lylppState = lylppCorrections(secularElemt)
    // To transform to Special Polar Nodals, get the eccentric anomaly
    val eaState = solveKeplerEq(lylppState)
    LyddaneConversions.lyddane2SpecialPolarNodal(eaState, lylppState)
  }
  
  def lylppCorrections(secularElemt : SGPElems[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]) : LyddaneElems[F] = {
    import secularElemt._,wgs.`J3/J2`,ictx.{c,s}
    
    // Brouwer long-period gravitational corrections are reformulated in Lyddane’s (F,S,C,a,h,I).
    // At the precision of SGP4, there are only corrections for F and S.
    
    // C´´ = e´´ * cos(g´´), there is no long period correction for Lyddane's C term, which is defined as e*cosω
    val ecosω = e * cos(ω)  // axnl
    val p = a * (1 - e * e)   // 1/p´´ = 1/ (a´´ * (1 − e´´²))
    // Lyddane S' = e*sinω + (-`J3/J2`*sinI0/2)/p´´ =  e*sinω - `J3/J2`/p/2 * s = e*sinω - ϵ3*s
    val ϵ3 = `J3/J2`/p/2 

    // sgp4fix for divide by zero with I = 180 deg
    val `1+c` : F = 
      if (abs(1+c) > 1.5e-12.as[F]) (1 + c) else 1.5e-12.as[F]
    
    val aynl : F = e * sin(ω) - ϵ3 * s
    
    // Lyddane F' = (M´´ + g´´ + h´´) +  1/p´´*(-`J3/J2`*sinI0*(3 + 5*cosI0)/(1 + cosI0)/4)*e*cosω
    val xl : F = M + ω + Ω - ϵ3 * s * (3 + 5*c) * ecosω / `1+c` / 2
    
    // no more corrections, that is, L’= L",  I’= I", h’= h"
    
    LyddaneElems(I, a, Ω, ecosω, aynl, xl)
  }
  
}