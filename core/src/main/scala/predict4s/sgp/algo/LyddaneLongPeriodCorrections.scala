package predict4s.sgp.algo

import org.scalactic.Or
import org.scalactic.Good
import org.scalactic.Bad
import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord._

trait LyddaneLongPeriodCorrections[F] extends LongPeriodCorrections[F] with TwoTermsKeplerEq {
 
  override def lppCorrections(secularElemt : SGPSecularCtx[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]) 
      : LPPSPNResult[F] = {
    // long period corrections in Lyddane's coordinates
    val lyddaneElems = lylppCorrections(secularElemt)
    for {
      // To transform to Special Polar Nodals, get the eccentric anomaly
      eaState <- solveKeplerEq(lyddaneElems)
      spnctx <- LyddaneConversions.lyddane2SpecialPolarNodal(eaState, lyddaneElems)
    } yield (spnctx._1, spnctx._2, secularElemt)
  }
  
  def lylppCorrections(secularCtx : SGPSecularCtx[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F])
      : LyddaneElems[F] = {
    val result = lylppCorrectionsCtx(secularCtx)
    result._1
  }
        
  def lylppCorrectionsCtx(secularCtx : SGPSecularCtx[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F])
      : LyddaneResult[F] = {
    
    import secularCtx.{_1 => elem,_3 => wgs,_2 => ictx}
    import ictx.{c,s},wgs.`J3/J2`,elem.{a,e,ω,M,I,Ω}
    
    // Brouwer long-period gravitational corrections are reformulated in Lyddane’s (F,S,C,a,h,I).
    // At the precision of normal SGP4, there are only corrections for F and S.
    val `η²` = (1 - e*e)
    val η = sqrt(`η²`)
    val p = a * `η²`          // p´´ = a´´*(1 − e´´²)
    val ϵ3 = `J3/J2`/p/2 
    
    val esinω = e * sin(ω)  // S´´ = e´´*sin(g´´) , g´´ = ω
    val ecosω = e * cos(ω)   
 
    // Then, apply the long-period corrections to compute the "prime" variables

    // sgp4fix for divide by zero with I = 180 deg
    val `1+c` : F = 
      if (abs(1+c) > 1.5e-12.as[F]) (1 + c) else 1.5e-12.as[F]
    
    // C´´ = e´´ * cos(g´´) there is no long period correction for Lyddane's C term
    val C = ecosω
    // Lyddane S' = e*sinω + (-`J3/J2`*sinI0/2)/p´´ =  e*sinω - `J3/J2`/p/2 * s = e*sinω - ϵ3*s
    val S : F = esinω - ϵ3 * s
    
    // Lyddane F' = (M´´ + g´´ + h´´) +  1/p´´*(-`J3/J2`*sinI0*(3 + 5*cosI0)/(1 + cosI0)/4)*e*cosω
    val F : F = M + ω + Ω - ϵ3 * s * ecosω * (3 + 5*c) / `1+c` / 2
    
    // no more corrections, that is, L’= L",  I’= I", h’= h"
    
    (LyddaneElems(I, a, Ω, C, S, F), (ϵ3,η,esinω))
  }
  
}