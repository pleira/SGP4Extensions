package predict4s.sgp.algo

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import org.scalactic.Or
import org.scalactic.Good
import org.scalactic.Bad
import predict4s.coord._
import predict4s.sgp._

trait LyddaneExtraLongPeriodCorrections[F] extends LongPeriodCorrections[F] with TwoTermsKeplerEq {
  
  override def lppCorrections(secularElemt : SGPSecularCtx[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]) 
      : LPPSPNResult[F] = {
    val lyddaneElems = lylppCorrections(secularElemt)
    for {
      // long period corrections in Lyddane's coordinates
      // To transform to Special Polar Nodals, get the eccentric anomaly
      eaState <- solveKeplerEq(lyddaneElems)
      spnctx <- LyddaneConversions.lyddane2SpecialPolarNodal(eaState, lyddaneElems)
    } yield (spnctx._1, spnctx._2, secularElemt)
  }
  
  def lylppCorrections(secularElemt : SGPSecularCtx[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F])
      : LyddaneElems[F] = {
    
    import secularElemt.{_1 => elem,_3 => wgs,_2 => ictx}
    import ictx.{c,s},wgs.`J3/J2`,elem.{a,e,ω,M,I,Ω}

    // Brouwer long-period gravitational corrections are reformulated in Lyddane’s (F,S,C,a,h,I).
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
    
    // Lyddane elements including 2nd order corrections // FIXME: note the singularity for (sinI) s = 0 
    val s_ =
      if (s < 1.5e-12.as[F]) 1.5e-12.as[F] else s
      
    val Il = I + ϵ3*esinω*c      
    val h =  Ω - ϵ3*ecosω*c/s_
    val C = ecosω*(1 - ϵ3*esinω*(1/s_ - 2*s_))
    val S = esinω - ϵ3*((`η²` + 2*`ecosω²`)*s_ - `ecosω²`/s_)
    val F = M + ω + Ω - ϵ3 * s_ * ecosω * ((1+2*c)/`1+c` + `η²`/(1+η))    // Lyddane F' = (M´´ + g´´ + h´´) +  ...

    LyddaneElems(Il, a, h, C, S, F)
  }
  
}