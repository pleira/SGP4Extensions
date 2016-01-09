package predict4s.coord

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._

case class AnomalyState[F](E : F, cosE: F, sinE: F, ecosE: F, esinE: F) {
  // define alternative methods to allow expressing a different Anomaly U (not the Eccentric Anomaly)
  def U = E; def cosU: F = cosE; def sinU: F = sinE; def ecosU: F = ecosE; def esinU: F = esinE;
}

case class AuxVariables[F](s: F, c: F, p: F, κ: F, σ: F, n: F, β: F, sin2f: F, cos2f: F)

object SGPElemsConversions {
 
  /**
   * Builds SGPElems with original mean motion (n0'', n0dp) and semimajor axis (a0'' , a0dp).
   * 
   */  
  def sgpElemsAndContext[F: Field: Trig: NRoot: Order](tle: TLE, wgs: SGPConstants[F]) 
      :  (SGPElems[F], Context0[F]) = { 
    val e0 = tle.eccentricity.toDouble.as[F]
    val i0 = tle.inclination.toDouble.toRadians.as[F]
    val pa = tle.argumentOfPeriapsis.toDouble.toRadians.as[F]
    val raan = tle.rightAscension.toDouble.toRadians.as[F]
    val meanAnomaly =  tle.meanAnomaly.toDouble.toRadians.as[F]
    val meanMotionPerDay = tle.meanMotion.toDouble.as[F]
    val year = tle.year
    val bStar = tle.atmosphericDragCoeficient.toDouble.as[F]
    // in julian days
    val epoch : F = {
      val mdhms = TimeUtils.days2mdhms(tle.year, tle.epoch.toDouble)
      (TimeUtils.jday(tle.year, mdhms._1, mdhms._2, mdhms._3, mdhms._4, mdhms._5) - 2433281.5).as[F]
    }
    val ω0 = pa
    val Ω0 = raan
    val M0 = meanAnomaly    
    val radPerMin0 = TimeUtils.revPerDay2RadPerMin(meanMotionPerDay)
    //val context0 = Context0(i0, e0)
    val elem0Ctx = calcContextAndOriginalMotionAndSemimajorAxis(e0,i0,ω0,Ω0,M0, bStar,epoch, radPerMin0, wgs)
    elem0Ctx
    // (SGPElems[F](n0,e0,i0,ω0,Ω0,M0,a0,bStar,epoch), context0)
  }

  private def calcContextAndOriginalMotionAndSemimajorAxis[F: Field: NRoot : Order: Trig](
      e: F,I: F,ω: F,Ω: F,M: F, bStar: F,epoch: F, radPerMin: F, wgs: SGPConstants[F]) 
    : (SGPElems[F], Context0[F]) = {
    val `e²` : F = e*e
    val s : F = sin(I)
    val c : F = cos(I)
    val `c²` : F = c*c
    val x3thm1 = 3*`c²` - 1
    val `β0²` = 1 - `e²`
    val β0 = `β0²`.sqrt
    val `β0³` = β0 * `β0²`
    val (n, a) = calcOriginalMotionAndSemimajorAxis(radPerMin,x3thm1,`β0³`,wgs)
    val context0 = Context0(a, `e²`,s,c,`c²`, x3thm1,β0,`β0²`,`β0³`, wgs)
    (SGPElems[F](n,e,I,ω,Ω,M,a,bStar,epoch), context0)
  }
  
  private def calcOriginalMotionAndSemimajorAxis[F: Field: NRoot : Order: Trig](n: F, x3cos2Im1: F, `β0³`: F, wgs: SGPConstants[F]) 
      : (F, F) = {
    import wgs.{KE,J2}
    
    val a1   = (KE / n) fpow (2.0/3.0).as[F]  // (Ke / n0) pow 1.5   
    val tval = 3 * J2 * x3cos2Im1 / `β0³` / 4  // 3 * k2 * (3*`cos²I0` - 1) / ((1-`e0²`) pow 1.5) / 4 
    val δ1   = tval / (a1*a1)
    val a0   = a1 * (1 - δ1 * (1.as[F]/3.as[F] + δ1 * (1 + 134 * δ1 / 81)))
    val δ0   = tval / (a0 * a0)  
    val n0dp = n   / (1 + δ0) 
    val a0dp = (KE / n0dp) fpow (2.as[F]/3.as[F])  // a0   / (1 - δ0)
    (n0dp, a0dp)
  }
   
  def sgpelems2SpecialPolarNodal[F: Field: Trig: NRoot: Order](eaState: AnomalyState[F], secularElem : SGPElems[F], wgs: SGPConstants[F]) = {
    import eaState._ 
    import secularElem.{a,I,e,n,ω,Ω}
    import wgs.twopi
    
    val `e²` = e*e
    val p = a*(1 - `e²`)  // semilatus rectum , as MU=1, p=Z²
    val β = sqrt(1 - `e²`) 
    if (p < 0.as[F]) throw new Exception("p: " + p)
    val `√p` = sqrt(p)  
    val `√a` = sqrt(a)

    val r = a * (1 - ecosE)        // r´        
    val R = sqrt(a) / r * esinE     // R´ = L/r *esinE, L=sqrt(nu*a), MU=1 in SGP4 variables, later applied
    val Θ = sqrt(a) * β            // MU=1 
    val numer = β * sinE
    val denom = (cosE - e)
    val sinf = a/r*numer
    val cosf = a/r*denom
    val esinf = e*sinf
    val ecosf = e*cosf
    val f = atan2(sinf, cosf)
    //val f = trueAnomaly(eaState, e)
    val θ0to2pi = f + ω 
    val θ = if (θ0to2pi > pi.as[F])  θ0to2pi - twopi else θ0to2pi 
    //val N = H
//    val av = AuxVariables(sin(I), cos(I), p, ecosf, esinf, n, β, sin2f, cos2f)
    val av = AuxVariables(sin(I), cos(I), p, ecosf, esinf, n, β, 0.as[F], 0.as[F])
    
    // do check
    {
    import av._
    assert(abs(κ - (av.p/r - 1)) < 1E-12.as[F] ) 
    assert(abs(σ - (av.p*R/Θ)) < 1E-12.as[F])
    }
    
    (SpecialPolarNodal(I,θ,Ω,r,R,Θ/r), av) 
  }
  
}
