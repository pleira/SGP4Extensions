package predict4s.sgp.pn

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord.CartesianElems
import predict4s.coord.SpecialPolarNodal
import predict4s.coord.PolarNodalElems
import predict4s.coord.CoordTransformation._
import predict4s.coord.SGPElems

// compute the corrections in polar-nodal variables. 
class SGP4PN[F : Field : NRoot : Order : Trig](
 sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4(sec) with SimpleKeplerEq {
   
  type ShortPeriodCorrections = SpecialPolarNodal[F]
  type ShortPeriodState = (SpecialPolarNodal[F], ShortPeriodCorrections) // final values, corrections ShortPeriodPolarNodalContext
  type LongPeriodState = (SpecialPolarNodal[F], F, F, F, F, F, F) // final values, context variables
  type EccentricAState = EccentricAnomalyState[F]
 
  override def periodicCorrections(secularElemt : SGPElems[F])
      :  (FinalState, ShortPeriodState, LongPeriodState, EccentricAState) = {
    val eaState = solveKeplerEq(secularElemt)
    val pnSecularContext = sgpelems2SpecialPolarNodal(eaState, secularElemt)
    val lppSPNContext = lppCorrections(pnSecularContext, secularElemt)
    val sppPolarNodalContext = sppCorrections(lppSPNContext)
    val finalPNState = sppPolarNodalContext._1
    (finalPNState, sppPolarNodalContext, lppSPNContext, eaState)
  } 
   
  def sgpelems2SpecialPolarNodal(eaState: EccentricAnomalyState[F], secularElem : SGPElems[F]) = {
    import eaState._ 
    import secularElem.{a,I,e,n,ω,Ω}
    import sec.wgs.twopi
    
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
  
   
  def lppCorrections(pn: (SpecialPolarNodal[F], AuxVariables[F]), secElemt: SGPElems[F]) : (SpecialPolarNodal[F], F, F, F, F, F, F) = {
    import pn._1._,pn._2.{p,s,c},sec.wgs.`J3/J2`, secElemt.n,sec.wgs.twopi
    //(s: F, c: F, p: F, κ: F, σ: F, n: F, β: F, sin2f: F, cos2f: F)
    val ϵ3 = `J3/J2`/p/2
    val σ = p*R/Θ
    val κ = p/r - 1
    //val nθ = if (θ > 0) θ else (θ + pi.as[F]) 
    val sinθ = sin(θ)
    val cosθ = cos(θ)
    val δr = ϵ3 * p * s * sinθ
    val δθ = ϵ3 * ( (2*s + κ/s)*cosθ + (1/s - s)* σ * sinθ)
    val δR = ϵ3 * `Θ/r` * (1+κ) * s * cosθ
    val δΘ = ϵ3 * Θ * s * (κ*sinθ - σ * cosθ)
    val rl = r+δr
    val Rl = R+δR
    val Θl = Θ+δΘ // angular momentum
    val θl = θ+δθ 
    //val θl = if (θ<0 && abs(δθ)>abs(θ)) θ-δθ else θ+δθ

    //val absθl = abs(θ)+abs(δθ)  // if (θ>0) θ+δθ else θ-δθ // argument latitude
//    val θlt = θ+δθ // if (θ>0) absθl else - absθl
//    val θl = if (θlt > pi.as[F])  pi - θlt  else θlt 
    // val θf = if (θl > pi.as[F])  θl - twopi else θl    
    // recalculate the "state" variables here
    val sin2θ = sin(2*θl) // 2 * cos(θl) * sin(θl)
    val cos2θ = cos(2*θl) // 1 - 2 * sin(θl) * sin(θl)
    // val a = rl /(1 - ecosE) 
    val pl = Θl*Θl // MU=1
    // note:    e² = κ² + σ²
    val κl = pl/rl - 1
    val σl = pl*Rl/Θl
    val `el²` = κl*κl + σl*σl
    val βl = sqrt(1 - `el²`)
    (SpecialPolarNodal(I, θl, Ω, rl, Rl, Θl/rl), `el²`, pl, βl, sin2θ, cos2θ, n)
  }
  
 // def sppCorrections(lppState: LongPeriodState, aux: AuxVariables) : ShortPeriodState = {
  def sppCorrections(lppState: LongPeriodState) : ShortPeriodState = {
    import lppState.{_1 => lppPN,_3 => pl,_4 => βl,_5 => sin2θ,_6 => cos2θ, _7 => n}
    import lppPN._
    import sec.wgs.{J2,KE}
    import sec.ctx0.{c,s,`7c²-1`,`1-c²`,`3c²-1`}
 
    val `J2/p/2` = J2 / pl / 2
    val ϵ2 = - `J2/p/2` / pl / 2
    val δI = - 3 * ϵ2 * c * s * cos2θ
    val δθ =       ϵ2 * `7c²-1` * sin2θ / 2
    val δΩ = - 3 * ϵ2 * c * sin2θ
    val δr =   3 * ϵ2 * r * βl * `3c²-1` + `J2/p/2` * `1-c²` * cos2θ / 2
    val δR = - n * `J2/p/2` * `1-c²` * sin2θ / KE  // rdot, angular velocity
    val δrvdot = n * `J2/p/2` * (`1-c²` * cos2θ + 1.5 * `3c²-1`) / KE 
    val δspp = SpecialPolarNodal(δI,δθ,δΩ,δr,δR,δrvdot)
    val finalPN = lppPN + δspp
    (finalPN, δspp)
  }

//  def trueAnomaly(eaState: EccentricAnomalyState[F], e: F) : F = {
//    import eaState.E
//    val f = 2*atan( sqrt((1+e)/(1-e))*tan(E/2))
//    f
//  }
}

object SGP4PN  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) : SGP4PN[F] = new SGP4PN(sec)
  
}
