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
    val sin2f = 2 * cosf * sinf
    val cos2f = 1 - 2 * sinf * sinf
    val av = AuxVariables(sin(I), cos(I), p, ecosf, esinf, n, β, sin2f, cos2f)
    
    // do check
//    {
//    import av._
//    assert(abs(κ - (av.p/r - 1)) < 1E-10.as[F] ) 
//    assert(abs(σ - (av.p*R/Θ)) < 1E-10.as[F])
//    }
    
    (SpecialPolarNodal(I,θ,Ω,r,R,Θ/r), av) 
  }
  
   
  def lppCorrections(pn: (SpecialPolarNodal[F], AuxVariables[F]), secElemt: SGPElems[F]) : (SpecialPolarNodal[F], F, F, F, F, F, F) = {
    import pn._1._,pn._2._,sec.wgs.`J3/J2`, secElemt.n
    val ϵ3 = `J3/J2`/p/2
    val sinθ = sin(θ)
    val cosθ = cos(θ)
    val δr = ϵ3 * p * s * sinθ
    val δθ = ϵ3 * ( (2*s + κ/s)*cosθ + (1/s - s)* σ * sinθ)
    val δR = ϵ3 * `Θ/r` * (1+κ) * s * cosθ
    val δΘ = ϵ3 * Θ * s * (κ*sinθ - σ * cosθ)
    val rl = r+δr
    (SpecialPolarNodal(I,θ+δθ, Ω, rl, R+δR, (Θ+δΘ)/rl), 0.as[F], p, β, sin2f, cos2f, n)
  }
  
 // def sppCorrections(lppState: LongPeriodState, aux: AuxVariables) : ShortPeriodState = {
  def sppCorrections(lppState: LongPeriodState) : ShortPeriodState = {
    import lppState.{_1 => lppPN,_3 => pl,_4 => βl,_5 => sin2θ,_6 => cos2θ, _7 => n}
    import lppPN._
    import sec.wgs.{J2,KE}
    import sec.ctx0.{c,s,x7thm1,x1mth2,con41}
 
    val temp1  = J2 / pl / 2
    val temp2  = temp1 / pl 
    val δI = 1.5 * temp2 * c * s * cos2θ
    val δθ = - temp2 * x7thm1 * sin2θ / 4
    val δΩ = 1.5 * temp2 * c * sin2θ
    val δr =  - r * 1.5 * temp2 * βl * con41 + temp1 * x1mth2 * cos2θ / 2
    val δR = - n * temp1 * x1mth2 * sin2θ / KE  // rdot, angular velocity
    val δrvdot = n * temp1 * (x1mth2 * cos2θ + 1.5 * con41) / KE 
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
