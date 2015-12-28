package predict4s.sgp.pn

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._
import predict4s.sgp.GeoPotentialCoefs
import predict4s.sgp._
import predict4s.sgp.LaneCoefs
import predict4s.coord.CartesianElems
import predict4s.coord.SpecialPolarNodal
import predict4s.coord.PolarNodalElems
import predict4s.coord.CoordTransformation._
import predict4s.coord.SGPElems

// compute the corrections in polar-nodal variables. 
class SGP4PN[F : Field : NRoot : Order : Trig](
 sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4(sec) {
   
  type ShortPeriodCorrections = SpecialPolarNodal[F]
  type FinalState = SpecialPolarNodal[F]
  type ShortPeriodState = (SpecialPolarNodal[F], ShortPeriodCorrections) // final values, corrections ShortPeriodPolarNodalContext
//  type LongPeriodState = (SpecialPolarNodal[F], SpecialPolarNodal[F]) // final values, context variables
  type LongPeriodState = (SpecialPolarNodal[F], F, F, F, F, F, F) // final values, context variables
  type EccentricAState = EccentricAnomalyState[F]
 
  case class AuxVariables(s: SinI, c: CosI, p: F, κ: F, σ: F, n: F, β: F, sin2f: F, cos2f: F)
 
  override def periodicCorrections(secularElemt : SGPElems[F])
      :  (FinalState, ShortPeriodState, LongPeriodState, EccentricAState) = {
    val eaState = solveKeplerEq(secularElemt)
    val delauney = DelauneyVars.sgpelem2Delauney(secularElemt) // NOTE: MU=1
    val pnContext = delauney2PolarNodal(eaState, delauney, secularElemt)
    //val pnContext = sgpelems2PolarNodal(eaState, secularElemt)
    val lppPNContext = lppCorrectionsSPN(pnContext, secularElemt)
    //val lppSPN = lppPNContext._1.polarNodal2SpecialPolarNodal()
    val sppPolarNodalContext = sppCorrections(lppPNContext)
    val finalPNState = sppPolarNodalContext._1
    (finalPNState, sppPolarNodalContext, lppPNContext, eaState)
  } 

  override def propagate2CartesianContext(t: Minutes) = {
    val ((finalPolarNodal, sppState, lppState, eaState), secularElemt) = propagate2PolarNodalContext(t)
    import finalPolarNodal._
    val uPV: CartesianElems[F] = polarNodal2UnitCartesian(I, su, Ω)
    val (p, v) = convertAndScale2UnitVectors(uPV.pos, uPV.vel, mrt, mvt, rvdot)
    val posVel = CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
    (posVel, uPV, finalPolarNodal, sppState, lppState, eaState)       
  }
  
  /**
   * Solve Kepler's equation expressed in Delauney's variables 
   * 		M = E − e sinE
   * to compute E the eccentric anomaly.
   * The Newton-Raphson iterations start from E0 = M = (l in Delauneys).
   */
  def solveKeplerEq(elem : SGPElems[F]): EccentricAnomalyState[F] = {
       
    import elem.{e,M}, sec.wgs.twopi
    
    def loop(E: F, remainingIters: Int) : EccentricAnomalyState[F] = {
      val sinE = sin(E)
      val cosE = cos(E)
      val ecosE = e*cosE 
      val esinE = e*sinE
      val fdot = 1 - ecosE
      val f = M - (E - esinE)
      val tem : F = f / fdot  
      val incr =
        if (abs(tem) > 0.95.as[F]) {
          if (tem > 0.as[F]) 0.95.as[F] else -0.95.as[F]
        } else tem
      val En = E+incr
      if (remainingIters <= 0 || abs(incr) < 1e-12.as[F]) {
        EccentricAnomalyState(En,cosE,sinE,ecosE,esinE)   
      } else {
        loop(En, remainingIters - 1)
      }
    }
    loop(M, 10)
  }
    
  def delauney2SpecialPolarNodal(eaState: EccentricAnomalyState[F], secularElem : SGPElems[F]) = {
    import eaState._ 
    import secularElem._
    import sec.wgs.twopi
    
    // It follows the usual transformation to polar-nodal variables
    // (r, θ, R, Θ) −→ (F, C, S, a)  with C' = e'cosg and  S' = e'sing
    // Note: Vallado's SGP4 uses rθdot = Θ/r instead of Θ
    // here the l probably means long period, not Lyddane
    val `e²` = e*e
    val p = a*(1 - `e²`)  // semilatus rectum , as MU=1, p=Z²
    if (p < 0.as[F]) throw new Exception("p: " + p)
    val `√p` = sqrt(p)  
    
    val r = a * (1 - ecosE)        // r´        
    val R = sqrt(a) /r * esinE     // R´ = L/r *esinE, L=sqrt(nu*a), MU=1 in SGP4 variables, later applied
    val rvdot = `√p`/r         // Θ’/r’ that is Θ/r 
    val β = sqrt(1 - `e²`)     // β’ = G/L

    val sinf = a / r * β * sinE
    val cosf = a / r * (cosE - e)
    val f = atan2(sinf, cosf)
    val θ0to2pi = f + ω
    val θ = if (θ0to2pi > pi.as[F])  θ0to2pi - twopi else θ0to2pi 
    val sin2f = 2 * cosf * sinf
    val cos2f = 1 - 2 * sinf * sinf
    (SpecialPolarNodal(I, θ, Ω, r, R, rvdot), `e²`, p, β, n, sin2f, cos2f) 
  }
  
  def delauney2PolarNodal(eaState: EccentricAnomalyState[F], delauney : DelauneyVars[F], secularElem : SGPElems[F]) = {
    import eaState._ 
    import delauney._
    import secularElem.{a,I,e,n}
    import sec.wgs.twopi
    
    val `e²` = e*e
    val p = a*(1 - `e²`)  // semilatus rectum , as MU=1, p=Z²
    if (p < 0.as[F]) throw new Exception("p: " + p)
    val `√p` = sqrt(p)  

    val r = a * (1 - ecosE)        // r´        
    val R = L /r * esinE     // R´ = L/r *esinE, L=sqrt(nu*a), MU=1 in SGP4 variables, later applied
    val Θ = G             // MU=1 
    val β = sqrt(1 - `e²`)     // β’ = G/L
    val numer = β * sinE
    val denom = (cosE - e)
    val sinf = a/r*numer
    val cosf = a/r*denom
    val esinf = e*sinf
    val ecosf = e*cosf
    val f = atan2(sinf, cosf)
    //val f = trueAnomaly(eaState, e)
    val θ0to2pi = f + g
    val θ = if (θ0to2pi > pi.as[F])  θ0to2pi - twopi else θ0to2pi 
    val ν = h
    val N = H
    val sin2f = 2 * cosf * sinf
    val cos2f = 1 - 2 * sinf * sinf
    val av = AuxVariables(sin(I), cos(I), p, ecosf, esinf, n, β, sin2f, cos2f)
    
    // do check
//    {
//    import av._
//    assert(abs(κ - (av.p/r - 1)) < 1E-10.as[F] ) 
//    assert(abs(σ - (av.p*R/Θ)) < 1E-10.as[F])
//    }
    
    (PolarNodalElems(r,θ,ν,R,Θ,N), av) 
  }
  
//  def delauney2PolarNodal(eaState: EccentricAnomalyState, secularElem : SGPElems[F]) = {
//    import eaState._ 
//    import secularElem._
//
//    // It follows the usual transformation to polar-nodal variables
//    // (r, θ, R, Θ) −→ (F, C, S, a)  with C' = e'cosg and  S' = e'sing
//    // Note: Vallado's SGP4 uses rθdot = Θ/r instead of Θ
//    // here the l probably means long period, not Lyddane
//    val `e²` = e*e
//    val p = a*(1 - `e²`)  // semilatus rectum , as MU=1, p=Z²
//    if (p < 0.as[F]) throw new Exception("p: " + p)
//    val `√p` = sqrt(p)  
//    
//    val r = a * (1 - ecosE)        // r´        
//    val R = sqrt(a) /r * esinE     // R´ = L/r *esinE, L=sqrt(nu*a), MU=1 in SGP4 variables, later applied
//    val Θ = `√p`             // MU=1 
//    val β = sqrt(1 - `e²`)     // β’ = G/L
//    val f = atan2(sinf, cosf)
//    val θ = f + ω
//    val sinf = a / r * β * sinE
//    val cosf = a / r * (cosE - e)
//    val f = atan2(sinf, cosf)
//    val θ = f + ω
////    val sin2f = 2 * cosf * sinf
////    val cos2f = 1 - 2 * sinf * sinf
//    (PolarNodalElems(I, θ, Ω, r, Θ, rvdot), `e²`, p, β, n) 
//  }
  def lppCorrections(pn: (PolarNodalElems[F], AuxVariables)) : (PolarNodalElems[F],PolarNodalElems[F]) = {
    import pn._1._,pn._2._,sec.wgs._
    val `p/r` = p/r
    val ϵ3 = `J3/J2`/p/2
    val sinθ = sin(θ)
    val cosθ = cos(θ)
    val δr = ϵ3 * p * s * sinθ
    val δθ = ϵ3 * ( (2*s + κ/s)*cosθ + (1/s - s)* σ * sinθ)
    val δR = ϵ3 * (Θ/r) * (1+κ) * s * cosθ
    val δΘ = ϵ3 * Θ * s * (κ*sinθ - σ * cosθ)
    (PolarNodalElems(r+δr,θ+δθ,ν,R+δR,Θ+δΘ, (Θ+δΘ)*c), PolarNodalElems(δr,δθ,0.as[F],δR,δΘ,δΘ*c))
  }
    
  def lppCorrectionsSPN(pn: (PolarNodalElems[F], AuxVariables), secElemt: SGPElems[F]) : (SpecialPolarNodal[F], F, F, F, F, F, F) = {
    import pn._1._,pn._2._,sec.wgs._, secElemt.n
    val `p/r` = p/r
    val ϵ3 = `J3/J2`/p/2
    val sinθ = sin(θ)
    val cosθ = cos(θ)
    val δr = ϵ3 * p * s * sinθ
    val δθ = ϵ3 * ( (2*s + κ/s)*cosθ + (1/s - s)* σ * sinθ)
    val δR = ϵ3 * (Θ/r) * (1+κ) * s * cosθ
    val δΘ = ϵ3 * Θ * s * (κ*sinθ - σ * cosθ)
    val rl = r+δr
    (SpecialPolarNodal(acos(c),θ+δθ,ν, rl, R+δR, (Θ+δΘ)/rl), 0.as[F], p, β, sin2f, cos2f, n)
  }
    
  def lppCorrectionsSPNAlternative(pn: (PolarNodalElems[F], AuxVariables)) : (SpecialPolarNodal[F],SpecialPolarNodal[F]) = {
    import pn._1._,pn._2._,sec.wgs._
    val `p/r` = p/r
    val ϵ3 = `J3/J2`/p/2
    val sinθ = sin(θ)
    val cosθ = cos(θ)
    val δr = ϵ3 * p * s * sinθ
    val δθ = ϵ3 * ( (2*s + κ/s)*cosθ + (1/s - s)* σ * sinθ)
    val δR = ϵ3 * (Θ/r) * (1+κ) * s * cosθ
    val δΘ = ϵ3 * Θ * s * (κ*sinθ - σ * cosθ)
    val rl = r+δr
    (SpecialPolarNodal(acos(c),θ+δθ,ν, rl, R+δR, (Θ+δΘ)/rl), SpecialPolarNodal(0.as[F],δθ,0.as[F],δr,δR,δΘ))
  }
  
 // def sppCorrections(lppState: LongPeriodState, aux: AuxVariables) : ShortPeriodState = {
  def sppCorrections(lppState: LongPeriodState) : ShortPeriodState = {
    import lppState.{_1 => lppPN,_3 => pl,_4 => βl,_5 => sin2f,_6 => cos2f, _7 => n}
    import lppPN._
    import sec.wgs.{J2,KE}
    import sec.ctx0.{c,s,x7thm1,x1mth2,con41}
    //val sPN = lppPN.polarNodal2SpecialPolarNodal()
    val temp1  = J2 / pl / 2
    val temp2  = temp1 / pl 
    val δI = 1.5 * temp2 * c * s * cos2f
    val δsu = - temp2 * x7thm1 * sin2f / 4
    val δΩ = 1.5 * temp2 * c * sin2f
    val δr =  - r * 1.5 * temp2 * βl * con41 + temp1 * x1mth2 * cos2f / 2
    val δrdot = - n * temp1 * x1mth2 * sin2f / KE
    val δrvdot = n * temp1  * (x1mth2 * cos2f + 1.5 * con41) / KE 
    val δspp = SpecialPolarNodal(δI,δsu,δΩ,δr,δrdot,δrvdot)
    val finalPN = lppPN + δspp
    (finalPN, δspp)
  }

  def trueAnomaly(eaState: EccentricAnomalyState[F], e: F) : F = {
    import eaState.E
    val f = 2*atan( sqrt((1+e)/(1-e))*tan(E/2))
    f
  }
}

object SGP4PN  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) : SGP4PN[F] = new SGP4PN(sec)
  
}
