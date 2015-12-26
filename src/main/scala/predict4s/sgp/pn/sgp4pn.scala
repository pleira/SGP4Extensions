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
   
  type ShortPeriodCorrections = PolarNodalElems[F]
  type FinalState = PolarNodalElems[F]
  type ShortPeriodState = (PolarNodalElems[F], ShortPeriodCorrections) // final values, corrections ShortPeriodPolarNodalContext
  type LongPeriodState = (PolarNodalElems[F], PolarNodalElems[F]) // final values, context variables
  type EccentricAState = EccentricAnomalyState[F]
 
  case class AuxVariables(s: SinI, c: CosI, p: F, κ: F, σ: F, n: F, β: F)
 
  override def periodicCorrections(secularElemt : SGPElems[F])
      :  (FinalState, ShortPeriodState, LongPeriodState, EccentricAState) = {
    val eaState = solveKeplerEq(secularElemt)
    val delauney = DelauneyVars.sgpelem2Delauney(secularElemt) // NOTE: MU=1
    val pnContext = delauney2PolarNodal(eaState, delauney, secularElemt)
    //val pnContext = sgpelems2PolarNodal(eaState, secularElemt)
    val lppPNContext = lppCorrections(pnContext)
    val lppSPN = lppPNContext._1.polarNodal2SpecialPolarNodal()
    val sppPolarNodalContext = sppCorrections(lppPNContext, pnContext._2)
    val (finalPNState, _) = sppPolarNodalContext
    (finalPNState, sppPolarNodalContext, lppPNContext, eaState)
  }
  
  def periodicCorrectionsAlternative(secularElemt : SGPElems[F])
      :  (FinalState, ShortPeriodState, LongPeriodState, EccentricAState) = {
    val eaState = solveKeplerEq(secularElemt)
    val delauney = DelauneyVars.sgpelem2Delauney(secularElemt) // NOTE: MU=1
    val pnContext = delauney2PolarNodal(eaState, delauney, secularElemt)
    val lppPNContext = lppCorrections(pnContext)
    val lppSPN = lppPNContext._1.polarNodal2SpecialPolarNodal()
    val sppPolarNodalContext = sppCorrections(lppPNContext, pnContext._2)
    val (finalPNState, _) = sppPolarNodalContext
    (finalPNState, sppPolarNodalContext, lppPNContext, eaState)
  }
  
  override def propagate2CartesianContext(t: Minutes) = {
    val ((finalPolarNodal, sppState, lppState, eaState), secularElemt) = propagate2PolarNodalContext(t)
    import finalPolarNodal._
    val uPV: CartesianElems[F] = polarNodal2UnitCartesian(θ, R, ν)
    //val uPV: CartesianElems[F] = polarNodal2UnitCartesian(I, su, Ω)
    // FIXME
    val mrt = r ; val mvt = Θ; val rvdot = N // relations not verified, just to get this class compiled 
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
    val θ = f + ω
//    val sin2f = 2 * cosf * sinf
//    val cos2f = 1 - 2 * sinf * sinf
    (SpecialPolarNodal(I, θ, Ω, r, R, rvdot), `e²`, p, β, n) 
  }
  
  def delauney2PolarNodal(eaState: EccentricAnomalyState[F], delauney : DelauneyVars[F], secularElem : SGPElems[F]) = {
    import eaState._ 
    import delauney._
    import secularElem.{a,I,e, n}
    val `e²` = e*e
    val p = a*(1 - `e²`)  // semilatus rectum , as MU=1, p=Z²
    if (p < 0.as[F]) throw new Exception("p: " + p)
    val `√p` = sqrt(p)  

    val r = a * (1 - ecosE)        // r´        
    val R = L /r * esinE     // R´ = L/r *esinE, L=sqrt(nu*a), MU=1 in SGP4 variables, later applied
    val Θ = G             // MU=1 
    val β = sqrt(1 - `e²`)     // β’ = G/L
//    val sinf = a / r * β * sinE
//    val cosf = a / r * (cosE - e)   
//    val f = atan2(sinf, cosf)
    val numer = β * sinE
    val denom = (cosE - e)
    val f = atan2(numer, denom)
    val ecosf = e*a/r*denom
    val esinf = e*a/r*numer
    val θ = f + g
    val ν = h
    val N = H
    val av = AuxVariables(sin(I), cos(I), p, ecosf, esinf, n, β)
    
    // do check
    {
    import av._
    assert(abs(κ - (av.p/r - 1)) < 1E-6.as[F] ) 
    assert(abs(σ - (av.p*R/Θ)) < 1E-6.as[F])
    }
    
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
    val ϵ3 = `J3/J2`*α/p/2
    val sinθ = sin(θ)
    val cosθ = cos(θ)
    val δr = ϵ3 * p * s * sinθ
    val δθ = ϵ3 * ( (2*s + κ/s)*cosθ + (1/s - s)* σ * sinθ)
    val δR = ϵ3 * (Θ/r) * (1+κ) * s * cosθ
    val δΘ = ϵ3 * Θ * s * (κ*sinθ - σ * cosθ)
    (PolarNodalElems(r+δr,θ+δθ,ν,R+δR,Θ+δΘ, N), PolarNodalElems(δr,δθ,0.as[F],δR,δΘ,0.as[F]))
  }
 
  def sppCorrections(lppState: LongPeriodState, aux: AuxVariables) : ShortPeriodState = {
    import lppState.{_1 => lppPN}
    import aux.{p => pl,β => βl,_}
    import lppPN._
    import sec.wgs.{J2,KE}
    
//    val temp1  = J2 / pl / 2
//    val temp2  = temp1 / pl 
//    val δI = 1.5 * temp2 * c * s // * cos2u
//    val δsu = - temp2 * x7thm1 // * sin2u / 4
//    val δΩ = 1.5 * temp2 * c // * sin2u
//    val δr =  - r * 1.5 * temp2 * βl * con41 + temp1 * x1mth2 // * cos2u / 2
//    val δrdot = - n * temp1 * x1mth2 // * sin2u / KE
//    val δrvdot = n * temp1 // * (x1mth2 * cos2u + 1.5 * con41) / KE 
//    val δspp = SpecialPolarNodal(δI,δsu,δΩ,δr,δrdot,δrvdot)
//    val finalPN = lppPN + δspp
    val δspp = PolarNodalElems(0.as[F],0.as[F],0.as[F],0.as[F],0.as[F],0.as[F])
    (lppPN, δspp)
  }

}

object SGP4PN  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) : SGP4PN[F] = new SGP4PN(sec)
  
}
