package predict4s.sgp.pn

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord._
import predict4s.coord.SGPElemsConversions._

// compute the corrections in polar-nodal variables. 
class SGP4PN[F : Field : NRoot : Order : Trig](
 sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4(sec) with SimpleKeplerEq with ShortPeriodPolarNodalCorrections[F] {
 
  val wgs = sec.wgs
  val ctx0 = sec.ctx0   
 
  override def periodicCorrections(secularElemt : SGPElems[F])
      :  (FinalState[F], ShortPeriodState[F], LongPeriodState[F], AnomalyState[F]) = {
    val eaState = solveKeplerEq(secularElemt)
    val pnSecularContext = sgpelems2SpecialPolarNodal(eaState, secularElemt, wgs)
    val lppSPNContext = lppCorrections(pnSecularContext, secularElemt)
    val sppPolarNodalContext = sppCorrections(lppSPNContext)
    val finalPNState = sppPolarNodalContext._1
    (finalPNState, sppPolarNodalContext, lppSPNContext, eaState)
  } 
  
   
  def lppCorrections(pn: (SpecialPolarNodal[F], AuxVariables[F]), secElemt: SGPElems[F]) : (SpecialPolarNodal[F], LongPeriodContext[F]) = {
    import pn._1._,pn._2.{p,s,c},sec.wgs.{KE,`J3/J2`}, secElemt.n,sec.wgs.twopi
    //(s: F, c: F, p: F, κ: F, σ: F, n: F, β: F, sin2f: F, cos2f: F)
    val ϵ3 = `J3/J2`/p/2 // ϵ3 = 1/2 aE/p C30/C20 in Lara's
    
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
    // note: e² = κ² + σ²
    val κl = pl/rl - 1
    val σl = pl*Rl/Θl
    val `el²` = κl*κl + σl*σl
    val βl = sqrt(1 - `el²`)
    (SpecialPolarNodal(I, θl, Ω, rl, Rl, Θl/rl), LongPeriodContext(`el²`, pl, βl, sin2θ, cos2θ, n))
  }
}

object SGP4PN  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) : SGP4PN[F] = new SGP4PN(sec)
  
  def apply[F : Field : NRoot : Order : Trig](tle: TLE, wgs: SGPConstants[F]) :  SGP4PN[F] =  {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs)
    val secular = BrouwerLaneSecularCorrections(elem0AndCtx, wgs)
    new SGP4PN[F](secular)
  }  
}
