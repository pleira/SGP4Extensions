package predict4s.sgp.algo

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord._
import predict4s.coord.SGPElemsConversions._

trait SPNLongPeriodCorrections[F] extends LongPeriodCorrections[F] with SimpleKeplerEq {
  
  val wgs: SGPConstants[F]
  
  override def lppCorrections(secularElemt : SGPElems[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]) 
  : (SpecialPolarNodal[F], LongPeriodContext[F]) = {
    // long period corrections in SpecialPolarNodal coordinates
    val eaState = solveKeplerEq(secularElemt)
    val spnSecularContext = sgpelems2SpecialPolarNodal(eaState, secularElemt, wgs)
    lppPNCorrections(spnSecularContext)
  }
   
  def lppPNCorrections(spn: (SpecialPolarNodal[F], AuxVariables[F]))(implicit ev: Field[F], nr: NRoot[F], trig: Trig[F])
       : (SpecialPolarNodal[F], LongPeriodContext[F]) = {
    import spn._1._,spn._2.{p,s,c},wgs.{KE,`J3/J2`,twopi}
    //(s: F, c: F, p: F, κ: F, σ: F, n: F, β: F, sin2f: F, cos2f: F)
    val ϵ3 = `J3/J2`/p/2 // ϵ3 = 1/2 aE/p C30/C20 in Lara's
    
    val σ = p*R/Θ
    val κ = p/r - 1
    val sinθ = sin(θ)
    val cosθ = cos(θ)
    val δr = ϵ3 * p * s * sinθ
    val δθ = ϵ3 * ( (2*s + κ/s)*cosθ + (1/s - s)* σ * sinθ)
    val δΩ = ϵ3 * c/s * (κ*cosθ + σ * sinθ) 
    val δR = ϵ3 * `Θ/r` * (1+κ) * s * cosθ
    val δΘ = ϵ3 * Θ * s * (κ*sinθ - σ * cosθ)
    
    val θl = θ+δθ 
    val Ωl = Ω-δΩ
    val rl = r+δr
    val Rl = R+δR
    val Θl = Θ+δΘ // angular momentum
    // derive correction for the inclination, cosI' = c / (1 + ϵ3*s*(κ*sinθ - σ*cosθ))
    val cosI = c / (1 + ϵ3*s*(κ*sinθ - σ*cosθ))
    val Il = acos(cosI)
    
    // recalculate the "state" variables here
    val sin2θ = sin(2*θl) // 2 * cos(θl) * sin(θl)
    val cos2θ = cos(2*θl) // 1 - 2 * sin(θl) * sin(θl)
    // val a = rl /(1 - ecosE) 
    val pl = Θl*Θl // MU=1
    val κl = pl/rl - 1
    val σl = pl*Rl/Θl
    val `el²` = κl*κl + σl*σl
    val βl = sqrt(1 - `el²`)
    (SpecialPolarNodal(Il, θl, Ωl, rl, Rl, Θl/rl), LongPeriodContext(`el²`, pl, sqrt(pl), βl, sin2θ, cos2θ))
  }
  
  def lppCorrectionsAlt(spn: (SpecialPolarNodal[F], AuxVariables[F]))(implicit ev: Field[F], nr: NRoot[F], trig: Trig[F])
       : (SpecialPolarNodal[F], LongPeriodContext[F]) = {
    import spn._1._,spn._2.{p,s,c},wgs.{KE,`J3/J2`,twopi}
    //(s: F, c: F, p: F, κ: F, σ: F, n: F, β: F, sin2f: F, cos2f: F)
    val ϵ3 = `J3/J2`/p/2 // ϵ3 = 1/2 aE/p C30/C20 in Lara's
    val σ = p*R/Θ
    val κ = p/r - 1
    val sinθ = sin(θ)
    val cosθ = cos(θ)
    val ξ = s * sinθ
    val χ = s * cosθ 
    val δθ = ϵ3 * ( (2*s + κ/s)*cosθ + (1/s - s)* σ * sinθ)
    val δΩ = ϵ3 * c/s * (κ*cosθ + σ * sinθ) 
    val δr = ϵ3 * p * ξ
    val δR = ϵ3 * `Θ/r` * (1+κ) * χ
    val δΘ = ϵ3 * Θ * (κ*ξ - σ*χ)
    val rl = r+δr
    val Rl = R+δR
    val Θl = Θ+δΘ // angular momentum
    val θl = θ+δθ 
    val Ωl = Ω-δΩ
    // recalculate the "state" variables here
    val sin2θ = sin(2*θl) // 2 * cos(θl) * sin(θl)
    val cos2θ = cos(2*θl) // 1 - 2 * sin(θl) * sin(θl)
    // val a = rl /(1 - ecosE) 
    val pl = Θl*Θl // MU=1
    val κl = pl/rl - 1
    val σl = pl*Rl/Θl
    val `el²` = κl*κl + σl*σl
    val βl = sqrt(1 - `el²`)
    (SpecialPolarNodal(I, θl, Ωl, rl, Rl, Θl/rl), LongPeriodContext(`el²`, pl, sqrt(pl), βl, sin2θ, cos2θ))
  }
  
}