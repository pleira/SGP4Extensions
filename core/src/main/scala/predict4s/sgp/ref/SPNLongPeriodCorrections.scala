package predict4s
package sgp
package ref

import org.scalactic.Or
import org.scalactic.Good
import org.scalactic.Bad
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord._

trait SPNLongPeriodCorrections[@sp(Double) F] {
  
  def lppCorrectionsSPN(spnCtx: SPNSecularCtx[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]): (SpecialPolarNodal[F], LongPeriodContext[F]) = {
    import spnCtx.{_1 => spn}, spn._, spnCtx.{_2 => secularCtx}, secularCtx.{_1 => elem}
    import elem.{Ω=>_, _} // do not import Ω from secular elements, just use spn version
    import secularCtx._2.{c,s,`s²`}
    import secularCtx._3.{KE,`J3/J2`,`2pi`}

    val p = a*(1 - e*e)  // semilatus rectum , as MU=1, p=Z²
    val ϵ3 = `J3/J2`/p/2 // ϵ3 = 1/2 aE/p C30/C20 in Lara's
    
    // note the singularity for (sinI) s = 0, 
    // use s_ only in terms that would "explode" 
    val s_ = if (s < 1.5e-12.as[F]) 1.5e-12.as[F] else s
    
    val σ = p*R/Θ
    val κ = p/r - 1
    val sinθ = sin(θ)
    val cosθ = cos(θ)
    val ξ = s * sinθ
    val χ = s * cosθ
    
//    val δθ = ϵ3 * ((2*s_ + κ/s_)*cosθ + (1/s_ - s_)* σ * sinθ)
//    val δΩ = ϵ3 * c/s_ * (κ*cosθ + σ * sinθ) 
//    val δr = ϵ3 * p * s * sinθ
//    val δR = ϵ3 * `Θ/r` * (1+κ) * s * cosθ
//    val δΘ = ϵ3 * Θ * s * (κ*sinθ - σ * cosθ)
    val δθ =   ϵ3 * ((2*s_ + κ/s_)*cosθ + (1/s_ - s_)* σ * sinθ)
    val δΩ = - ϵ3 * c/s_ * (κ*cosθ + σ * sinθ) 
    
   // val δθplusδΩ = ϵ3 * (2*χ +κ*χ/`s²` + ξ*σ/`s²` - ξ*σ -  (c*κ*χ + c*σ*ξ)/`s²`)
   // val δθplusδΩ = ϵ3 * (2*χ - ξ*σ + (κ*χ + ξ*σ - c*(κ*χ + σ*ξ))/((1+c)*(1-c)) )
   // val δθplusδΩ = ϵ3 * (2*χ - ξ*σ + (κ*χ + ξ*σ)/(1+c))
   // val δθplusδΩ = ϵ3 * (2*χ + (κ*χ - ξ*σ*c )/(1+c))
    val δr = ϵ3 * p * ξ
    val δR = ϵ3 * `Θ/r` * (1+κ) * χ
    val δΘ = ϵ3 * Θ  * (κ*ξ  - σ*χ)

    // derive correction for the inclination, cosI' = c / (1 + ϵ3*s*(κ*sinθ - σ*cosθ))
    val cosI = c / (1 + ϵ3*(κ*ξ - σ*χ))
    val Il = acos(cosI)
    
    // apply corrections
    val θl_ = θ+δθ 
    // convert to -pi,pi range
    val diff = abs(θl_) - pi.as[F]
    val θl = if (diff > 0.as[F]) {
      if (θl_ > 0.as[F]) θl_ - `2pi`   
      else `2pi` + θl_
    } else θl_
    
    val Ωl = Ω+δΩ
    val rl = r+δr
    val Rl = R+δR
    val Θl = Θ+δΘ // angular momentum
    
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

  // this one is just used to do a test with Lara's lpp corrections
  def lppCorrectionsCPN(spnCtx: SPNSecularCtx[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]) 
       : CSpecialPolarNodal[F] = {
    import spnCtx.{_1 => spn}, spn._, spnCtx.{_2 => secularCtx}, secularCtx.{_1 => elem}
    import elem.{Ω=>_, _} // do not import Ω from secular elements, just use spn version
    import secularCtx._2.{c,s,`s²`}
    import secularCtx._3.{KE,`J3/J2`,`2pi`}

    val p = a*(1 - e*e)  // semilatus rectum , as MU=1, p=Z²
    val ϵ3 = `J3/J2`/p/2 // ϵ3 = 1/2 aE/p C30/C20 in Lara's
    
    // note the singularity for (sinI) s = 0, 
    // use s_ only in terms that would "explode" 
    val s_ = if (s < 1.5e-12.as[F]) 1.5e-12.as[F] else s
    
    val σ = p*R/Θ
    val κ = p/r - 1
    val sinθ = sin(θ)
    val cosθ = cos(θ)
    val ξ = s * sinθ
    val χ = s * cosθ
    
//    val δθ = ϵ3 * ((2*s_ + κ/s_)*cosθ + (1/s_ - s_)* σ * sinθ)
//    val δΩ = ϵ3 * c/s_ * (κ*cosθ + σ * sinθ) 
//    val δr = ϵ3 * p * s * sinθ
//    val δR = ϵ3 * `Θ/r` * (1+κ) * s * cosθ
//    val δΘ = ϵ3 * Θ * s * (κ*sinθ - σ * cosθ)
    val δθ =   ϵ3 * ((2*s_ + κ/s_)*cosθ + (1/s_ - s_)* σ * sinθ)
    val δΩ = - ϵ3 * c/s_ * (κ*cosθ + σ * sinθ) 
    
//    val δθplusδΩ = ϵ3 * (2*χ +κ*χ/`s²` + ξ*σ/`s²` - ξ*σ -  (c*κ*χ + c*σ*ξ)/`s²`)
    // val δθplusδΩ = ϵ3 * (2*χ - ξ*σ + (κ*χ + ξ*σ - c*(κ*χ + σ*ξ))/((1+c)*(1-c)) )
        // val δθplusδΩ = ϵ3 * (2*χ - ξ*σ + (κ*χ + ξ*σ)/(1+c))
        // val δθplusδΩ = ϵ3 * (2*χ + (κ*χ - ξ*σ*c )/(1+c))
    val δr = ϵ3 * p * ξ
    val δR = ϵ3 * `Θ/r` * (1+κ) * χ
    val δΘ = ϵ3 * Θ  * (κ*ξ  - σ*χ)

    // derive correction for the inclination, cosI' = c / (1 + ϵ3*s*(κ*sinθ - σ*cosθ))
    val cosI = c / (1 + ϵ3*(κ*ξ - σ*χ))
    //val Il = acos(cosI)
    
    // apply corrections
    val θl_ = θ+δθ 
    // convert to -pi,pi range
    val diff = abs(θl_) - pi.as[F]
    val θl = if (diff > 0.as[F]) {
      if (θl_ > 0.as[F]) θl_ - `2pi`   
      else `2pi` + θl_
    } else θl_
    
    val Ωl = Ω+δΩ
    val rl = r+δr
    val Rl = R+δR
    val Θl = Θ+δΘ // angular momentum
    
    // recalculate the "state" variables here
    val sin2θ = sin(2*θl) // 2 * cos(θl) * sin(θl)
    val cos2θ = cos(2*θl) // 1 - 2 * sin(θl) * sin(θl)
    // val a = rl /(1 - ecosE) 
    val pl = Θl*Θl // MU=1
    val κl = pl/rl - 1
    val σl = pl*Rl/Θl
    val `el²` = κl*κl + σl*σl
    val βl = sqrt(1 - `el²`)
    CSpecialPolarNodal(cosI, θl, Ωl, rl, Rl, Θl/rl)
  }
 
}