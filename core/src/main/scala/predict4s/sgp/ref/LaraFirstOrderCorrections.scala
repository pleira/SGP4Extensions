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
import predict4s.coord.LaraConversions._
import predict4s.coord.SGPElemsConversions._
import predict4s.coord.{SGPElems,AnomalyState}

trait LaraFirstOrderCorrections[@sp(Double) F] {
  
  // This implementation includes more terms with respect lppCorrectionsOld
  def lppCorrections(lnSingular: LaraNonSingular[F], secularElemt : SGPSecularCtx[F])(implicit ev: Field[F])
      : (LaraNonSingular[F], SGPSecularCtx[F]) = {
    val elem = secularElemt._1
		val ictx = secularElemt._2    
    val wgs = secularElemt._3
    import lnSingular._
    import elem.{a,e}, wgs.`J3/J2`, ictx.{s,c,`c²`}
    
    val `e²` = e*e
    val p = a*(1 - `e²`)  // semilatus rectum , as MU=1
    val `p/r` = p/r
    val ϵ3 = `J3/J2`/p/2
    val κ = `p/r` - 1
    val σ =  p*R/Θ
      
    val `χ²` = χ*χ
    val `ξ²` = ξ*ξ   
    
    val δψ =  ϵ3 * (2*χ + (κ*χ - c*σ*ξ)/(1+c))
    val δξ =  ϵ3 * (2*`χ²` + κ*(1 - `ξ²`))
    val δχ = -ϵ3 * (`c²`*σ + (2 + κ)*ξ*χ)
    val δr =  ϵ3 * p * ξ
    val δR =  ϵ3 * (Θ/r) * (1 + κ) * χ
    val δΘ =  ϵ3 * Θ * (κ*ξ - σ*χ)
    
    val rl = r+δr
    val Rl = R+δR
    val Θl = δΘ+Θ
    (LaraNonSingular(ψ+δψ,ξ+δξ,χ+δχ,rl,Rl,Θl), secularElemt)
  }
  
  def sppCorrections(lppState: (LaraNonSingular[F], SGPSecularCtx[F]))(implicit ev: Field[F]) 
    : LaraNonSingular[F] = {
    import lppState.{_1=>lnSingular,_2=>secularElemt}
    val elem = secularElemt._1
		val ictx = secularElemt._2    
    val wgs = secularElemt._3
    import wgs.J2, ictx.{c,`c²`,s}
    import lnSingular._
    
    val p = Θ*Θ 
    val ϵ2 = -J2/p/p/4
    
    val `χ²` = χ*χ
    val `ξ²` = ξ*ξ
    val δψ = -ϵ2 * ((1+7*c)/(1+c)) * ξ * χ 
    val δξ = -ϵ2 * (`χ²` - 3*`c²`) * ξ
    val δχ =  ϵ2 * (`ξ²` - 3*`c²`) * χ
    val δr =  ϵ2 * r * (`ξ²` - `χ²` - 3 + 9*`c²`)
    val δR =  ϵ2 * 4 * (Θ/r) * ξ * χ
    val δΘ =  ϵ2 * 3 * Θ * (`ξ²` - `χ²`)
    LaraNonSingular(ψ+δψ,ξ+δξ,χ+δχ,r+δr,R+δR,Θ+δΘ)
  }
  
  def sppCorrectionsAlternative(lppState: (LaraNonSingular[F], SGPSecularCtx[F]))(implicit ev: Field[F]) 
    : LaraNonSingular[F] = {
    import lppState.{_1=>lnSingular,_2=>secularElemt}
    val elem = secularElemt._1
		val ictx = secularElemt._2    
    val wgs = secularElemt._3
    import wgs.J2, ictx.{c,`c²`,s,`s²`}
    import lnSingular._
    
    val p = Θ*Θ 
    val ϵ2 = -J2/p/p/4
    val `χ²` = χ*χ
    val `ξ²` = ξ*ξ
    val cos2θ = `χ²` - `ξ²`
    val sin2θ = 2*ξ*χ/`s²`
    val δψ = -ϵ2 * ((1+7*c)/(1+c)) * ξ * χ 
    val δξ = -ϵ2 * (`χ²` - 3*`c²`) * ξ
    val δχ =  ϵ2 * (`ξ²` - 3*`c²`) * χ
    val δr =  ϵ2 * r * ( - cos2θ - 3 + 9*`c²`)
    val δR =  ϵ2 * 2 * `s²`* sin2θ * (Θ/r)      // ϵ2 * 4 * (Θ/r) * ξ * χ
    val δΘ =  - ϵ2 * 3 * Θ * cos2θ
    LaraNonSingular(ψ+δψ,ξ+δξ,χ+δχ,r+δr,R+δR,Θ+δΘ)
  }
  
  def allCorrections(lnSingular: LaraNonSingular[F], secularElemt : SGPSecularCtx[F])(implicit ev: Field[F])
      : LaraNonSingular[F] = {
    val elem = secularElemt._1
		val ictx = secularElemt._2    
    val wgs = secularElemt._3
    import lnSingular._
    import elem.{a,e}, wgs.{`J3/J2`,J2}, ictx.{s,c,`c²`}
    
    val `e²` = e*e
    val p = a*(1 - `e²`)  // semilatus rectum , as MU=1
    val `p/r` = p/r
    val ϵ3 = `J3/J2`/p/2
    val κ = `p/r` - 1
    val σ =  p*R/Θ      
    val `χ²` = χ*χ
    val `ξ²` = ξ*ξ   
    
    // long period corrections
    val δψl =  ϵ3 * (2*χ + (κ*χ - c*ξ*σ)/(1+c))
    val δξl =  ϵ3 * (2*`χ²` + κ*(1 - `ξ²`))
    val δχl = -ϵ3 * (`c²` * σ + (2 + κ)*χ*ξ)
    val δrl =  ϵ3 * p * ξ
    val δRl =  ϵ3 * (Θ/r) * (1 + κ) * χ
    val δΘl =  ϵ3 * Θ * (κ*ξ - σ*χ)

    // LPP corrections in Lara Non-Sigular variables
    val ψl = ψ+δψl
    val ξl = ξ+δξl
    val χl = χ+δχl
    val rl = r+δrl
    val Rl = R+δRl
    val Θl = Θ+δΘl
    
    // now, use the LPP corrections variables to calculate SPP corrections
    val pl = Θl*Θl 
    val ϵ2 = -J2/pl/pl/4
    val `χl²` = χl*χl
    val `ξl²` = ξl*ξl
    val δψs = -ϵ2 * ((1+7*c)/(1+c)) * ξl * χl 
    val δξs = -ϵ2 * (`χl²` - 3*`c²`) * ξl
    val δχs = -ϵ2 * (`ξl²` - 3*`c²`) * χl
    val δrs =  ϵ2 * r * (`ξl²` - `χl²` - 3 + 9*`c²`)
    val δRs =  ϵ2 * 4 * (Θ/r) * ξl * χl
    val δΘs =  ϵ2 * 3 * Θ * (`ξl²` - `χl²`)
    LaraNonSingular(ψl+δψs,ξl+δξs,χl+ δχs,rl+δrs,Rl+δRs,Θl+δΘs)   
  }
    
//  def lppCorrectionsOld(lnSingular: LaraNonSingular[F], secularElemt : SGPSecularCtx[F])(implicit ev: Field[F])
//      : (LaraNonSingular[F], LaraLongPeriodContext[F]) = {
//    val elem = secularElemt._1
//		val ictx = secularElemt._2    
//    val wgs = secularElemt._3
//    import lnSingular._
//    import elem.{a,e}, wgs.`J3/J2`, ictx.{s,c}
//    
//    val `e²` = e*e
//    val p = a*(1 - `e²`)  // semilatus rectum , as MU=1
//    val ϵ3 = `J3/J2`/p/2
//    val `p/r` = p/r
//    val δψ = 2 * ϵ3 * χ 
//    val δξ = χ * δψ
//    val δχ = - ξ * δψ
//    val δr = ϵ3 * ξ * p
//    val δR = ϵ3 * (Θ/r) * `p/r` * χ
//    val δΘ = ϵ3 * Θ * ((`p/r` - 1) * ξ - p*R*χ/Θ)
//    
//        // recalculate the "state" variables here
//    // val a = rl /(1 - ecosE) 
//    val Θl = δΘ+Θ
//    val Rl = R+δR
//    val rl = r+δr
//    val pl = Θl*Θl // MU=1
//    (LaraNonSingular(ψ+δψ,ξ+δξ,χ+δχ,rl,Rl,Θl), LaraLongPeriodContext(0.as[F], pl, 0.as[F], 0.as[F], 0.as[F], 0.as[F]))
//  }  
}

