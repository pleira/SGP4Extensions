package predict4s.sgp.lara

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord._
import predict4s.coord.LaraConversions._
import predict4s.coord.SGPElemsConversions._
import predict4s.coord.{SGPElems,AnomalyState}
/*
case class LaraLongPeriodContext[F](`el²`: F, pl: F, βl: F, sin2θ: F, cos2θ: F, n: F)

trait LaraFirstOrderCorrections[F] extends SimpleKeplerEq {
  
  val wgs : SGPConstants[F]
  val inclCtx: InclinationCtx[F]
  
  // this method uses Lara's Non Singular variables useful for the calculation of the corrections
  def periodicCorrectionsNative(secularElemt : SGPElems[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]) 
      : (LaraNonSingular[F], (LaraNonSingular[F], LaraLongPeriodContext[F])) = {
      
    val eaState = solveKeplerEq(secularElemt)
    // TODO: check note from 4 Jan 2016
    val spnSecularContext = sgpelems2SpecialPolarNodal(eaState, secularElemt, wgs)
    
    // secular state at time t in Lara Non Singular variables
    val sect = specialPolarNodal2LaraNonSingular(spnSecularContext)    
    val lppt = lppCorrections(sect, spnSecularContext._2)   
    val sppt = sppCorrections(lppt)
    (sppt, lppt)
  }
    
  def lppCorrectionsOld(lnSingular: LaraNonSingular[F], aux: AuxVariables[F])(implicit ev: Field[F])
      : (LaraNonSingular[F], LaraLongPeriodContext[F]) = {
    import lnSingular._,wgs.`J3/J2`
    import aux.p
    //import sec.ctx0._
    val ϵ3 = `J3/J2`/p/2
    val `p/r` = p/r
    val δψ = 2 * ϵ3 * χ 
    val δξ = χ * δψ
    val δχ = - ξ * δψ
    val δr = ϵ3 * ξ * p
    val δR = ϵ3 * (Θ/r) * `p/r` * χ
    val δΘ = ϵ3 * Θ * ((`p/r` - 1) * ξ - p*R*χ/Θ)
    
        // recalculate the "state" variables here
    // val a = rl /(1 - ecosE) 
    val Θl = δΘ+Θ
    val Rl = R+δR
    val rl = r+δr
    val pl = Θl*Θl // MU=1
    (LaraNonSingular(ψ+δψ,ξ+δξ,χ+δχ,rl,Rl,Θl), LaraLongPeriodContext(0.as[F], pl, 0.as[F], 0.as[F], 0.as[F], 0.as[F]))
  }
    
  // This implementation includes more terms with respect lppCorrectionsOld
  def lppCorrections(lnSingular: LaraNonSingular[F], aux: AuxVariables[F])(implicit ev: Field[F])
      : (LaraNonSingular[F], LaraLongPeriodContext[F]) = {
    import lnSingular._,wgs.`J3/J2`
    import aux.{p,σ,κ,c,s}

    val `χ²` = χ*χ
    val `ξ²` = ξ*ξ   
    val  ϵ3  = `J3/J2`/p/2
    val `c²` = c*c
    
    val δψ =  ϵ3 * (2*χ + (κ*χ - c*ξ*σ)/(1+c))
    val δξ =  ϵ3 * (2*`χ²` + κ*(1 - `ξ²`))
    val δχ = -ϵ3 * (`c²` * σ + (2 + κ)*χ*ξ)
    val δr =  ϵ3 * p * ξ
    val δR =  ϵ3 * (Θ/r) * (1 + κ) * χ
    val δΘ =  ϵ3 * Θ * (κ*ξ - σ*χ)
    
        // recalculate the "state" variables here
    // val a = rl /(1 - ecosE) 
    val Θl = δΘ+Θ
    val Rl = R+δR
    val rl = r+δr
    val pl = Θl*Θl // MU=1
    (LaraNonSingular(ψ+δψ,ξ+δξ,χ+δχ,rl,Rl,Θl), LaraLongPeriodContext(0.as[F], pl, 0.as[F], 0.as[F], 0.as[F], 0.as[F]))
  }
  
  def sppCorrections(lppState: (LaraNonSingular[F], LaraLongPeriodContext[F]))(implicit ev: Field[F]) 
    : LaraNonSingular[F] = {
    import lppState.{_1=>lns,_2=>lctx}
    import wgs.J2, inclCtx.{c,`c²`,s},lctx.{pl=>p}, lns._
    val ϵ2 = -J2/p/p/4
    
    val `χ²` = χ*χ
    val `ξ²` = ξ*ξ
    val δψ = -ϵ2 * ((1+7*c)/(1+c)) * ξ * χ 
    val δξ = -ϵ2 * (`χ²` - 3*`c²`) * ξ
    val δχ = -ϵ2 * (`ξ²` - 3*`c²`) * χ
    val δr =  ϵ2 * r * (`ξ²` - `χ²` - 3 + 9*`c²`)
    val δR =  ϵ2 * 4 * (Θ/r) * ξ * χ
    val δΘ =  ϵ2 * 3 * Θ * (`ξ²` - `χ²`)
    LaraNonSingular(ψ+δψ,ξ+δξ,χ+δχ,r+δr,R+δR,Θ+δΘ)
  }
  
}
*/