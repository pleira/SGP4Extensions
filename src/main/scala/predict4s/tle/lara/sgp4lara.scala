package predict4s.tle.lara

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._
import predict4s.tle.GeoPotentialCoefs
import predict4s.tle._
import TEME._   
import predict4s.tle.LaneCoefs


class SGP4Lara[F : Field : NRoot : Order : Trig](
    elem0: SGPElems[F],
    wgs: SGPConstants[F],
    ctx0: Context0[F],
    geoPot: GeoPotentialCoefs[F],
    gctx: GeoPotentialContext[F],
    laneCoefs : LaneCoefs[F],
    secularTerms : (SecularFrequencies[F], DragSecularCoefs[F]),
    isImpacting: Boolean,
    rp: F
  ) extends SGP4(elem0, wgs, ctx0, geoPot, gctx, laneCoefs, secularTerms, isImpacting, rp) {
  
  override def periodicCorrections(secularElemt : SGPElems[F], secularDragCoefs: DragSecularCoefs[F])
      : (ShortPeriodPolarNodalContext, LongPeriodPolarNodalContext, EccentricAnomalyState) = {
    val eaState = solveKeplerEq(secularElemt)
    val secPNt = lydanne2SpecialPolarNodal(eaState, secularElemt)
    // secular state at time t in Lara Non Singular variables
    val sect = polarNodal2LaraNonSingular(ctx0.s, secPNt)  // (sinI, secularPolarNodal)
    val lppc = lppCorrections(sect)
    // apply long period periodic corrections at time t
    val lppt = LaraNonSingular(sect.ψ + lppc.ψ,sect.ξ + lppc.ξ,sect.χ + lppc.χ, 
         sect.r + lppc.r, sect.R + lppc.R, sect.Θ + lppc.Θ)
    val sppc = sppCorrections(lppt)
    // apply short period periodic corrections at time t
    val sppt = LaraNonSingular(lppt.ψ + sppc.ψ, lppt.ξ + sppc.ξ, lppt.χ + sppc.χ, 
         lppt.r + sppc.r, lppt.R + sppc.R, lppt.Θ + sppc.Θ)
    // final state in Polar Nodal coordinates at time t         
    val sppPNt : PolarNodalElems[F] = laraNonSingular2PolarNodal(sppt) 
    
    // conversions for comparison with Vallado's results
    //   case class ShortPeriodPolarNodalContext(I: F, su: F, Ω: F, mrt: F, mvt: F, rvdot: F, δI: F, δsu: F, δΩ: F, δrdot: F, δrvdot: F) {
    //   case class LongPeriodPolarNodalContext(r : F, θ : F, R : F, `Θ/r` : F, `el²`: F,  pl : F, βl : F, sin2u: F, cos2u: F)
    val spPNC : ShortPeriodPolarNodalContext = ???
      // ShortPeriodPolarNodalContext()
    val lpPNC :  LongPeriodPolarNodalContext = ???
      // LongPeriodPolarNodalContext()
    (spPNC, lpPNC, eaState)
  }
  
  def periodicCorrectionsBis(secularElemt : SGPElems[F], secularDragCoefs: DragSecularCoefs[F])
      : (PolarNodalElems[F], LaraNonSingular, LaraNonSingular, EccentricAnomalyState) = {
    val eaState = solveKeplerEq(secularElemt)
    val secPNt = lydanne2SpecialPolarNodal(eaState, secularElemt)
    // secular state at time t in Lara Non Singular variables
    val sect = polarNodal2LaraNonSingular(ctx0.s, secPNt)  // (sinI, secularPolarNodal)
    val lppc = lppCorrections(sect)
    // apply long period periodic corrections at time t
    val lppt = LaraNonSingular(sect.ψ + lppc.ψ,sect.ξ + lppc.ξ,sect.χ + lppc.χ, 
         sect.r + lppc.r,sect.R + lppc.R,sect.Θ + lppc.Θ)
    val sppc = sppCorrections(lppt)
    // apply short period periodic corrections at time t
    val finalState = LaraNonSingular(lppt.ψ + sppc.ψ,lppt.ξ + sppc.ξ,lppt.χ + sppc.χ, 
         lppt.r + sppc.r,lppt.R + sppc.R,lppt.Θ + sppc.Θ)
    val finalPolarNodal : PolarNodalElems[F] = laraNonSingular2PolarNodal(finalState) 
     (finalPolarNodal, sppc, lppc, eaState)   
  }
  
  case class LaraNonSingular(ψ : F, ξ: F, χ: F, r: F, R: F, Θ: F)
  // TODO: this class is identical to LongPeriodPolarNodalContext
  case class SpecialPolarNodalContext(
      r : F, // the radial distance 
      θ : F, 
      R : F, // radial velocity 
      `Θ/r` : F, // the total angular momentum/r
      `el²`: F,   // ---- Context 
      pl : F, 
      βl : F,
      sin2u: F, 
      cos2u: F
  ) {
    def su0 = θ; def rdot0 = R; def rvdot0 = `Θ/r`;  
  }
  
  def lppCorrections(lnSingular: LaraNonSingular) : LaraNonSingular = {
    import lnSingular._
    import ctx0._
    val `p/r` = p/r
    val δψ = 2 * ϵ3 * χ 
    val δξ = χ * δψ
    val δχ = - ξ * δψ
    val δr = ϵ3 * ξ * p
    val δR = ϵ3 * (Θ/r) * `p/r` * χ
    val δΘ = ϵ3 * Θ * ((`p/r` - 1) * ξ - p*R*χ/Θ)
    LaraNonSingular(δψ,δξ,δχ,δr,δR,δΘ)
  }
  
  def sppCorrections(lnSingular: LaraNonSingular) : LaraNonSingular = {
    import lnSingular._
    import ctx0._
    val `χ²` : F = χ**2
    val `ξ²` : F = ξ**2
    
    val `∆ψ` : F  = - ϵ2 * ((1+7*c)/(1+c)) * ξ * χ 
    val `∆ξ` : F  = - ϵ2 * (`χ²` - 3 * `c²`) * ξ
    val `∆χ` : F  = - ϵ2 * (`ξ²` - 3 * `c²`) * χ
    val `∆r` : F  = ϵ2 * r * (`ξ²` - `χ²` - 3 + 9 * `c²`)
    val `∆R` : F  = ϵ2 * 4 * (Θ/r) * ξ * χ
    val `∆Θ` : F  = ϵ2 * 3 * Θ * (`ξ²` - `χ²`)
    LaraNonSingular(`∆ψ`,`∆ξ`,`∆χ`,`∆r`,`∆R`,`∆Θ`)
  }
  
  /**
   * Solve the Kepler equation 
   * 		ℓ = E - e sinE
   * where E is the eccentric anomaly.
   * We are using Lydanne's elements as variables.
   */
  def solveKeplerEq(elem : SGPElems[F]): EccentricAnomalyState = {
       
    import elem.{e,Ω,ω,M,a}, wgs.twopi
     
    //---------------------------------------------------------------------------------------
    // CONFIRM
    //---------------------------------------------------------------------------------------
    
    // Lydanne's Transformation
    val axn : F = e * cos(ω)
    val temp : F = 1 / (a * (1 - e * e))
    
    import secularTerms._2._ // dragSecularCoefs._  
    val ayn : F = e * sin(ω) + temp * aycof
    val xl : F  = M + ω + Ω + temp * xlcof * axn
     
    //---------------------------------------------------------------------------------------
    
    // Ω is the ascending node, E is the eccentric anomaly, and e is the eccentricity.
    var ktr : Int = 1
    // U = F' - h' = M" + g"; 
    val u    = Field[F].mod(xl - Ω, twopi.as[F])
    var eo1  = u
    var tem5 : F = 9999.9.as[F]     //   sgp4fix for kepler iteration
    var ecosE : F = 0.as[F]
    var esinE : F = 0.as[F]
    var coseo1 : F = 0.as[F]
    var sineo1 : F = 0.as[F]

    //   the following iteration needs better limits on corrections
    while ((abs(tem5) >= 1e-12.as[F]) && (ktr <= 10)) {
       sineo1 = sin(eo1)
       coseo1 = cos(eo1)
       ecosE = axn * coseo1 + ayn * sineo1
       esinE = axn * sineo1 - ayn * coseo1

       val fdot = 1 - ecosE
       val f = (u + esinE - eo1)
       tem5 = f / fdot  // delta value
       if(abs(tem5) >= 0.95.as[F])
           tem5 = if (tem5 > 0.as[F]) 0.95.as[F]  else -0.95.as[F] 
       eo1 = eo1 + tem5
       ktr = ktr + 1
     }
     
     EccentricAnomalyState(eo1,coseo1,sineo1,ecosE,esinE)   
  }
  
  def laraNonSingular2PolarNodal(lnSingular: LaraNonSingular) : TEME.PolarNodalElems[F] = {
	  import lnSingular._
	  val `ξ²` : F = ξ**2
	  val `χ²` : F = χ**2
	  val `R/r` : F = R/r
	  val `Θ/r` : F = Θ/r
	  val N : F = ???
	  val c : F = N/Θ
	  val cosψ = cos(ψ)
	  val sinψ = sin(ψ)
	  val q = ξ * χ / (1 + c)
	  val τ = 1 - `χ²` / (1 + c)
	  val b = 1 - `ξ²` / (1 + c)
	  val θ : F = ???
	  val ν : F = ??? 
    TEME.PolarNodalElems(r,θ,ν,R,Θ,N)
 }
  
  def lydanne2SpecialPolarNodal(eaState: EccentricAnomalyState, secularElem: SGPElems[F]) = {
    import eaState._ 
    import secularElem._ // {n,e,I,ω,Ω,M,a}

    // It follows the usual transformation to polar-nodal variables
    // (r, θ, R, Θ) −→ (F, C, S, a)  with C' = e'cosg and  S' = e'sing
    // Note: Vallado's SGP4 uses rθdot = Θ/r instead of Θ

    val `e²` : F = e**2
    val p = a*(1 - `e²`)  // semilatus rectum , as MU=1, p=Z²
    if (p < 0.as[F]) throw new Exception("p: " + p)
      
    val r     = a * (1 - ecosE)          // r´        
    val rdot  = sqrt(a) * esinE/r       // R´
    val rvdot = sqrt(p) / r            // Θ’/r’ that is Θ/r 
    val βl     = sqrt(1 - `e²`)          // y’
    val temp0  = esinE / (1 + βl)         
     
    // u is the true anomaly that can be defined immediately as the polar angle θ = (Ox, OS), x along the semimajor axis, S sat position
    val sinu : F = ??? // FIXME ?a / r * (sineo1 - aynl - axnl * temp0)             // sinu
    val cosu : F = ??? // FIXME a / r * (coseo1 - axnl + aynl * temp0)             // cosu
    val su0 : F  = ??? // atan2(sinu, cosu)                                   // u, that is θ
    val sin2u : F = ??? // 2 * cosu * sinu
    val cos2u : F = ??? // 1 - 2 * sinu * sinu
    SpecialPolarNodalContext(r, su0, rdot, rvdot, `e²`, p, βl, sin2u, cos2u)
  }
  
  // Sec 4.2 Lara's personal communication (r, θ, R, Θ) −→ (F, C, S, a)
  def lyddane2PolarNodal(elem: SGPElems[F], eas : EccentricAnomalyState): TEME.PolarNodalElems[F] = {
    import eas._
    import elem._
    import wgs.μ
    val `e²` : F = e**2
    val β = (1 - `e²`).sqrt
    val L = (μ*a).sqrt
    val r = a*(1 - ecosE)
    val R = (L/r) * esinE 
    val Θ = L * β
    // val sinf = 
    val f : F = ??? // atan(β*esinE/(ecosE - `e²`))
    val g : F =  ???
	  val θ : F = f + g 
	  val ν : F = ??? 
 	  val N : F = ??? 
    TEME.PolarNodalElems(r,θ,ν,R,Θ,N)
  }

  def polarNodal2LaraNonSingular(s: SinI, polarNodal: SpecialPolarNodalContext) : LaraNonSingular = {
    import polarNodal._ 
    import elem0.{Ω=>ν} // FIXME: TBC
    val ψ = ν + θ
    val ξ = s * sin(θ)
    val χ = s * cos(θ)
    val Θ = `Θ/r`*r
    LaraNonSingular(ψ, ξ, χ, r, R, Θ) 
  }
  
  def cartesian2LaraNonSingular(pv: TEME.CartesianElems[F]) : LaraNonSingular = {
    import pv._
    // (x,y,z) position, (X,Y,Z) velocity
    val r : F = (x**2 + y**2 + z**2).sqrt 
    val R : F = (x*X + y*Y + z*Z)/r
    val N : F = x*X - y*Y
    val Θ : F = ((y*Z - z*Y)**2 + (z*X - x*Z)**2 + N**2).sqrt
    val χ : F = (r*Z - z*R)/Θ
    val ξ : F =  z/r
    val c = N/Θ
    val `χ²` : F = χ**2
    val q = ξ * χ / (1 + c)
    val τ : F = 1 - `χ²` / (1 + c)
    // val `q²+τ²` = q*q + τ*τ 
//    val sinψ : F = (x*q + y*τ)/`q²+τ²`/r
//    val cosψ : F = (x*τ - y*q)/`q²+τ²`/r
    val tanψ : F = (x*q + y*τ)/(x*τ - y*q)
    val ψ : F = atan(tanψ)     
    LaraNonSingular(ψ, ξ, χ, r, R, Θ)
  }
  
  def laraNonSingular2Cartesian(lnSingular: LaraNonSingular) : CartesianElems[F] = {
    import lnSingular._
    val `ξ²` : F = ξ**2
    val `χ²` : F = χ**2
    val `R/r` : F = R/r
    val `Θ/r` : F = Θ/r
    val N : F = ???
    val c : F = N/Θ
    val cosψ = cos(ψ)
    val sinψ = sin(ψ)
    val q = ξ * χ / (1 + c)
    val τ = 1 - `χ²` / (1 + c)
    val b = 1 - `ξ²` / (1 + c)

    val ux = (b * cosψ + q * sinψ)
    val uy = (b * sinψ - q * cosψ)
    val uz = ξ

    CartesianElems(
      r * ux,
      r * uy,
      r * uz,
      R * ux - `Θ/r` * (q * cosψ + τ * sinψ),
      R * uy - `Θ/r` * (q * sinψ - τ * cosψ),
      R * uz + `Θ/r` * χ)
    }

}

object SGP4Lara extends SGP4Factory {
  
  def apply[F : Field : NRoot : Order : Trig](elem0Ctx0: (SGPElems[F], Context0[F]))(implicit wgs0: SGPConstants[F]) :  SGP4Lara[F] = {
    val (elem, wgs, ctx0, geoPot, gctx, laneCoefs, secularFreqs, isImpacting, rp) = from(elem0Ctx0)
    new SGP4Lara(elem, wgs, ctx0, geoPot, gctx, laneCoefs, secularFreqs, isImpacting, rp)
  }
  
}

