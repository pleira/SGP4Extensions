package predict4s.tle.lara

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._
import predict4s.tle.GeoPotentialCoefs
import predict4s.tle._
import predict4s.tle.LaneCoefs
import predict4s.coord.PolarNodalElems
import predict4s.coord.CartesianElems
import predict4s.coord.SpecialPolarNodal
import predict4s.coord.CoordTransformation._

class SGP4Lara[F : Field : NRoot : Order : Trig](
 sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4(sec) {
 
  type FinalState = PolarNodalElems[F]
  type ShortPeriodState = LaraNonSingular
  type LongPeriodState = LaraNonSingular
  type EccentricAState = EccentricAnomalyState
  type PolarNodalSecularState = (SpecialPolarNodal[F], F, F, F, F, F)
//      r : F, // the radial distance 
//      θ : F, 
//      R : F, // radial velocity 
//      `Θ/r` : F, // the total angular momentum/r
//      `el²`: F,   // ---- Context 
//      pl : F, 
//      βl : F,
//      sin2u: F, 
//      cos2u: F
//  ) {
//    def su0 = θ; def rdot0 = R; def rvdot0 = `Θ/r`;  
//  }
  
  override def periodicCorrections(secularElemt : SGPElems[F])
      :  (FinalState, ShortPeriodState, LongPeriodState, EccentricAState) = {
    
    // After computing the double-prime Delaunay/Lyddane variables from the secular terms, 
    // the Kepler equation must be solved to find first ƒ, the true anomaly, and then θ, 
    // the argument of latitude, in order to compute corresponding double-prime polar-nodal variables.
    val eaState = solveKeplerEq(secularElemt)

    val (secPNt, _,_,_,_,_)  = lyddane2SpecialPolarNodal(eaState, secularElemt)
    
    // secular state at time t in Lara Non Singular variables
    val sect = specialPolarNodal2LaraNonSingular(sec.ctx0.s, secPNt)  // (sinI, secularPolarNodal)
    
    val lppc = lppCorrections(sect)
    
    // apply long period periodic corrections at time t
    val lppt = sect + lppc 
    
    val sppc = sppCorrections(lppt)
    // apply short period periodic corrections at time t
    val sppt = lppt + sppc
    
    // final state in Polar Nodal coordinates at time t         
    val finalPolarNodalt : PolarNodalElems[F] = laraNonSingular2PolarNodal(sppt) 
    
    (finalPolarNodalt, sppc, lppc, eaState)
  }
 
  override def propagate2CartesianContext(t: Minutes) = {
    val ((finalPolarNodal, sppState, lppState, eaState), secularElemt) = propagate2PolarNodalContext(t)
    import finalPolarNodal._
    // FIXME
    val uPV: CartesianElems[F] = polarNodal2UnitCartesian(θ, R, ν)
    val mrt = r ; val mvt = Θ; val rvdot = N // relations not verified, just to get this class compiled 
    val (p, v) = convertAndScale2UnitVectors(uPV.pos, uPV.vel, mrt, mvt, rvdot)
    val posVel = CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
    (posVel, uPV, finalPolarNodal, sppState, lppState, eaState)    
  }
  
  case class LaraNonSingular(ψ : F, ξ: F, χ: F, r: F, R: F, Θ: F) {
    def +(o: LaraNonSingular) = LaraNonSingular(ψ + o.ψ,ξ+ o.ξ,χ+ o.χ,r+ o.r,R+ o.R,Θ+ o.Θ)
  }
  
  def lppCorrections(lnSingular: LaraNonSingular) : LaraNonSingular = {
    import lnSingular._
    import sec.ctx0._
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
    import sec.ctx0._
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
   * Solve the Kepler equation φ = ƒ − ℓ = ƒ − u + e sinu   where 
   * u = 2 arctan (sqrt((1-e)/(1+e))*tan(ƒ/2))
   * e² = κ² + σ²
   * ƒ is unambiguously computed from cosƒ = κ/e and  sinƒ = σ/e
   * with κ = p/r - 1 and σ = p*R/Θ
   * which are derived from 
   * R = (G/p) e sinƒ
   * r = p / (1 + e cosƒ)
   */
  def solveKeplerEq(elem : SGPElems[F]): EccentricAnomalyState = {
       
    import elem.{e,Ω,ω,M,a}, sec.wgs.twopi
     
    //---------------------------------------------------------------------------------------
    // CONFIRM
    //---------------------------------------------------------------------------------------
    
    // Lydanne's Transformation
    val axn : F = e * cos(ω)
    val temp : F = 1 / (a * (1 - e * e))
    
    import sec.dragCoefs._  
    val ayn : F = e * sin(ω) + temp * aycof
    val xl : F  = M + ω + Ω + temp * xlcof * axn
     
    //---------------------------------------------------------------------------------------
    
    // Ω is the ascending node, E is the eccentric anomaly, and e is the eccentricity.
    var ktr : Int = 1
    // U = F' - h' = M" + g"; 
    val u    = Field[F].mod(xl - Ω, twopi.as[F])
    var E  = u
    var tem5 : F = 9999.9.as[F]     //   sgp4fix for kepler iteration
    var ecosE : F = 0.as[F]
    var esinE : F = 0.as[F]
    var cosE : F = 0.as[F]
    var sinE : F = 0.as[F]

    //   the following iteration needs better limits on corrections
    while ((abs(tem5) >= 1e-12.as[F]) && (ktr <= 10)) {
       sinE = sin(E)
       cosE = cos(E)
       ecosE = axn * cosE + ayn * sinE
       esinE = axn * sinE - ayn * cosE

       val fdot = 1 - ecosE
       val f = (u + esinE - E)
       tem5 = f / fdot  // delta value
       if(abs(tem5) >= 0.95.as[F])
           tem5 = if (tem5 > 0.as[F]) 0.95.as[F]  else -0.95.as[F] 
       E = E + tem5
       ktr = ktr + 1
     }
     
     EccentricAnomalyState(E,cosE,sinE,ecosE,esinE)   
  }
  
  // ν = ψ − θ and sinθ = ξ/s, cosθ = χ/s and c = N/Θ, tanθ = ξ/χ
  def laraNonSingular2PolarNodal(lnSingular: LaraNonSingular) : PolarNodalElems[F] = {
	  import lnSingular._
	  val N : F = Θ*cos(sec.elem0.I) // FIXME
	  val θ : F = atan(ξ/χ)
	  val ν : F = ψ - θ
    PolarNodalElems(r,θ,ν,R,Θ,N)
  }
  
  def lyddane2SpecialPolarNodal(eaState: EccentricAnomalyState, secularElem: SGPElems[F]) 
      : PolarNodalSecularState = {
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
    val sinu : F = sinE // FIXME ?a / r * (sinE - aynl - axnl * temp0)             // sinu
    val cosu : F = cosE // FIXME a / r * (cosE - axnl + aynl * temp0)             // cosu
    val su0 : F  = E    // FIXME atan2(sinu, cosu)                                   // u, that is θ
    val sin2u : F = 2 * cosu * sinu
    val cos2u : F = 1 - 2 * sinu * sinu
    // SpecialPolarNodalContext(r, su0, rdot, rvdot, `e²`, p, βl, sin2u, cos2u)
    (SpecialPolarNodal(I, su0, Ω, r, rdot, rvdot), `e²`, p, βl, sin2u, cos2u)
  }
  
  // Sec 4.2 Lara's personal communication (r, θ, R, Θ) −→ (F, C, S, a)
  def lyddane2PolarNodal(elem: SGPElems[F], eas : EccentricAnomalyState): PolarNodalElems[F] = {
    import eas._
    import elem._
    import sec.wgs.μ
    val `e²` : F = e**2
    val β = sqrt(1 - `e²`)
    val L = sqrt(μ*a)
    val r = a*(1 - ecosE)
    val R = (L/r) * esinE 
    val Θ = L * β
    val `tanf/2` = sqrt((1+e)/(1-e))*tan(E/2)
    val f : F = 2*atan(`tanf/2`)
    val g : F = ω
	  val θ : F = f + g 
	  val ν : F = Ω   // FIXME ν is not defined if I=0 , 
 	  val N : F = Θ*cos(I) 
    PolarNodalElems(r,θ,ν,R,Θ,N)
  }

  def specialPolarNodal2LaraNonSingular(s: SinI, polarNodal: SpecialPolarNodal[F]) : LaraNonSingular = {
    import polarNodal._ 
    import sec.elem0.{Ω=>ν,I=>θ,ω=>R} // FIXME: just wrong
    val ψ = ν + θ
    val ξ = s * sin(θ)
    val χ = s * cos(θ)
    val Θ =  rvdot  // `Θ/r`*r  check
    LaraNonSingular(ψ, ξ, χ, r, R, Θ) 
  }
  
  def polarNodal2LaraNonSingular(s: SinI, polarNodal: PolarNodalElems[F]) : LaraNonSingular = {
    import polarNodal._ 
    import sec.elem0.{Ω=>ν} // FIXME: TBC
    val ψ = ν + θ
    val ξ = s * sin(θ)
    val χ = s * cos(θ)
    //val Θ =  rvdot  // `Θ/r`*r  check
    LaraNonSingular(ψ, ξ, χ, r, R, Θ) 
  }
  
  def cartesian2LaraNonSingular(pv: CartesianElems[F]) : LaraNonSingular = {
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
    val N : F = Θ*cos(sec.elem0.I) // FIXME
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

object SGP4Lara {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) :  SGP4Lara[F] = new SGP4Lara(sec)

}

