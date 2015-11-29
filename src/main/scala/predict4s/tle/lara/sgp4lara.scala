package predict4s.tle.lara

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._
import predict4s.tle.GeoPotentialCoefs
import predict4s.tle.OrbitalState
import predict4s.tle._
import TEME._   
import predict4s.tle.LaneCoefs

case class SGP4State[F](orbitalState: OrbitalState[F], uPV: TEME.CartesianElems[F]) 

class SGP4Lara[F : Field : NRoot : Order : Trig](
    val elem0: TEME.SGPElems[F],
    val wgs: SGPConstants[F],
    val geoPot: GeoPotentialCoefs[F],
    val laneCoefs : LaneCoefs[F],
    val otherCoefs : OtherCoefs[F],
    val isImpacting: Boolean
  )  {
  
  type SinI = F  // type to remember dealing with the sine   of the Inclination 
  type CosI = F  // type to remember dealing with the cosine of the Inclination 
  type Minutes = F // type to remember dealing with minutes from epoch
 
  val eValidInterval = Interval.open(0.as[F],1.as[F])
   
  import elem0._, wgs._
  val `e²` : F = e**2
  val s : SinI = sin(I)
  val c : CosI = cos(I)
  val `c²` : CosI = c**2
  val `s²` : SinI = s**2
  val p : F = a * (1 - `e²`)            // semilatus rectum , which also is G²/μ, with G as the Delauney's action, the total angular momentum
  val `α/p` : F = α/p
  val ϵ2 : F = -J2*(`α/p`**2) / 4
  val ϵ3 : F = (`J2/J3`)*`α/p` / 2      // or (`C30/C20`)*`α/p` / 2 
  val η : F = (1 - `e²`).sqrt           // eccentricity function G/L, with G as the Delauney's action, the total angular momentum , and L = √(μ a)
 
  def propagate(t: Minutes) : SGP4State[F] = {
    val secularElemt = secularCorrections(t)
    val eaState = solveKeplerEq(secularElemt)
    val secularPolarNodal = delauney2PolarNodal(secularElemt, eaState)
    val secularLaraNonSingular = polarNodal2LaraNonSingular(s, secularPolarNodal)
    val lppState = lppCorrections(secularLaraNonSingular)
    val sppState = sppCorrections(s, c, `c²`, secularLaraNonSingular)
    val finalState = secularLaraNonSingular // FIXME apply really the corrections + lppState + sppState
    val finalPolarNodal : PolarNodalElems[F] = laraNonSingular2PolarNodal(finalState) 
      
    // unit position and velocity 
    import finalPolarNodal._
    val I: F = ???; val R: F = ???; val Ω: F = ???;  val mrt: F = ???; val mvt: F = ???; val rvdot: F = ???
    val uPV: TEME.CartesianElems[F] = TEME.polarNodal2UnitCartesian(I, R, Ω)
    
    // return position and velocity (in km and km/sec)
    val (p, v) = convertUnitVectors(uPV.pos, uPV.vel, mrt, mvt, rvdot)
    val posVel = TEME.CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
    val orbitalState = OrbitalState(t, posVel)
    SGP4State(orbitalState, uPV)
  }

  /**
   * Lara's code works with internal units of length LU (units of earth’s radius  
   * R⊕ in km) and time TU (units of the orbit’s period in min) 
   * TU = 60 * sqrt( (R⊕ km)³ /(μ km³ /s² ) ) min
   * where μ is the earth’s gravitational constant; μ = 1 UL³/UT² in internal units.    
   */
  def convertUnitVectors(pos : Vector[F], vel : Vector[F], mrt: F, mvt: F, rvdot: F)
      : (Vector[F], Vector[F]) = {
      import wgs._
      ( (aE*mrt) *: pos,  vkmpersec *: (mvt *: pos + rvdot *: vel))
  }  

  case class LaraNonSingular(ψ : F, ξ: F, χ: F, r: F, R: F, Θ: F)
  case class EccentricAnomalyState(eo1 : F, coseo1: F, sineo1: F, ecosE: F, esinE: F)  

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
  
  // Sec 4.2 Lara's personal communication (r, θ, R, Θ) −→ (F, C, S, a)
  def delauney2PolarNodal(elem: TEME.SGPElems[F], eas : EccentricAnomalyState) : TEME.PolarNodalElems[F] = {
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
    val f : F = atan(β*esinE/(ecosE - `e²`))
    val g : F =  ???
	  val θ : F = f + g 
	  val ν : F = ??? 
 	  val N : F = ??? 
    TEME.PolarNodalElems(r,θ,ν,R,Θ,N)
  }


  def polarNodal2LaraNonSingular(s: SinI, polarNodal: TEME.PolarNodalElems[F]) : LaraNonSingular = {
    import polarNodal._ 
    val ψ = ν + θ
    val ξ = s * sin(θ)
    val χ = s * cos(θ)
    LaraNonSingular(ψ, ξ, χ, r, R, Θ) 
  }
  
  def cartesian2LaraNonSingular(pv: TEME.CartesianElems[F]) : LaraNonSingular = {
    import pv._
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
  
  /** 
   *  This calculation updates the secular elements at epoch to the desired date given by 
   *  the time t in minutes from the epoch 
   */
  def secularCorrections(t: Minutes): TEME.SGPElems[F] = {
    
    import otherCoefs.{ωdot,Ωdot,mdot=>Mdot,Ωcof}
    
    // Gravity corrections
    // Brouwer’s gravitational corrections are applied first
    // here we are with Delaunays variables, 
    // ωdot is gdot, Mdot is ℓdot, and  Ωdot is hdot.
    val `t²` : F = t**2    
    val ωdf  : F = ω + ωdot*t
    val Ωdf  : F = Ω + Ωdot*t
    val Mdf  : F = M + Mdot*t
    
    // Next, the secular corrections due to the atmospheric drag are incorporated;
    // in particular δh, δL, δe, δℓ (in Delaunay's)
   
    val (tempa,tempe,templ,ωm, mp) : (F,F,F,F,F) = tempTerms(t: Minutes, ωdf, Mdf)

    // Compute the secular elements (not exactly secular: they mix long-period terms from drag)

    val am : F  = ((KE/n) fpow (2.0/3.0).as[F]) * tempa*tempa // a * tempa**2  
    val nm : F  = KE / (am pow 1.5)
    val em_ : F = e - tempe
    val Ωm  : F = Ωdf + Ωcof*`t²` 
    
    // fix tolerance for error recognition
    // sgp4fix am is fixed from the previous nm check
    if (!eValidInterval.contains(em_))
      {
        // sgp4fix to return if there is an error in eccentricity
        // FIXME: we should move to use Either
        return TEME.SGPElems(nm, em_, I, ωm, Ωm, mp, am, bStar, epoch) 
      }

    // sgp4fix fix tolerance to avoid a divide by zero
    // TBC:  is this needed in Lara's version
    val em = if (em_ < 1.0e-6.as[F]) 1.0e-6.as[F] else em_ 
    
    val Mm_  = mp + n*templ
     
    // modulus so that the angles are in the range 0,2pi
    val Ω_      = Ωm  % twopi
    val ω_      = ωm  % twopi
    
    // Lyddane's variables and back 
    val ℓm      = Mm_ + ωm + Ωm
    val lm      = ℓm  % twopi
    val Mm      = (lm - ω_ - Ω_) % twopi   
    TEME.SGPElems(nm, em, I, ω_, Ω_, Mm, am, bStar, epoch)
  }

  def tempTerms(t: Minutes, ωdf: F, Mdf: F) : (F,F,F,F,F) = {

    import laneCoefs._
    import otherCoefs.{ωcof,delM0,sinM0,Mcof}    
    import geoPot._        
    val `t²` : F = t**2    
 
    // It should be noted that when epoch perigee height is less than
    // 220 kilometers, the equations for a and Lane's are truncated after the C1 term, 
    // and the terms involving C5 , δω, and δM are dropped.    
    if (isImpacting) 
      return (1 - C1*t, bStar*C4*t, t2cof*`t²`, ωdf, Mdf)
    
    val `t³` = `t²`*t
    val `t⁴` = `t²`*`t²`
    val δω : F = ωcof*t
    val δM : F = Mcof*( (1+η*cos(Mdf))**3 - delM0)
    val Mpm_ : F = Mdf + δω + δM
    val ωm_  : F = ωdf - δω - δM
       
    (1 - C1*t - D2 * `t²` - D3 * `t³` - D4 * `t⁴`, 
     bStar*(C4*t + C5*(sin(Mpm_) - sinM0)), 
     t2cof*`t²` + t3cof*`t³` + `t⁴` * (t4cof + t*t5cof),
     ωm_, 
     Mpm_)
  }
  
  /**
   * Solve the Kepler equation 
   * 		ℓ = E - e sinE
   * where E is the eccentric anomaly.
   * We are using Delauney's elements as variables.
   */
  def solveKeplerEq(elem : TEME.SGPElems[F]): EccentricAnomalyState = {
       
    import elem.{e,Ω,ω,M,a}, wgs.twopi
     
    //---------------------------------------------------------------------------------------
    // CONFIRM
    //---------------------------------------------------------------------------------------
    
    // TBC : Delaunay Transformation
    val axnl : F = e * cos(ω)
    val temp : F = 1 / (a * (1 - e * e))
    
    // TBC: is here LPPE added? should aycof and xlcof go away here?
    import otherCoefs.{aycof,xlcof}
    val aynl : F = e * sin(ω) + temp * aycof
    val xl : F  = M + ω + Ω + temp * xlcof * axnl
     
    //---------------------------------------------------------------------------------------
    
    /* --------------------- solve kepler's equation  M = E - e sin E     --------------- */
    // Nodep (or M) is the mean anomaly, E is the eccentric anomaly, and e is the eccentricity.
    var ktr : Int = 1
    val u    = Field[F].mod(xl - Ω, twopi.as[F])
    var eo1  = u
    var tem5 : F = 9999.9.as[F]     //   sgp4fix for kepler iteration
    var ecosE : F = 0.as[F]
    var esinE : F = 0.as[F]
    var coseo1 : F = 0.as[F]
    var sineo1 : F = 0.as[F]
     
    //   the following iteration needs better limits on corrections
    while ((abs(tem5) >= 1e-12.as[F]) && (ktr <= 10))
     {
       sineo1 = sin(eo1)
       coseo1 = cos(eo1)
       ecosE = axnl * coseo1 + aynl * sineo1
       esinE = axnl * sineo1 - aynl * coseo1

       val fdot   = 1 - ecosE
       val f = (u + esinE - eo1)
       tem5   = f / fdot  // delta value
       if(abs(tem5) >= 0.95.as[F])
           tem5 = if (tem5 > 0.as[F]) 0.95.as[F]  else -0.95.as[F] 
       eo1    = eo1 + tem5
       ktr = ktr + 1
     }
     
     EccentricAnomalyState(eo1,coseo1,sineo1,ecosE,esinE)   
  }
  
  def lppCorrections(lnSingular: LaraNonSingular) : LaraNonSingular = {
    import lnSingular._
    val `p/r` = p/r
    val δψ = 2 * ϵ3 * χ 
    val δξ = χ * δψ
    val δχ = - ξ * δψ
    val δr = ϵ3 * ξ * p
    val δR = ϵ3 * (Θ/r) * `p/r` * χ
    val δΘ = ϵ3 * Θ * ((`p/r` - 1) * ξ - p*R*χ/Θ)
    LaraNonSingular(δψ,δξ,δχ,δr,δR,δΘ)
  }
  
  def sppCorrections(s: SinI, c: CosI, `c²`: CosI, lnSingular: LaraNonSingular) : LaraNonSingular = {
    import lnSingular._
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

}

object SGP4Lara extends SGP4Factory {
  
  def apply[F : Field : NRoot : Order : Trig](elemTLE: TEME.SGPElems[F])(implicit wgs0: SGPConstants[F]) : SGP4Lara[F] = {
    val (elem, wgs, geoPot, laneCoefs, otherCoefs, isImpacting) = from(elemTLE)
    new SGP4Lara(elem, wgs, geoPot, laneCoefs, otherCoefs, isImpacting)
  }
  
}

