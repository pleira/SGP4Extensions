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


case class SGP4State[F](orbitalState: OrbitalState[F], uPV: TEME.CartesianElems[F], elem: TEME.SGPElems[F]) 
    // sppState : ShortPeriodPeriodicState[F], wgs: SGPConstants[F])


class SGP4Lara[F : Field : NRoot : Order : Trig](val secularEffects: SecularEffects[F])(implicit wgs: SGPConstants[F])  {
  import wgs._
  
  def propagate(t: F) : SGP4State[F] = {
    val eaState = NewtonRaphsonKeplerSolver.solve(secularEffects)
    
    val secularPolarNodal = delaunay2PolarNodal(eaState)
    val secularLaraNonSingular  = polarNodal2LaraNonSingular(secularPolarNodal)
    val lppState = lppCorrections(secularLaraNonSingular)
    val sppState = sppCorrections(secularLaraNonSingular)
    val finalState = secularLaraNonSingular // + lppState + sppState
    val finalPolarNodal : PolarNodalElems[F] = laraNonSingular2PolarNodal(finalState) 
      
    // unit position and velocity 
    import finalPolarNodal._
    val I: F = ???; val R: F = ???; val Ω: F = ???;  val mrt: F = ???; val mvt: F = ???; val rvdot: F = ???
    val uPV: TEME.CartesianElems[F] = TEME.polarNodal2UnitCartesian(I, R, Ω)
    
    // return position and velocity (in km and km/sec)
    val (p, v) = convertUnitVectors(uPV.pos, uPV.vel, mrt, mvt, rvdot)
    val posVel = TEME.CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
    val orbitalState = OrbitalState(t, posVel)
    SGP4State(orbitalState, uPV, ???)
  }

  /**
   * Lara's code works with internal units of length LU (units of earth’s radius  
   * R⊕ in km) and time TU (units of the orbit’s period in min) 
   * TU = 60 * sqrt( (R⊕ km)³ /(μ km³ /s² ) ) min
   * where μ is the earth’s gravitational constant; μ = 1 UL³/UT² in internal units.    
   */
  def convertUnitVectors(pos : Vector[F], vel : Vector[F], mrt: F, mvt: F, rvdot: F)(implicit wgs: SGPConstants[F])
      : (Vector[F], Vector[F]) = {
      import wgs._
      ( (aE*mrt) *: pos,  vkmpersec *: (mvt *: pos + rvdot *: vel))
  }  

 val a: F = 1.as[F] // FIXME: semi-major axis
 val I: F = 1.as[F] // FIXME: inclination
 val e: F = 1.as[F] // FIXME: eccentricity
 val s : F = sin(I)
 val c : F = cos(I)
 val `c²` : F = c**2
 val `s²` : F = s**2
 val J2oJ3 : F = J2/J3
 val α : F = ???
 val p : F = a * (1 - e*e)
 val `α/p` : F = α/p
 val ϵ2 : F = -J2*(`α/p`**2) / 4
 val ϵ3 : F = (J2oJ3)*`α/p` / 2


case class LaraNonSingular[F](ψ : F, ξ: F, χ: F, r: F, R: F, Θ: F)

  def laraNonSingular2PolarNodal(lnSingular: LaraNonSingular[F]) : PolarNodalElems[F] = {
    
    ???
  }
  
  def delaunay2PolarNodal[F](eas : EccentricAnomalyState[F]) : TEME.PolarNodalElems[F] = {
    ???
  }
  
  def polarNodal2LaraNonSingular(polarNodal: TEME.PolarNodalElems[F]) : LaraNonSingular[F] = 
  {
    import polarNodal._ 
    val ψ = ν + θ
    val ξ = s * sin(θ)
    val χ = s * cos(θ)
    LaraNonSingular(ψ, ξ, χ, r, R, Θ) 
  }
  
  def lppCorrections(lnSingular: LaraNonSingular[F]) : LaraNonSingular[F] = {
    import lnSingular._
    val `p/r` = p/r
    val δψ : F = 2.as[F] * χ * ϵ3
    val δξ = χ * δψ
    val δχ = - (ξ * δψ)
    val δr = p * ϵ3 * ξ
    val δR = (Θ/r) * ϵ3 *  `p/r` * χ
    val δΘ = Θ * ϵ3 * ( ( `p/r` - 1) * ξ - p*R*χ/Θ)
    LaraNonSingular(δψ,δξ,δχ,δr,δR,δΘ)
  }
  
  def sppCorrections(lnSingular: LaraNonSingular[F]) : LaraNonSingular[F] = {
    import lnSingular._
//        val `t³` = `t²`*t
//        val `t⁴` = `t²`*`t²`
//        val `p/r` = p/r
    val `χ²` : F = χ**2
    val `ξ²` : F = ξ**2
    
    val `∆ψ` : F  = - ϵ2 * ((1+7*c)/(1+c)) * ξ * χ 
    val `∆ξ` : F  = - ϵ2 * (`χ²` - 3 * `c²` ) * ξ
    val `∆χ` : F  = - ϵ2 * (`ξ²` - 3 * `c²` ) * χ
    val temp : F  = 3 * (1 - 3 * `c²` ) 
    val `∆r` : F  = ϵ2 * r * ( `ξ²` - `χ²` - 3 + 9 * `c²`)
    val `∆R` : F  = ϵ2 * 4 * (Θ/r) * ξ * χ
    val `∆Θ` : F  = ϵ2 * 3 * Θ * (`ξ²` - `χ²`)
    LaraNonSingular(`∆ψ`,`∆ξ`,`∆χ`,`∆r`,`∆R`,`∆Θ`)
  }

  
  def stateVectorCartesian(lnSingular: LaraNonSingular[F]) : CartesianElems[F] = {
       // After replacing ν = ψ − θ and sin θ = ξ/s, cos θ = χ/s, the
       // transformation from nonsingular to Cartesian variables can be obtained from the sequence
       // (s and c are abbreviations for the sine and cosine of the inclination)
    import lnSingular._
    val `ξ²` : F = ξ**2
    val `χ²` : F = χ**2
    val `R/r` : F = R/r
    val `Θ/r` : F = Θ/r
    val N : F = ???
    // Here is another c
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
      
  /**
   * Here we are starting with a TLE which has been transformed into the original not transformed SGP Elements
   * The method returns a SGP4 Propagator where some coeficients are calculated
   */
  def apply[F : Field : NRoot : Order : Trig](elem0: TEME.SGPElems[F])(implicit wgs: SGPConstants[F]) : SGP4Lara[F] = {
    val dpState = DpTransform.dpState(elem0)
    val geoPot  = GeoPotentialState(dpState)
    val secularEffects = SecularEffects(geoPot)
    new SGP4Lara(secularEffects)
  }

  // FIXME
//  def dpState[F: Field: Trig](tle: TLE)(implicit wgs: SGPConstants[F]) :  DpTransform.DpState[F] = 
//    DpTransform.dpState(TEME.sgpElems(tle))
//
//  def geoState[F: Field: Trig](tle: TLE)(implicit wgs: SGPConstants[F]) : GeoPotentialState[F] =
//    GeoPotentialState(dpState(tle))
  
}
