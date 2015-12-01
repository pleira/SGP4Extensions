package predict4s.tle
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._

/** 
 * The SGP-4 theory is applied for all orbits with periods of T <= 225 min. 
 * It performs a propagation in time of doubly averaged elements according to their
 * secular rates of change due to the zonal harmonics J2 and J4 of the Earth potential,
 * and due to drag perturbations in an atmosphere with a power-law altitude profile of air density. 
 * The propagated, doubly averaged elements at epoch are subsequently
 * converted into singly averaged elements, by overlaying long-periodic
 * perturbations due to J3, before a final conversion step to osculating elements by superimposition
 * of first-order, short-period perturbation amplitudes due to J2. 
 * (from Space Debris, by H. Klinkrad, pag 216).
 */
abstract class SGP4[F : Field : NRoot : Order : Trig](
    val elem0: TEME.SGPElems[F],    
    val wgs: SGPConstants[F]
    ){
  
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
  val x3thm1     = 3*`c²` - 1
  val con41      = x3thm1
  val con42      = 1 - 5*`c²`
  val x1mth2     = 1 - `c²`
  
  type SinI = F  // type to remember dealing with the sine   of the Inclination 
  type CosI = F  // type to remember dealing with the cosine of the Inclination 
  type Minutes = F // type to remember dealing with minutes from epoch
 
  def propagatePolarNodalAndContext(t: Minutes) : ((F,F,F,F,F,F), LongPeriodPeriodicState, TEME.SGPElems[F], EccentricAnomalyState)
  
  def propagatePolarNodal(t: Minutes) = {
    val (finalPolarNodal, _, _, _) = propagatePolarNodalAndContext(t)
    finalPolarNodal
  }

  def propagate2CartesianAndContext(t: Minutes) = {
    val (finalPolarNodalXX, lppState, secularElemt, eaState) = propagatePolarNodalAndContext(t)
    import finalPolarNodalXX.{_1=>I,_2=>R,_3=> Ω, _4=>mrt,_5=>mvt,_6=>rvdot}
    val uPV: TEME.CartesianElems[F] = TEME.polarNodal2UnitCartesian(I, R, Ω)
    val (p, v) = convertAndScale2UnitVectors(uPV.pos, uPV.vel, mrt, mvt, rvdot)
    val posVel = TEME.CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
    (posVel, uPV, finalPolarNodalXX, lppState, secularElemt, eaState)    
  }

  def propagate2Cartesian(t: Minutes) : TEME.CartesianElems[F] = {  
    val (posVel, _, _,_,_,_) = propagate2CartesianAndContext(t)
    posVel
  }
  
  def propagate(t: Minutes)  = propagate2CartesianAndContext(t)

  case class LongPeriodPeriodicState(axnl: F, aynl: F, xl: F)
  case class EccentricAnomalyState(eo1 : F, coseo1: F, sineo1: F, ecosE: F, esinE: F)  
  case class ShortPeriodPeriodicState(
    elem: TEME.SGPElems[F], 
    I: F,     // inclination 
    R: F,     // Radial velocity    
    Ω: F,     // argument of the node
    mrt: F, 
    mvt: F, 
    rvdot: F)


  /**
   * Vallado's code works with internal units of length LU (units of earth’s radius  
   * R⊕ in km) and time TU (units of the orbit’s period in min) 
   * TU = 60 * sqrt( (R⊕ km)³ /(μ km³ /s² ) ) min
   * where μ is the earth’s gravitational constant; μ = 1 UL³/UT² in internal units.    
   */
  def convertAndScale2UnitVectors(pos : Vector[F], vel : Vector[F], mrt: F, mvt: F, rvdot: F): (Vector[F], Vector[F]) = {
      import wgs._
      ( (aE*mrt) *: pos,  vkmpersec *: (mvt *: pos + rvdot *: vel))
  }  
    
}



