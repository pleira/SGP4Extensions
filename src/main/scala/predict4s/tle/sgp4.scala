package predict4s.tle
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.coord.CartesianElems
 
trait HelperTypes[F] {  
  type SinI = F  // type to remember dealing with the sine of the Inclination 
  type CosI = F  // type to remember dealing with the cosine of the Inclination 
  type Minutes = F // type to remember dealing with minutes from epoch
  type FinalState 
  type ShortPeriodState
  type LongPeriodState
  type EccentricAState
  type SecularElems = SGPElems[F]
}
  
trait SecularCorrections[F] extends HelperTypes[F] {
  def secularCorrections(t: Minutes): SGPElems[F]
}

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
    val sec : BrouwerLaneSecularCorrections[F]
    ) extends HelperTypes[F] {
  
  case class EccentricAnomalyState(E : F, cosE: F, sinE: F, ecosE: F, esinE: F) {
    def eccentricAnomaly = E
  }
  
  def propagate(t: Minutes)  = propagate2CartesianContext(t)

  def gsto : F = predict4s.tle.gstime(sec.elem0.epoch + 2433281.5) 
  
  def propagate2PolarNodalContext(t: Minutes): ((FinalState, ShortPeriodState, LongPeriodState, EccentricAState), SecularElems) = {
    val secularElemt = secularCorrections(t)
    (periodicCorrections(secularElemt), secularElemt)
  }
  
  def propagate2PolarNodal(t: Minutes) = {
    val ((finalPolarNodal, _, _, _), _) = propagate2PolarNodalContext(t)
    finalPolarNodal
  }

  def propagate2CartesianContext(t: Minutes) : 
    (CartesianElems[F], CartesianElems[F], FinalState, ShortPeriodState, LongPeriodState, EccentricAState)

  def propagate2Cartesian(t: Minutes) : CartesianElems[F] = {  
    val (posVel, _, _,_,_,_) = propagate2CartesianContext(t)
    posVel
  }
  
  /** 
   *  Calculates the new secular elements at time t in minutes from the epoch of the initial elements 
   */
  def secularCorrections(t: Minutes): SGPElems[F] = sec.secularCorrections(t)  
  
  def periodicCorrections(secularElemt : SGPElems[F]): (FinalState, ShortPeriodState, LongPeriodState, EccentricAState)

  
  /**
   * Vallado's code works with internal units of length LU (units of earth’s radius  
   * R⊕ in km) and time TU (units of the orbit’s period in min) 
   * TU = 60 * sqrt( (R⊕ km)³ /(μ km³ /s²) ) min
   * where μ is the earth’s gravitational constant; μ = 1 UL³/UT² in internal units.    
   */
  def convertAndScale2UnitVectors(pos : Vector[F], vel : Vector[F], mrt: F, mvt: F, rvdot: F): (Vector[F], Vector[F]) = {
      import sec.wgs.{aE,vkmpersec}
      ( (aE*mrt) *: pos,  vkmpersec *: (mvt *: pos + rvdot *: vel))
  }  

}



