package predict4s
package sgp

import org.scalactic.Or
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.coord._
import predict4s.coord.CoordinatesConversions._

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
abstract class SGP4[@sp(Double) F : Field : NRoot : Order : Trig](
    val sec : BrouwerLaneSecularCorrections[F]
    ) {
 
  type Minutes = sec.Minutes

  def gsto : F = TimeUtils.gstime(sec.elem0Ctx.elem.epoch + 2433281.5) 
    
  def propagate(t: Minutes): SGPPropResult[F] = propagate2CartesianContext(t)
   
  def propagate2CartesianContext(t: Minutes) : SGPPropResult[F] = 
    for {
      secularElemt <- secularCorrections(t)
      spn <- periodicCorrections(secularElemt)
      unitpv = spn2UnscaledCartesian(spn)
      pv = scaleUnitCartesians(unitpv,spn.r)
    } yield (pv, unitpv, spn) 
  
    
  def corrections2CartesianContext(secularElemt : SGPSecularCtx[F]) : SGPPropResult[F] = 
    for {
      spn <- periodicCorrections(secularElemt)
      unitpv = spn2UnscaledCartesian(spn)
      pv = scaleUnitCartesians(unitpv,spn.r)
    } yield (pv, unitpv, spn) 
      
  def propagate2SPNContext(t: Minutes) = 
    for {
     sc <- secularCorrections(t)
     pc <- periodicCorrections(sc)
    } yield (pc, sc)
    
  /** 
   *  Calculates the new secular elements at time t in minutes from the epoch of the initial elements 
   */
  def secularCorrections(t: Minutes): SGPSecularResult[F] = sec.secularCorrections(t)  

  /** 
   *  Applies the periodic corrections to the secular elements at time t in minutes from the epoch of the initial elements 
   */  
  def periodicCorrections(secularElemt : SGPSecularCtx[F]) : SGPSPNResult[F]
  
  /**
   * Vallado's code works with internal units of length LU (units of earth’s radius  
   * R⊕ in km) and time TU (units of the orbit’s period in min) 
   * TU = 60 * sqrt( (R⊕ km)³ /(μ km³ /s²) ) min
   * where μ is the earth’s gravitational constant; μ = 1 UL³/UT² in internal units.    
   */ 
  def scaleUnitCartesians(unitElems: CartesianElems[F], r: F): CartesianElems[F] = {
      import sec.elem0Ctx.wgs.{aE,vkmpersec}, unitElems._
      CartesianElems( (aE*r)*x, (aE*r)*y , (aE*r)*z, vkmpersec*vx, vkmpersec*vy, vkmpersec*vz)
  } 
    
}
