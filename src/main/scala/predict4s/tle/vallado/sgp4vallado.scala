package predict4s.tle.vallado

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
import predict4s.tle.DpTransform


case class SGP4State[F](orbitalState: OrbitalState[F], uPV: TEME.CartesianElems[F], elem: TEME.SGPElems[F], 
    sppState : ShortPeriodPeriodicState[F], wgs: SGPConstants[F])

class SGP4Vallado[F : Field : NRoot : Order : Trig](val secularEffects: SecularEffects[F])(implicit wgs: SGPConstants[F])  {
    
  def propagate(t: F) : SGP4State[F] = {
    val secularState = secularEffects.calculate(t)
    val lppState = LongPeriodPeriodicEffects.calculate(secularState)    
    val eaState = NewtonRaphsonKeplerSolver.solve(lppState)
    val sppState = ShortPeriodPeriodicEffects.calculate(eaState)

    // unit position and velocity 
    import sppState._
    val uPV: TEME.CartesianElems[F] = TEME.polarNodal2UnitCartesian(I, R, Ω)
    
    // return position and velocity (in km and km/sec)
    val (p, v) = convertUnitVectors(uPV.pos, uPV.vel, mrt, mvt, rvdot)
    val posVel = TEME.CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
    val orbitalState = OrbitalState(t, posVel)
    SGP4State(orbitalState, uPV, sppState.elem, sppState, wgs)
  }

  /**
   * Vallado's code works with internal units of length LU (units of earth’s radius  
   * R⊕ in km) and time TU (units of the orbit’s period in min) 
   * TU = 60 * sqrt( (R⊕ km)³ /(μ km³ /s² ) ) min
   * where μ is the earth’s gravitational constant; μ = 1 UL³/UT² in internal units.    
   */
  def convertUnitVectors(pos : Vector[F], vel : Vector[F], mrt: F, mvt: F, rvdot: F)(implicit wgs: SGPConstants[F])
      : (Vector[F], Vector[F]) = {
      import wgs._
      ( (aE*mrt) *: pos,  vkmpersec *: (mvt *: pos + rvdot *: vel))
  }  

}

object SGP4Vallado {
  
  def apply[F : Field : NRoot : Order : Trig](elem0: TEME.SGPElems[F])(implicit wgs: SGPConstants[F]) : SGP4Vallado[F] = {
    val dpState = DpTransform.dpState(elem0)
    val geoPot  = GeoPotentialState(dpState)
    val secularEffects = SecularEffects(geoPot)
    new SGP4Vallado(secularEffects)
  }

  // FIXME
//  def dpState[F: Field: Trig](tle: TLE)(implicit wgs: SGPConstants[F]) :  DpTransform.DpState[F] = 
//    DpTransform.dpState(TEME.sgpElems(tle))
//
//  def geoState[F: Field: Trig](tle: TLE)(implicit wgs: SGPConstants[F]) : GeoPotentialState[F] =
//    GeoPotentialState(dpState(tle))
  
}
