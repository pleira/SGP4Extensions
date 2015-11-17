package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._

import TEME._   

case class SGP4HootsState[F](orbitalState: OrbitalState[F], uPV: TEME.CartesianElems[F], elem: TEME.SGPElems[F], 
    sppState : ShortPeriodPeriodicState[F], sec: HootsSecularEffects[F], wgs: SGPConstants[F])

case class SGP4Hoots[F : Field : NRoot : Order : Trig](state0: SGP4HootsState[F])  { // extends SGP4[F]
    
  implicit val wgs: SGPConstants[F] = state0.wgs
  val secularEffects =  state0.sec
  
  def propagate(t: F) : SGP4HootsState[F] = {
    val secularState = secularEffects.secularEffects(t)
    val lppState = SGP4LongPeriodicEffects.calcHootsSGP4LongPeriodicEffects(secularState)    
    val eaState = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(lppState)
    val sppState = ShortPeriodPeriodicPerturbations.calcPositionVelocity(eaState)

    // unit position and velocity 
    import sppState._
    val uPV: TEME.CartesianElems[F] = TEME.coord2UnitCartesian(xinc, su, xnode)
    
    // return position and velocity (in km and km/sec)
    val (p, v) = convertUnitVectors(uPV.pos, uPV.vel, mrt, mvt, rvdot)
    val posVel = TEME.CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
    val orbitalState = OrbitalState(t, posVel)
    SGP4HootsState(orbitalState, uPV, sppState.elem, sppState, secularEffects, wgs)
  }

  def convertUnitVectors(pos : Vector[F], vel : Vector[F], mrt: F, mvt: F, rvdot: F) 
      : (Vector[F], Vector[F]) = {      
     ( mrt *: pos,  (1.0/60.0).as[F] *: (mvt *: pos + rvdot *: vel))
  }  

}

object SGP4Hoots {

  def apply[F : Field : NRoot : Order : Trig](elem0: TEME.SGPElems[F])(implicit wgs: SGPConstants[F]) : SGP4Hoots[F] = {
  
    val dpState = DpTransform.dpState(elem0)
    val geoPot  = GeoPotentialState(dpState)
    val secularEffects = HootsSecularEffects(geoPot)
    val t0 = 0.as[F]
    val secularState = secularEffects.secularEffects(t0)
    val lppState = SGP4LongPeriodicEffects.calcHootsSGP4LongPeriodicEffects(secularState)    
    val eaState = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(lppState)
    val sppState = ShortPeriodPeriodicPerturbations.calcPositionVelocity(eaState)

    // unit position and velocity 
    import sppState._
    val uPV: TEME.CartesianElems[F] = TEME.coord2UnitCartesian(xinc, su, xnode)
    
    // return position and velocity (in km and km/sec)
    val (p, v) = convertUnitVectors(uPV.pos, uPV.vel, mrt, mvt, rvdot)
    val posVel0 = TEME.CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
    val orbitalState0 = OrbitalState(t0, posVel0)
    val state0 = SGP4HootsState(orbitalState0, uPV, elem0, sppState, secularEffects, wgs)
    SGP4Hoots(state0)
  }

  def convertUnitVectors[F : Field : NRoot : Order : Trig](pos : Vector[F], vel : Vector[F], mrt: F, mvt: F, rvdot: F) 
      : (Vector[F], Vector[F]) = {      
     ( mrt *: pos,  (1.0/60.0).as[F] *: (mvt *: pos + rvdot *: vel))
  }  
  
}


