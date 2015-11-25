package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._

import TEME._


case class SGP4LaraB[F : Field : NRoot : Order : Trig](val state0: SGP4Context[F]) extends SGP4[F] {
    
  implicit val wgs: SGPConstants[F] = state0.wgs
    
  override def propagate(t: F) : OrbitalState[F] = {
    val (_, el) = SecularEffects.propagate(t)(state0.tif)
    val (nodep, axnl, aynl, xl) = SGP4LongPeriodicEffects.calculateSGP4LongPeriodicEffects(state0.tif, el)
    val (eo1,ecosE,esinE) = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(nodep, axnl, aynl, xl)
    val nm    = el.n
    val xincp = el.i
    val cosip = cos(xincp)
    val sinip = sin(xincp)
    // here, should be something returned before in other coordinates 
    val posVel = ShortPeriodPeriodicPerturbations.calcPositionVelocity(state0.tif, nm, xincp, cosip, sinip, el.a, nodep, axnl, aynl, xl, eo1)
    OrbitalState(t, posVel)
  }
}

object SGP4LaraB {

  def apply[F : Field : NRoot : Order : Trig](tle: TLE)(implicit wgs: SGPConstants[F]) : SGP4LaraB[F] = {
    
    val elem0 = TEME.sgpElems[F](tle)
    val tif  = SGP4TimeIndependentFunctions(elem0)
      // Propagate for time 0 minutes to get all initialized. 
    val t0 = 0.as[F]
    val (_, el) = SecularEffects.propagate(t0)(tif)

    val (nodep, axnl, aynl, xl) = SGP4LongPeriodicEffects.calculateSGP4LongPeriodicEffects(tif, el)
    
    val (eo1,ecosE,esinE) = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(nodep, axnl, aynl, xl)
    val nm    = el.n
    val xincp = el.i
    val cosip = cos(xincp)
    val sinip = sin(xincp)
    // here, should be something returned before in other coordinates 
    val posVel0 = ShortPeriodPeriodicPerturbations.calcPositionVelocity(tif, nm, xincp, cosip, sinip, el.a, nodep, axnl, aynl, xl, eo1)
    val context0 = SGP4Context(t0, elem0, posVel0, tif, wgs)
    SGP4LaraB(context0)
  }
  
}

