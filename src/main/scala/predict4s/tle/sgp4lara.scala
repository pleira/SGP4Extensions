package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._

import TEME._

trait SGP4L[F] extends SGP4[F] {
  
  def getSecularEffect : Store[SecularPerturb[F]]
 
  def getLongPeriodPeriodicEffect(t: F)(state: SecularPerturb[F]) : Store[LongPeriodPeriodicPerturb[F]]

  def getShortPeriodPeriodicEffect(t: F)(lppp: LongPeriodPeriodicPerturb[F]) : Store[ShortPeriodPeriodicPerturb[F]] 

  def Cond[X,Y](
    test: => Boolean,
    left: => Store[X], 
    right: => Store[Y]): Store[Either[X,Y]] = 
    if (test)
      left map (Left(_))
    else 
      right map (Right(_))

  trait Interpreter{
    def runFrom[A, B](initial: Store[A])(
      program: A => Store[B]): Either[StoreError,B] = 
      run(initial flatMap program)

    def run[A](program: Store[A]): Either[StoreError, A]
  }
  
}

/**
 * TODO: Use of PolarNodal variables 
 */
case class SGP4Lara[F: Field : NRoot : Order : Trig](val state0: SGP4Context[F]) extends SGP4L[F] {
  
  implicit val wgs: SGPConstants[F] = state0.wgs
  
  override def propagate(t: F) : OrbitalState[F] = {
      val (_, el, am) = SecularEffects.propagate(t)(state0.tif)
      val (nodep, axnl, aynl, xl) = SGP4LongPeriodicEffects.calculateSGP4LongPeriodicEffects(state0.tif, el, am)
      val (eo1,ecosE,esinE) = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(nodep, axnl, aynl, xl)
      val nm    = el.n0
      val xincp = el.i0
      val cosip = cos(xincp)
      val sinip = sin(xincp)
      // here, should be something returned before in other coordinates 
      val posVel = ShortPeriodPeriodicPerturbations.calcPositionVelocity(state0.tif, nm, xincp, cosip, sinip, am, nodep, axnl, aynl, xl, eo1)
      OrbitalState(t, posVel)
    }
  
  override def getSecularEffect : Store[SecularPerturb[F]] = 
    StoreAndThen(PolarNodalSecularCalc[F](state0.tif), (state : SecularPerturb[F]) => Return(state))
  
  override def getLongPeriodPeriodicEffect(t: F)(state: SecularPerturb[F]) : Store[LongPeriodPeriodicPerturb[F]] =
    StoreAndThen(PolarNodalLongPeriodPeriodicCalc[F](state), (lppp : LongPeriodPeriodicPerturb[F]) => Return(lppp))

  override def getShortPeriodPeriodicEffect(t: F)(lppp: LongPeriodPeriodicPerturb[F]) : Store[ShortPeriodPeriodicPerturb[F]] =
    StoreAndThen(PolarNodalShortPeriodPeriodicCalc[F](lppp), (sppp : ShortPeriodPeriodicPerturb[F]) => Return(sppp))

}


object SGP4Lara {

  def apply[F : Field : NRoot : Order : Trig](tle: TLE)(implicit wgs: SGPConstants[F]) : SGP4Lara[F] = {
    
    val elem0 = TEME.sgpElems[F](tle)
    val tif  = SGP4TimeIndependentFunctions(elem0)
      // Propagate for time 0 minutes to get all initialized. 
    val t = 0.as[F] 
    val (_, el, am) = SecularEffects.propagate(t)(tif)

    val (nodep, axnl, aynl, xl) = SGP4LongPeriodicEffects.calculateSGP4LongPeriodicEffects(tif, el, am)
    
    val (eo1,ecosE,esinE) = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(nodep, axnl, aynl, xl)
    val nm    = el.n0
    val xincp = el.i0
    val cosip = cos(xincp)
    val sinip = sin(xincp)
    // here, should be something returned before in other coordinates 
    val posVel0 = ShortPeriodPeriodicPerturbations.calcPositionVelocity(tif, nm, xincp, cosip, sinip, am, nodep, axnl, aynl, xl, eo1)
    val state0 = SGP4Context(t, elem0, posVel0, tif, wgs)
    SGP4Lara(state0)
  }
}

case class SecularPerturb[F](elem: TEME.SGPElems[F], tif : SGP4TimeIndependentFunctions[F])
case class LongPeriodPeriodicPerturb[F](elem: TEME.SGPElems[F], lppp : F)
case class ShortPeriodPeriodicPerturb[F](elem: TEME.SGPElems[F], lppp : F)

// TODO
trait StoreInstruction[_]
case class DelaunaySecularCalc[F](tif : SGP4TimeIndependentFunctions[F]) extends StoreInstruction[SecularPerturb[F]]
case class DelaunayLongPeriodPeriodicCalc[F](secular : SecularPerturb[F]) extends StoreInstruction[LongPeriodPeriodicPerturb[F]]
case class DelaunayShortPeriodPeriodicCalc[F](lppp : LongPeriodPeriodicPerturb[F]) extends StoreInstruction[ShortPeriodPeriodicPerturb[F]]
case class PolarNodalSecularCalc[F](tif : SGP4TimeIndependentFunctions[F]) extends StoreInstruction[SecularPerturb[F]]
case class PolarNodalLongPeriodPeriodicCalc[F](secular : SecularPerturb[F]) extends StoreInstruction[LongPeriodPeriodicPerturb[F]]
case class PolarNodalShortPeriodPeriodicCalc[F](lppp : LongPeriodPeriodicPerturb[F]) extends StoreInstruction[ShortPeriodPeriodicPerturb[F]]


trait Store[U]{
  
  def flatMap[V](f: U => Store[V]): Store[V] = this match {
    case StoreAndThen(inst, next) => 
      StoreAndThen(inst, next andThen (_ flatMap f))
    case Return(t) => 
      f(t)
  }

  def map[V](f: U => V): Store[V] = 
    this flatMap ( u => Return(f(u)) )

}

case class Return[U](value: U) extends Store[U]
case class StoreAndThen[U,V](inst: StoreInstruction[U], next: U => Store[V]) extends Store[V]

sealed class StoreError(val msg: String) extends Exception(msg)
case class GenericError(override val msg: String) extends StoreError(msg)
case class WrapError(exception: Exception) extends StoreError(exception.getMessage)
