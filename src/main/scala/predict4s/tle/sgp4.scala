package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._

import TEME._

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
trait SGP4[F] {
  
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

class SGP4Brouwer[F: Field : NRoot : Order : Trig](tle: TLE)(implicit wgs: SGPConstants[F]) extends SGP4[F] {

  val elem = TEME.sgpElems[F](tle)
  val tif  = SGP4TimeIndependentFunctions(elem)

  override def getSecularEffect : Store[SecularPerturb[F]] = 
    StoreAndThen(DelaunaySecularCalc[F](tif), (state : SecularPerturb[F]) => Return(state))
  
  override def getLongPeriodPeriodicEffect(t: F)(state: SecularPerturb[F]) : Store[LongPeriodPeriodicPerturb[F]] =
    StoreAndThen(DelaunayLongPeriodPeriodicCalc[F](state), (lppp : LongPeriodPeriodicPerturb[F]) => Return(lppp))

  override def getShortPeriodPeriodicEffect(t: F)(lppp: LongPeriodPeriodicPerturb[F]) : Store[ShortPeriodPeriodicPerturb[F]] =
    StoreAndThen(DelaunayShortPeriodPeriodicCalc[F](lppp), (sppp : ShortPeriodPeriodicPerturb[F]) => Return(sppp))

}

class SGP4Lara[F: Field : NRoot : Order : Trig](tle: TLE)(implicit wgs: SGPConstants[F]) extends SGP4[F] {

  val elem = TEME.sgpElems[F](tle)
  val tif  = SGP4TimeIndependentFunctions(elem)

  override def getSecularEffect : Store[SecularPerturb[F]] = 
    StoreAndThen(PolarNodalSecularCalc[F](tif), (state : SecularPerturb[F]) => Return(state))
  
  override def getLongPeriodPeriodicEffect(t: F)(state: SecularPerturb[F]) : Store[LongPeriodPeriodicPerturb[F]] =
    StoreAndThen(PolarNodalLongPeriodPeriodicCalc[F](state), (lppp : LongPeriodPeriodicPerturb[F]) => Return(lppp))

  override def getShortPeriodPeriodicEffect(t: F)(lppp: LongPeriodPeriodicPerturb[F]) : Store[ShortPeriodPeriodicPerturb[F]] =
    StoreAndThen(PolarNodalShortPeriodPeriodicCalc[F](lppp), (sppp : ShortPeriodPeriodicPerturb[F]) => Return(sppp))

}

case class SGP4Perturb[F](t: F, elem: TEME.SGPElems[F], posVel: TEME.CartesianElems[F], tif : SGP4TimeIndependentFunctions[F])
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

/*
 * 
trait StoreInstruction[E[_], F]
case class DelaunaySecularCalc[F]() extends StoreInstruction[DelaunayElems, F]
case class DelaunayLongPeriodPeriodicCalc[F]() extends StoreInstruction[DelaunayElems, F]
case class DelaunayShortPeriodPeriodicCalc[F]() extends StoreInstruction[DelaunayElems, F]
case class PolarNodalSecularCalc[F]() extends StoreInstruction[PolarNodalElems, F]
case class PolarNodalLongPeriodPeriodicCalc[F]() extends StoreInstruction[PolarNodalElems, F]
case class PolarNodalShortPeriodPeriodicCalc[F]() extends StoreInstruction[PolarNodalElems, F]

trait Store[E[_], F]{
  
  def flatMap[V[_]](f: E[F] => Store[V, F]): Store[V, F] = this match {
    case StoreAndThen(inst, next) => 
//            StoreAndThen(inst, ???)
       //StoreAndThen.apply(inst, next andThen (_ flatMap f))
    case Return(t) => 
      f(t)
  }

  def map[V[_]](f: E[F] => V[F]): Store[V, F] = 
    this flatMap ( u => Return(f(u)) )

}
case class Return[E[_], F](value: E[F]) extends Store[E, F]
case class StoreAndThen[E[_], V[_], F](inst: StoreInstruction[E, F], next: E[F] => Store[V, F]) extends Store[V, F]  
*/

// TODO

//case class SGP4Brouwer[F : Field : NRoot : Order : Trig](val state0: SGP4Perturb[F]) extends SGP4[F] {
//     
//    override def propagate(t: F)(implicit wgs: WGSConstants[F]) : SGP4Perturb[F] = {
//      val (_, el, am) = SecularEffects.propagate(t)(state0.tif)
//      val (nodep, axnl, aynl, xl) = SGP4LongPeriodicEffects.calculateSGP4LongPeriodicEffects(state0.tif, el, am)
//      val (eo1,ecosE,esinE) = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(nodep, axnl, aynl, xl)
//      val nm    = el.n0
//      val xincp = el.i0
//      val cosip = cos(xincp)
//      val sinip = sin(xincp)
//      // here, should be something returned before in other coordinates 
//      val posVel = ShortPeriodPeriodicPerturbations.calcPositionVelocity(state0.tif, nm, xincp, cosip, sinip, am, nodep, axnl, aynl, xl, eo1)
//      SGP4State(t, el, posVel, state0.tif)
//    }
//}
//
//object SGP4Brouwer {
//
//  // TODO See if Delaunays variables can be introduced 
//  // this should use the state monad
//  def apply[F : Field : NRoot : Order : Trig](tle: TLE)(implicit wgs: WGSConstants[F]) : SGP4[F] = {
//    
//    val elem = TEME.sgpElems[F](tle)
//    val tif  = SGP4TimeIndependentFunctions(elem)
//      // Propagate for time 0 minutes to get all initialized. 
//    val t = 0.as[F] 
//    val (_, el, am) = SecularEffects.propagate(t)(tif)
//
//    val (nodep, axnl, aynl, xl) = SGP4LongPeriodicEffects.calculateSGP4LongPeriodicEffects(tif, el, am)
//    
//    val (eo1,ecosE,esinE) = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(nodep, axnl, aynl, xl)
//    val nm    = el.n0
//    val xincp = el.i0
//    val cosip = cos(xincp)
//    val sinip = sin(xincp)
//    // here, should be something returned before in other coordinates 
//    val posVel = ShortPeriodPeriodicPerturbations.calcPositionVelocity(tif, nm, xincp, cosip, sinip, am, nodep, axnl, aynl, xl, eo1)
//    val state0 = SGP4State(t, elem, posVel, tif)
//    SGP4Brouwer(state0)
//  }
//  
//}

