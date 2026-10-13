package sgp4
package model

import org.scalactic.Or
import spire.algebra._
import spire.implicits._
import spire.syntax.primitives._
import sgp4.coord._
import sgp4.coord.CoordinatesConversions._
import sgp4.coord.LNSConversions._
import sgp4.coord.LyddaneConversions._
import sgp4.coord.SGPElemsConversions._
import sgp4.earth_gravity.SGPConstants
import sgp4.io.TLE
import sgp4.propagation._

/** The propagator models of the SGP4 theory, which is applied for all orbits with periods of T <= 225 min. It performs
  * a propagation in time of doubly averaged elements according to their secular rates of change due to the zonal
  * harmonics J2 and J4 of the Earth potential, and due to drag perturbations in an atmosphere with a power-law altitude
  * profile of air density. The propagated, doubly averaged elements at epoch are subsequently converted into singly
  * averaged elements, by overlaying long-periodic perturbations due to J3, before a final conversion step to osculating
  * elements by superimposition of first-order, short-period perturbation amplitudes due to J2. (from Space Debris, by
  * H. Klinkrad, pag 216).
  *
  * All variants share the pipeline defined by the SGP4 base class: secular corrections to the mean elements, periodic
  * corrections and conversion to Cartesian TEME. The four concrete models differ only in the periodic corrections,
  * which are shown side by side here (see the README for the comparison table).
  */
abstract class SGP4[@sp(Double) F: Field: NRoot: Order: Trig: TruncatedDivision](
    val sec: BrouwerLaneSecularCorrections[F]
) {

  type Minutes = F // minutes from the TLE epoch

  def gsto: F = TimeUtils.gstime(sec.elem0Ctx.elem.epoch + 2433281.5)

  def propagate(t: Minutes): SGPPropResult[F] = propagate2CartesianContext(t)

  def propagate2CartesianContext(t: Minutes): SGPPropResult[F] =
    for {
      secularElemt <- secularCorrections(t)
      spn <- periodicCorrections(secularElemt)
      unitpv = spn2UnscaledCartesian(spn)
      pv = scaleUnitCartesians(unitpv, spn.r)
    } yield SGPPropCtx(pv, unitpv, spn)

  def corrections2CartesianContext(secularElemt: SGPSecularCtx[F]): SGPPropResult[F] =
    for {
      spn <- periodicCorrections(secularElemt)
      unitpv = spn2UnscaledCartesian(spn)
      pv = scaleUnitCartesians(unitpv, spn.r)
    } yield SGPPropCtx(pv, unitpv, spn)

  def propagate2SPNContext(t: Minutes) =
    for {
      sc <- secularCorrections(t)
      pc <- periodicCorrections(sc)
    } yield (pc, sc)

  /** Calculates the new secular elements at time t in minutes from the epoch of the initial elements
    */
  def secularCorrections(t: Minutes): SGPSecularResult[F] = sec.secularCorrections(t)

  /** Applies the periodic corrections to the secular elements at time t in minutes from the epoch of the initial
    * elements
    */
  def periodicCorrections(secularElemt: SGPSecularCtx[F]): SGPSPNResult[F]

  /** Vallado's code works with internal units of length LU (units of earth's radius R⊕ in km) and time TU (units of the
    * orbit's period in min) TU = 60 * sqrt( (R⊕ km)³ /(μ km³ /s²) ) min where μ is the earth's gravitational constant;
    * μ = 1 UL³/UT² in internal units.
    */
  def scaleUnitCartesians(unitElems: CartesianElems[F], r: F): CartesianElems[F] = {
    import sec.elem0Ctx.wgs.{aE, vkmpersec}, unitElems._
    CartesianElems((aE * r) * x, (aE * r) * y, (aE * r) * z, vkmpersec * vx, vkmpersec * vy, vkmpersec * vz)
  }
}

/** Common structure of the SGP4 propagator companion objects: construction from an already built secular corrections
  * model, or from a TLE via BrouwerLaneSecularCorrections.fromTLE, which can fail.
  */
trait SGP4Companion[Alg[_]] {

  def apply[F: Field: NRoot: Order: Trig: TruncatedDivision](sec: BrouwerLaneSecularCorrections[F]): Alg[F]

  def fromTLE[F: Field: NRoot: Order: Trig: TruncatedDivision](
      tle: TLE,
      wgs: SGPConstants[F]
  ): Alg[F] Or SGP4Error =
    BrouwerLaneSecularCorrections.fromTLE(tle, wgs).map(apply(_))
}

// Here are collected SGP4 algorithms that perform the periodic corrections in polar nodals (SPN variant),
// among them, Vallado's original

abstract class SGP4WithSPNCorrections[F: Field: NRoot: Order: Trig: TruncatedDivision](
    sec: BrouwerLaneSecularCorrections[F]
) extends SGP4(sec)
    with SPNShortPeriodCorrections[F] {

  override def periodicCorrections(secularElemt: SGPSecularCtx[F]): SGPSPNResult[F] =
    lppCorrections(secularElemt).map(sppCorrections)

  def lppCorrections(secularElemt: SGPSecularCtx[F]): LPPSPNResult[F]
}

/** Vallado's original algorithm: the long period corrections in Lyddane's variables, with the eccentric anomaly
  * obtained from Kepler's equation solved in Lyddane form.
  */
class SGP4Vallado[F: Field: NRoot: Order: Trig: TruncatedDivision](
    sec: BrouwerLaneSecularCorrections[F]
) extends SGP4WithSPNCorrections(sec)
    with LyddaneLongPeriodCorrections[F]
    with TwoTermsKeplerEq {

  override def lppCorrections(secularElemt: SGPSecularCtx[F]): LPPSPNResult[F] = {
    // long period corrections in Lyddane's coordinates
    val lydElems = lylppCorrections(secularElemt)
    for {
      // To transform to Special Polar Nodals, get the eccentric anomaly
      eaState <- solveKeplerEq(lydElems)
      spnctx <- lyd2spn(eaState, lydElems)
    } yield LPPSPNCtx(spnctx._1, spnctx._2, secularElemt)
  }
}

/** As SGP4Vallado, plus the extra long period terms in Lyddane's variables.
  */
class SGP4ValladoLong[F: Field: NRoot: Order: Trig: TruncatedDivision](
    sec: BrouwerLaneSecularCorrections[F]
) extends SGP4WithSPNCorrections(sec)
    with LyddaneExtraLongPeriodCorrections[F]
    with TwoTermsKeplerEq {

  override def lppCorrections(secularElemt: SGPSecularCtx[F]): LPPSPNResult[F] = {
    // long period corrections in Lyddane's coordinates
    val lydElems = lppLydCorrections(secularElemt)
    for {
      // To transform to Special Polar Nodals, get the eccentric anomaly
      eaState <- solveKeplerEq(lydElems)
      spnctx <- lyd2spn(eaState, lydElems)
    } yield LPPSPNCtx(spnctx._1, spnctx._2, secularElemt)
  }
}

/** The long period corrections computed directly in Special Polar Nodals.
  */
class SGP4PN[F: Field: NRoot: Order: Trig: TruncatedDivision](
    sec: BrouwerLaneSecularCorrections[F]
) extends SGP4WithSPNCorrections(sec)
    with SPNLongPeriodCorrections[F]
    with SimpleKeplerEq {

  /** Secular elements transformed to Special Polar Nodals, solving Kepler's equation for the eccentric anomaly. */
  private def spnSecularCtx(secularElemt: SGPSecularCtx[F]): SpecialPolarNodal[F] Or SGP4Error = {
    val elem = secularElemt.elem
    solveKeplerEq(elem.e, elem.M).map(eaState => sgpelems2spn(eaState, secularElemt))
  }

  override def lppCorrections(secularElemt: SGPSecularCtx[F]): LPPSPNResult[F] =
    for {
      spnSecular <- spnSecularCtx(secularElemt)
      lppcorr = lppCorrectionsSPN(SPNSecularCtx(spnSecular, secularElemt))
    } yield LPPSPNCtx(lppcorr._1, lppcorr._2, secularElemt)

  /** long period corrections in CSpecialPolarNodal coordinates used to compare with Lara's non singular results
    */
  def cpnLPPCorrections(secularElemt: SGPSecularCtx[F]): LPPCPNResult[F] =
    for {
      spnSecular <- spnSecularCtx(secularElemt)
      lppcorr = lppCorrectionsCPN(SPNSecularCtx(spnSecular, secularElemt))
    } yield lppcorr
}

/** Lara's algorithm: the first order long and short period corrections derived in his non-singular variables.
  */
class SGP4Lara[F: Field: NRoot: Order: Trig: TruncatedDivision](
    sec: BrouwerLaneSecularCorrections[F]
) extends SGP4(sec)
    with LaraFirstOrderCorrections[F]
    with SimpleKeplerEq {

  /** Secular elements in Lara Non Singular coordinates, solving Kepler's equation for the eccentric anomaly on the way.
    * The constant angular momentum component N = Θ*cosI is derived from the result where needed.
    */
  private def lnsSecular(secularElemt: SGPSecularCtx[F]): LaraNonSingular[F] Or SGP4Error = {
    val elem = secularElemt.elem
    val ictx = secularElemt.iCtx
    solveKeplerEq(elem.e, elem.M).map(eaState => spn2lns(sgpelems2spn(eaState, secularElemt), ictx))
  }

  override def periodicCorrections(secularElemt: SGPSecularCtx[F]): SGPSPNResult[F] =
    lnsSecular(secularElemt).map { lns =>
      val _N = lns.Θ * secularElemt.iCtx.c // N = Θ*cosI , which remains constant
      lns2spn(allCorrections(lns, secularElemt), _N)
    }

  // periodic corrections results in Lara Non Singular (LNS)
  def periodicCorrectionsLNS(secularElemt: SGPSecularCtx[F]): SGPLNSResult[F] =
    lnsSecular(secularElemt).map(lns => allCorrections(lns, secularElemt))

  /*
   * This is used/compare to test the LPP corrections in SPN coordinates
   */
  def propagateToSPNLPP(secularElemt: SGPSecularCtx[F]): LPPSPNResult[F] =
    lnsSecular(secularElemt).map { lns =>
      val _N = lns.Θ * secularElemt.iCtx.c // cosI = N/Θ, N remains a constant
      val lppcorr1 = lns2spn(lppCorrections(lns, secularElemt)._1, _N)
      LPPSPNCtx(lppcorr1, LongPeriodContext(0.as[F], 0.as[F], 0.as[F], 0.as[F], 0.as[F], 0.as[F]), secularElemt)
    }

  /*
   * This is used to test/compare the LPP corrections in CPN coordinates
   */
  def cpnLPPCorrections(secularElemt: SGPSecularCtx[F]): LPPCPNResult[F] =
    lnsSecular(secularElemt).map { lns =>
      val _N = lns.Θ * secularElemt.iCtx.c // cosI = N/Θ, N remains a constant
      lns2cpn(lppCorrections(lns, secularElemt)._1, _N)
    }
}

object SGP4Vallado extends SGP4Companion[SGP4Vallado] {

  def apply[F: Field: NRoot: Order: Trig: TruncatedDivision](sec: BrouwerLaneSecularCorrections[F]): SGP4Vallado[F] =
    new SGP4Vallado(sec)
}

object SGP4PN extends SGP4Companion[SGP4PN] {

  def apply[F: Field: NRoot: Order: Trig: TruncatedDivision](sec: BrouwerLaneSecularCorrections[F]): SGP4PN[F] =
    new SGP4PN(sec)
}

object SGP4ValladoLong extends SGP4Companion[SGP4ValladoLong] {

  def apply[F: Field: NRoot: Order: Trig: TruncatedDivision](
      sec: BrouwerLaneSecularCorrections[F]
  ): SGP4ValladoLong[F] = new SGP4ValladoLong(sec)
}

object SGP4Lara extends SGP4Companion[SGP4Lara] {

  def apply[F: Field: NRoot: Order: Trig: TruncatedDivision](sec: BrouwerLaneSecularCorrections[F]): SGP4Lara[F] =
    new SGP4Lara(sec)
}

/** The standard entry point of the SGP4 propagator, corresponding to the original Vallado algorithm: SGP4.apply and
  * SGP4.fromTLE build SGP4Vallado propagators, as in the python sgp4 library, where sgp4.model.SGP4 is Vallado's
  * implementation.
  */
object SGP4 extends SGP4Companion[SGP4Vallado] {

  def apply[F: Field: NRoot: Order: Trig: TruncatedDivision](sec: BrouwerLaneSecularCorrections[F]): SGP4Vallado[F] =
    new SGP4Vallado(sec)
}
