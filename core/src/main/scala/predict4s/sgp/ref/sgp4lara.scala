package predict4s
package sgp
package ref

import org.scalactic.Or
import spire.algebra._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord._
import predict4s.coord.LNSConversions._
import predict4s.coord.SGPElemsConversions._

class SGP4Lara[F: Field: NRoot: Order: Trig: TruncatedDivision](
    sec: BrouwerLaneSecularCorrections[F]
) extends SGP4(sec)
    with LaraFirstOrderCorrections[F]
    with SimpleKeplerEq {

  /** Secular elements in Lara Non Singular coordinates, together with the constant angular momentum component N =
    * Θ*cosI, solving Kepler's equation for the eccentric anomaly on the way.
    */
  private def lnsAndN(secularElemt: SGPSecularCtx[F]): (LaraNonSingular[F], F) Or ErrorMessage = {
    val elem = secularElemt._1
    val ictx = secularElemt._2
    for {
      eaState <- solveKeplerEq(elem.e, elem.M)
      spnSecular <- sgpelems2spn(eaState, secularElemt)
      _N = spnSecular.`Θ/r` * spnSecular.r * ictx.c // N = Θ*cosI , which remains constant
      lns = spn2lns(spnSecular, ictx)
    } yield (lns, _N)
  }

  override def periodicCorrections(secularElemt: SGPSecularCtx[F]): SGPSPNResult[F] =
    lnsAndN(secularElemt).map { case (lns, _N) =>
      lns2spn(allCorrections(lns, secularElemt), _N)
    }

  // periodic corrections results in Lara Non Singular (LNS)
  def periodicCorrectionsLNS(secularElemt: SGPSecularCtx[F]): SGPLNSResult[F] =
    lnsAndN(secularElemt).map { case (lns, _) =>
      allCorrections(lns, secularElemt)
    }

  /*
   * This is used/compare to test the LPP corrections in SPN coordinates
   */
  def propagateToSPNLPP(secularElemt: SGPSecularCtx[F]): LPPSPNResult[F] =
    lnsAndN(secularElemt).map { case (lns, _N) =>
      val lppcorr1 = lns2spn(lppCorrections(lns, secularElemt)._1, _N)
      (lppcorr1, LongPeriodContext(0.as[F], 0.as[F], 0.as[F], 0.as[F], 0.as[F], 0.as[F]), secularElemt)
    }

  /*
   * This is used to test/compare the LPP corrections in CPN coordinates
   */
  def cpnLPPCorrections(secularElemt: SGPSecularCtx[F]): LPPCPNResult[F] =
    lnsAndN(secularElemt).map { case (lns, _N) =>
      lns2cpn(lppCorrections(lns, secularElemt)._1, _N)
    }

}

object SGP4Lara {

  def apply[F: Field: NRoot: Order: Trig: TruncatedDivision](sec: BrouwerLaneSecularCorrections[F]): SGP4Lara[F] =
    new SGP4Lara(sec)

  def fromTLE[F: Field: NRoot: Order: Trig: TruncatedDivision](
      tle: TLE,
      wgs: SGPConstants[F]
  ): SGP4Lara[F] Or ErrorMessage =
    BrouwerLaneSecularCorrections.fromTLE(tle, wgs).map(SGP4Lara(_))
}
