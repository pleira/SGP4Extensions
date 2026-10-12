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

  /** Secular elements in Lara Non Singular coordinates, solving Kepler's equation for the eccentric anomaly on the way.
    * The constant angular momentum component N = Θ*cosI is derived from the result where needed.
    */
  private def lnsSecular(secularElemt: SGPSecularCtx[F]): LaraNonSingular[F] Or ErrorMessage = {
    val elem = secularElemt._1
    val ictx = secularElemt._2
    solveKeplerEq(elem.e, elem.M).map(eaState => spn2lns(sgpelems2spn(eaState, secularElemt), ictx))
  }

  override def periodicCorrections(secularElemt: SGPSecularCtx[F]): SGPSPNResult[F] =
    lnsSecular(secularElemt).map { lns =>
      val _N = lns.Θ * secularElemt._2.c // N = Θ*cosI , which remains constant
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
      val _N = lns.Θ * secularElemt._2.c // cosI = N/Θ, N remains a constant
      val lppcorr1 = lns2spn(lppCorrections(lns, secularElemt)._1, _N)
      (lppcorr1, LongPeriodContext(0.as[F], 0.as[F], 0.as[F], 0.as[F], 0.as[F], 0.as[F]), secularElemt)
    }

  /*
   * This is used to test/compare the LPP corrections in CPN coordinates
   */
  def cpnLPPCorrections(secularElemt: SGPSecularCtx[F]): LPPCPNResult[F] =
    lnsSecular(secularElemt).map { lns =>
      val _N = lns.Θ * secularElemt._2.c // cosI = N/Θ, N remains a constant
      lns2cpn(lppCorrections(lns, secularElemt)._1, _N)
    }

}

object SGP4Lara extends SGP4Companion[SGP4Lara] {

  def apply[F: Field: NRoot: Order: Trig: TruncatedDivision](sec: BrouwerLaneSecularCorrections[F]): SGP4Lara[F] =
    new SGP4Lara(sec)
}
