package sgp4.propagation

import spire.algebra.*
import spire.math.{sin, cos, abs}
import spire.implicits.*
import org.scalactic.Good
import scala.annotation.tailrec
import org.scalactic.Bad
import sgp4.coord.*

trait SimpleKeplerEq:

  /** Solve Kepler's equation M = E − e sinE to compute E the eccentric anomaly. The Newton-Raphson iterations start
    * from E0 = M = (l in Delauneys).
    */
  def solveKeplerEq[@sp(Double) F: Field: Trig: Order: FromDouble](e: F, M: F): AnomalyResult[F] =
    @tailrec
    def loop(E: F, remainingIters: Int): AnomalyResult[F] =
      val sinE = sin(E)
      val cosE = cos(E)
      val ecosE = e * cosE
      val esinE = e * sinE
      val fdot = 1 - ecosE
      val f = M - (E - esinE)
      val incr = f / fdot
      if abs(incr) < asF[F](1e-12) then Good(AnomalyState(E, cosE, sinE, ecosE, esinE))
      else if remainingIters < 0 then Bad(SGP4Error.KeplerNoConvergence(incr, E))
      else loop(E + incr, remainingIters - 1)
    loop(M, 10)

  def solveKeplerEq[@sp(Double) F: Field: Trig: Order: FromDouble](elem: SGPElems[F]): AnomalyResult[F] =
    solveKeplerEq(elem.e, elem.M)

  def solveKeplerEq[@sp(Double) F: Field: Trig: Order: FromDouble](elemCtx: SGPSecularCtx[F]): AnomalyResult[F] =
    solveKeplerEq(elemCtx.elem)
