package sgp4
package propagation

import spire.algebra._
import spire.math.{sin, cos, abs, pi}
import spire.implicits._
import spire.syntax.primitives._
import sgp4.coord._
import org.scalactic.Good
import org.scalactic.Bad
import scala.annotation.tailrec

trait TwoTermsKeplerEq {

  /** Solve Kepler's equation expressed in Lyddane's variables U = Ψ + S'cosΨ − C'sinΨ where U = F' − h' to compute the
    * anomaly Ψ = U'+g', where U is the eccentric anomaly and g is the argument of the perigee (ω). The Newton-Raphson
    * iterations start from Ψ0 = U.
    */
  def solveKeplerEq[@sp(Double) F: Field: Trig: Order: TruncatedDivision](elems: LyddaneElems[F]): AnomalyResult[F] = {
    import elems._
    // U = F' - h' = M" + g";
    val u = TruncatedDivision[F].tmod(xl - Ω, 2.as[F] * pi)

    @tailrec
    def loop(U: F, remainingIters: Int): AnomalyResult[F] = {
      val sinU = sin(U)
      val cosU = cos(U)
      val ecosU = `C´` * cosU + `S´` * sinU
      val esinU = `C´` * sinU - `S´` * cosU
      val fdot = 1 - ecosU
      val f = (u + esinU - U)
      val incr = f / fdot
      if (abs(incr) < 1e-12.as[F]) {
        // NOTE: as we have Lyddane's variables here, the eccentric anomaly is U - ω when comparing
        Good(AnomalyState(U, cosU, sinU, ecosU, esinU))
      } else if (remainingIters < 0) {
        Bad(SGP4Error.LyddaneKeplerNoConvergence(incr, U))
      } else {
        loop(U + incr, remainingIters - 1)
      }
    }
    loop(u, 10)
  }

}
