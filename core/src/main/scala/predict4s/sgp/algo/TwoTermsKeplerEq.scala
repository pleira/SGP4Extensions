package predict4s.sgp.algo
import spire.algebra._
import spire.math.{sin,cos,abs,pi}
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.coord._
import predict4s.sgp._
import org.scalactic.Or
import org.scalactic.Good
import org.scalactic.Bad

trait TwoTermsKeplerEq {
   
  /**
   * Solve Kepler's equation expressed in Lyddane's variables 
   * 		U = Ψ + S'cosΨ − C'sinΨ
   * where U = F' − h' to compute the anomaly Ψ = U'+g', where U is the eccentric anomaly 
   * and g is the argument of the perigee (ω).
   * The Newton-Raphson iterations start from Ψ0 = U.
   */
  def solveKeplerEq[F: Field: Trig: Order](elems: LyddaneElems[F]) : AnomalyResult[F]  = {
    import elems._   
    // U = F' - h' = M" + g"; 
    val u = Field[F].mod(xl - Ω, 2.as[F]*pi)  

    def loop(U: F, remainingIters: Int) : AnomalyResult[F] = {
      val sinU = sin(U)
      val cosU = cos(U)
      val ecosU = `C´` * cosU + `S´` * sinU
      val esinU = `C´` * sinU - `S´` * cosU
      val fdot = 1 - ecosU
      val f = (u + esinU - U)
      val incr = f / fdot  
      if (abs(incr) < 1e-12.as[F]) {
        // NOTE: as we have Lyddane's variables here, the eccentric anomaly is U - ω when comparing 
        Good(AnomalyState(U,cosU,sinU,ecosU,esinU))   
      } else if (remainingIters < 0) {
        Bad(s"No convergence while solving Kepler equation: incr=$incr, U=$U")
      } else {
        loop(U + incr, remainingIters - 1)
      }
    }
    loop(u, 10)
  }
  
}