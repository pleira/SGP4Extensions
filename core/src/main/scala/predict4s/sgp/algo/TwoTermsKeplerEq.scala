package predict4s.sgp.algo
import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.coord._

trait TwoTermsKeplerEq {
   
  /**
   * Solve Kepler's equation expressed in Lyddane's variables 
   * 		U = Ψ + S'cosΨ − C'sinΨ
   * where U = F' − h' to compute the anomaly Ψ = U'+g', where U is the eccentric anomaly 
   * and g is the argument of the perigee (ω).
   * The Newton-Raphson iterations start from Ψ0 = U.
   */
  def solveKeplerEq[F: Field: Trig: Order](lylppState: LyddaneElems[F]) : AnomalyState[F]  = {
    import lylppState._   
    // U = F' - h' = M" + g"; 
    val u = Field[F].mod(xl - Ω, 2.as[F]*pi)  

    def loop(U: F, remainingIters: Int) : AnomalyState[F] = {
      val sinU = sin(U)
      val cosU = cos(U)
      val ecosU = `C´` * cosU + `S´` * sinU
      val esinU = `C´` * sinU - `S´` * cosU
      val fdot = 1 - ecosU
      val f = (u + esinU - U)
      val tem : F = f / fdot  
      val incr =
        if (abs(tem) >= 0.95.as[F]) {
          if (tem > 0.as[F]) 0.95.as[F] else -0.95.as[F]
        } else tem
      if (remainingIters <= 0 || abs(incr) < 1e-12.as[F]) {
        // NOTE: as we have Lyddane's variables here, the eccentric anomaly is U - ω when comparing 
        AnomalyState(U,cosU,sinU,ecosU,esinU)   
      } else {
        loop(U + incr, remainingIters - 1)
      }
    }
    loop(u, 10)
  }
  
}