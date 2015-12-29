package predict4s.sgp.vallado
import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.coord.SGPElems
import predict4s.sgp.EccentricAnomalyState

trait TwoTermsKeplerEq {
   
  /**
   * Solve Kepler's equation expressed in Lyddane's variables 
   * 		U = Ψ + S'cosΨ − C'sinΨ
   * where U = F' − h' to compute the anomaly Ψ = U'+g', where U is the eccentric anomaly 
   * and g is the argument of the perigee (ω).
   * The Newton-Raphson iterations start from Ψ0 = U.
   */
  def solveKeplerEq[F: Field: Trig: Order](lylppState: LyddaneLongPeriodPeriodicState[F]) : EccentricAnomalyState[F]  = {
    import lylppState._   
    // U = F' - h' = M" + g"; 
    val u = Field[F].mod(xl - Ω, 2.as[F]*pi)  

    def loop(U: F, remainingIters: Int) : EccentricAnomalyState[F] = {
      val sinU = sin(U)
      val cosU = cos(U)
      val ecosU = axnl * cosU + aynl * sinU
      val esinU = axnl * sinU - aynl * cosU
      val fdot = 1 - ecosU
      val f = (u + esinU - U)
      val tem : F = f / fdot  
      val incr =
        if (abs(tem) >= 0.95.as[F]) {
          if (tem > 0.as[F]) 0.95.as[F] else -0.95.as[F]
        } else tem
      val Un = U+incr
      if (remainingIters <= 0 || abs(incr) < 1e-12.as[F]) {
        // NOTE: as we have Lyddane's variables here, the eccentric anomaly is U - ω when comparing 
        EccentricAnomalyState(Un,cosU,sinU,ecosU,esinU)   
      } else {
        loop(Un, remainingIters - 1)
      }
    }
    loop(u, 10)
  }
  
}