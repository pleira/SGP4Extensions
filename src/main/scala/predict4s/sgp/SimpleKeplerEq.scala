package predict4s.sgp

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.coord.SGPElems

trait SimpleKeplerEq {

  /**
   * Solve Kepler's equation expressed in Delauney's variables 
   * 		M = E − e sinE
   * to compute E the eccentric anomaly.
   * The Newton-Raphson iterations start from E0 = M = (l in Delauneys).
   */
   def solveKeplerEq[F: Field: Trig: Order](e: F, M : F): EccentricAnomalyState[F] = {
     def loop(E: F, remainingIters: Int) : EccentricAnomalyState[F] = {
      val sinE = sin(E)
      val cosE = cos(E)
      val ecosE = e*cosE 
      val esinE = e*sinE
      val fdot = 1 - ecosE
      val f = M - (E - esinE)
      val tem : F = f / fdot  
      val incr =
        if (abs(tem) > 0.95.as[F]) {
          if (tem > 0.as[F]) 0.95.as[F] else -0.95.as[F]
        } else tem
      if (remainingIters <= 0 || abs(incr) < 1e-12.as[F]) {
        EccentricAnomalyState(E,cosE,sinE,ecosE,esinE)   
      } else {
        loop(E+incr, remainingIters - 1)
      }
    }
    loop(M, 10)  
   }
   
   def solveKeplerEq[F: Field: Trig: Order](elem : SGPElems[F]): EccentricAnomalyState[F] = 
    solveKeplerEq(elem.e, elem.M)
  
}