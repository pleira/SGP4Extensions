package predict4s
package sgp
package ref

import spire.algebra._
import spire.math.{sin,cos,abs}
import spire.implicits._
import spire.syntax.primitives._
import org.scalactic.Good
import org.scalactic.Bad
import predict4s.coord._

trait SimpleKeplerEq {

  /**
   * Solve Kepler's equation expressed in Delauney's variables 
   * 		M = E − e sinE
   * to compute E the eccentric anomaly.
   * The Newton-Raphson iterations start from E0 = M = (l in Delauneys).
   */
   def solveKeplerEq[@sp(Double) F: Field: Trig: Order](e: F, M : F): AnomalyResult[F] = {
     def loop(E: F, remainingIters: Int) : AnomalyResult[F] = {
      val sinE = sin(E)
      val cosE = cos(E)
      val ecosE = e*cosE 
      val esinE = e*sinE
      val fdot = 1 - ecosE
      val f = M - (E - esinE)
      val incr = f / fdot  
      if (abs(incr) < 1e-12.as[F]) {
        Good(AnomalyState(E,cosE,sinE,ecosE,esinE))   
      } else if (remainingIters < 0) {
        Bad(s"No convergence while solving Kepler equation: incr=$incr, E=$E")
      } else {
        loop(E+incr, remainingIters - 1)
      }
    }
    loop(M, 10)  
   }
   
   def solveKeplerEq[@sp(Double) F: Field: Trig: Order](elem : SGPElems[F]): AnomalyResult[F] = 
    solveKeplerEq(elem.e, elem.M)
   
   def solveKeplerEq[@sp(Double) F: Field: Trig: Order](elemCtx : SGPSecularCtx[F]): AnomalyResult[F] = 
    solveKeplerEq(elemCtx._1)
}