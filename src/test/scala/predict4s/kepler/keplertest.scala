package predict4s.kepler

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.tle.EccentricAnomalyState

// Test the same algorithm as used in solving Kepler's equation 
object KeplerEquationSolver {
    
  // input in radians
  def solve(e : Double, M: Double): EccentricAnomalyState[Double] = {
         
    def loop(E: Double, remainingIters: Int) : EccentricAnomalyState[Double] = {
      val sinE = sin(E)
      val cosE = cos(E)
      val ecosE = e*cosE 
      val esinE = e*sinE
      val fdot = 1 - ecosE
      val f = M - (E - esinE)
      val tem  = f / fdot  
      val incr =
        if (abs(tem) >= 0.95) {
          if (tem > 0) 0.95 else -0.95
        } else tem
      val En = E+incr
      if (remainingIters <= 0 || abs(incr) < 1e-12) {
        EccentricAnomalyState(En,cosE,sinE,ecosE,esinE)   
      } else {
        loop(En, remainingIters - 1)
      }
    }
    loop(M, 10)
  }
}

// Solution can be compared with the results obtained in Wolframalpha.com
class KeplerTest extends FunSuite  {
  
  val twopi = 2 * pi
  val kepler = KeplerEquationSolver // new KeplerEquationSolver()
  
  test("Kepler equation solving test, input in degrees ") {
    val e = 0.02
    val listM = List(0, 20, 90, 180, 270, 360, 450, -20, -90, -180, -270, -360, -450)
    val ListE = List(0, 20.399423333349958, 91.1456865064012, 180, 268.8543134935988, 360.0, 451.1456865064012,
                        -20.399423333349958, -91.1456865064012, -180, -268.8543134935988, -360.0, -451.1456865064012)
    listM zip ListE foreach {p => solveKeplerAndCheck(e, p._1, p._2)}    
    solveKeplerAndCheck(0.6, 85, 115.91942563011823)
  }
  
  def solveKeplerAndCheck(e : Double, M: Double, E: Double) = {
    val eas = kepler.solve(e, M*pi/180)
    val Er = eas.E
    val Ed = Er*180/pi
    // Console.println("eccentric anomaly:" + Ed)
    assert(E === Ed) 
  }
  
}

// Solution can be compared with the results obtained in Wolframalpha.com
class KeplerAndTrueAnomalyTest extends FunSuite  {
  
  val twopi = 2 * pi
  val kepler = KeplerEquationSolver // new KeplerEquationSolver()
  implicit val toMinus12 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(2E-3)
  
  def solveKeplerAndCheck1(e : Double, M: Double, E: Double)  = {
      val eas  = kepler.solve(e, M*pi/180)
      val Ed = eas.E*180/pi
      // Console.println("eccentric anomaly:" + Ed)
      assert(E === Ed)
      eas
  }
  
  test("Kepler equation solving followed by True Anomaly test, inputs in degrees ") {
    val e = 0.02
    val `e²` = e*e
    val β = sqrt(1 - `e²`) 
    val listM = List(0.0, 20, 90, 180, 270, 360) // , 450, -20, -90, -180, -270, -360, -450)
    val ListE = List(0, 20.399423333349958, 91.1456865064012, 180, 268.8543134935988, 360) // , 451.1456865064012,
                        // -20.399423333349958, -91.1456865064012, -180, -268.8543134935988, -360.0, -451.1456865064012)
    val Listf = List(0, 20.802664870651377, 92.2912203675837, 180, -92.29122036758375, 0)  
    
    // note: precision is 2E-3 here, if not E-12
    val Listr = List(9800.0, 9812.542900454288, 10004, 10200, 10004, 9800)
    val a = 10000;
    val l3 = (listM, ListE, Listf).zipped.toList 
    val all = l3 zip Listr map { case ((a,b,c), d) => (a,b,c,d) }
    all foreach { p =>
      val eas = solveKeplerAndCheck1(e, p._1, p._2)
      val sinf = β*eas.sinE
      val cosf = (eas.cosE - e)
      val fr = atan2(sinf, cosf)
      val fd = fr*180/pi
      assert(fd === p._3)
      val r = a * (1 - eas.ecosE) // val p = a*(1 - `e²`)  // semilatus rectum , as MU=1, p=Z²
      assert(r === p._4)
    }    
  }
  
 }