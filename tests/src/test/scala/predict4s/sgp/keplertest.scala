package predict4s.sgp

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._

// Tests of the algorithm used in solving Kepler's equation 
// Solutions are compared with the results obtained in Wolframalpha.com for Kepler's equation

class KeplerAndTrueAnomalyTest extends FunSuite  {
  
  object kepler extends SimpleKeplerEq 
  
  // e: eccentricity, M: mean anomaly, E: expected eccentric anomaly
  def solveKeplerAndCheckE(e : Double, M: Double, E: Double) = for {
      eas <- kepler.solveKeplerEq(e, M.toRadians)
      xEd = eas.E.toDegrees
      // Console.println(s"calculated eccentric anomaly: ${Ed} , expected eccentric anomaly: ${E} ")
      _ = assert(E === xEd)
  } yield eas
      
  def checkr(ecosE: Double, r: Double) = {
      val a = 10000
      val rc = a * (1 - ecosE)
      // note: precision is 2E-3 here, if not E-12
      implicit val toMinus3 : Equality[Double] = TolerantNumerics.tolerantDoubleEquality(2E-3)
      assert(rc === r)
  }
  
  test("Kepler equation solving followed by True Anomaly test, inputs in degrees ") {
    val e = 0.02
    val `e²` = e*e
    val β = sqrt(1 - `e²`) 
    val all =  List( // M, E, f, r 
          (0.0, 0.0, 0.0, 9800.0)
        , (20.0, 20.39942333334996, 20.802664870651377, 9812.542900454288)
        , (90.0, 91.14568650641034, 92.2912203675837, 10004.0)
        , (180.0, 180.0, 180.0, 10200.0)
        , (270.0, 268.85431349358964, -92.29122036758375, 10004.0)
        , (360.0, 360.0, 0.0, 9800.0)
        , (450.0, 451.1456865064104, 92.29122036758375, 10004.0)
        , (-20.0, -20.39942333334996, -20.802664870651377, 9812.542900454288)
        , (-90.0, -91.14568650641034, -92.29122036758375, 10004.0)
        , (-180.0, -180.0, -180.0, 10200.0)
        , (-270.0, -268.85431349358964, 92.2912203675837, 10004.0)
        , (-360.0, -360.0, 0.0, 9800.0)
        )
  implicit val tolerant : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(1E-12)
    
    all foreach { p =>
      val M = p._1; val E = p._2; val f = p._3; val r = p._4
      val orEas = solveKeplerAndCheckE(e, M, E)
      if (orEas.isBad) fail
      else {
        val eas = orEas.get
        import eas._
        val sinf = β*sinE
        val cosf = (cosE - e)
        val fr = atan2(sinf, cosf)
        val fd = fr.toDegrees
        assert(fd === f)
        checkr(ecosE, r)
      }
    }
  }
  
 }