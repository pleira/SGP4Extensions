package predict4s.comparison

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.coord.SGPElemsFactory
import predict4s.sgp.lara.SGP4Lara
import predict4s.sgp.pn.SGP4PN
import org.scalatest.Ignore
import predict4s.collision.TLE22675
import predict4s.collision.TLE24946
import spire.std.any


@Ignore
class Sgp4ImplComparison2 extends FunSuite with TLE24946 with TLE22675 with TLE00005  with TLE06251 with TLE28057 {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants

  import spire.std.any.DoubleAlgebra
  val tles = List(tle22675,tle24946,tle00005,tle06251,tle28057)
  for (tle <- tles) {
    val elem0AndCtx = SGPElemsFactory.sgpElemsAndContext(tle)
    val model = BrouwerLaneSecularCorrections(elem0AndCtx)
    val lsgp4 = SGP4Lara[Double](model)
    val pnsgp4 = SGP4PN[Double](model)
    val results = 
      for (t <- 0 to 360 by 20;
        secularElemt = lsgp4.secularCorrections(t);
        (_, pnspps, pnlpps, pneas) = pnsgp4.periodicCorrections(secularElemt);
        (lfinalspn, _, llpps, leas) = lsgp4.periodicCorrections(secularElemt)
       ) yield ((pnspps, lfinalspn), (pnlpps, llpps), (pneas.E, leas.E))
    
    val r2 = results.unzip3
    val resSpp = r2._1
    val resLpp = r2._2
    val resEcc = r2._3
  
    val resL = resLpp.unzip  // now PN and SPN
    test(s"TLE ${tle.satelliteNumber} long period periodic Lara/Polar Nodals comparison")
    {  
      implicit val toMinus5 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(1.2E-5)
      resLpp foreach { result =>
        val (pnspn, lspn) = result
        val pn = pnspn._1
  //      val lspn = lsgp4.laraNonSingular2SpecialPolarNodal(lns, pn.I)
        assert(lspn.r === pn.r)
        assert(lspn.R === pn.R)
       // assert(lspn.Ω === pn.Ω)
        assert(lspn.`Θ/r` === pn.`Θ/r`)
        assert(lspn.I === pn.I)
        //assert(lspn.`Θ/r`*scala.math.cos(lspn.I) === pn.N/pn.r)
        //assert(lspn.θ === pn.θ)
      }
    }
//    test(s"TLE ${tle.satelliteNumber} Short period periodic Lara/Polar Nodals comparison")
//    {  
  //    implicit val toMinus5 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(1E-3)
  //    resSpp foreach { result =>
  //      val (pnspn, lspn) = result
  //      val pn = pnspn._1
  //      assert(lspn.r === pn.r)
  //      assert(lspn.R === pn.R)
  //      assert(lspn.Ω === pn.Ω)
  //      assert(lspn.`Θ/r` === pn.`Θ/r`)
  //      assert(lspn.I === pn.I)
  //      //assert(lspn.`Θ/r`*scala.math.cos(lspn.I) === pn.N/pn.r)
  //      assert(lspn.θ === pn.θ)
  //    }
//    }
    test(s"TLE ${tle.satelliteNumber} Eccentric anomaly comparison") {  
      implicit val toMinus12 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(1E-12)
      resEcc foreach { result =>
        val (pnE, lE) = result
        assert(lE === pnE)
      }
    }  
 }
//    val pn : PolarNodalElems[Double] = pnlpps._1
//    val v : SpecialPolarNodal[Double] = vlpps._1
//    // assert(v.θ === pn.θ)
//    assert(v.r === pn.r)
    
    
  
} 


