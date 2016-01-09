package predict4s.comparison

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.coord.SGPElemsConversions
import predict4s.sgp.vallado.SGP4Vallado
import predict4s.sgp.pn.SGP4PN
import predict4s.collision.TLE22675
import predict4s.collision.TLE24946
import spire.std.any

class Sgp4ImplComparison extends FunSuite with TLE22675 with TLE24946 with TLE00005  with TLE06251 with TLE28057 {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants
  
  import spire.std.any.DoubleAlgebra
  val tles = List(tle00005,tle06251,tle22675,tle24946,tle28057)
  for (tle <- tles) {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs)
    val model = BrouwerLaneSecularCorrections(elem0AndCtx,wgs)
    val vsgp4 = SGP4Vallado[Double](model)
    val pnsgp4 = SGP4PN[Double](model)
    val results = 
      for (t <- 0 to 360 by 1;
        secularElemt = vsgp4.secularCorrections(t);
        (_, pnspps, pnlpps) = pnsgp4.periodicCorrections(secularElemt);
        (_, vspps, vlpps) = vsgp4.periodicCorrections(secularElemt)
        // vE = veas.E - secularElemt.ω     // Vallado's solved kepler equation on Lyddane variables => E = u - ω
       ) yield ((pnspps, vspps), (pnlpps, vlpps))
    
    val r2 = results.unzip
    val resSpp = r2._1
    val resLpp = r2._2
  
    val resL = resLpp.unzip 
    test(s"TLE ${tle.satelliteNumber} : long period periodic Vallado/Polar Nodals comparison")
    {  
      implicit val toMinus5 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(3.4E-4)
      resLpp foreach { result =>
        val (pnspn, v) = result
        val pn = pnspn._1
        val vspn = v._1
        assert(vspn.r === pn.r)
        assert(vspn.R === pn.R)
        assert(vspn.Ω === pn.Ω)
        assert(vspn.`Θ/r` === pn.`Θ/r`)
        assert(vspn.I === pn.I)
        //assert(vspn.`Θ/r`*scala.math.cos(vspn.I) === pn.N/pn.r)
        assert(vspn.θ === pn.θ)
        // check auxiliary variables `el²`, pl, βl, sin2θ, cos2θ
        import v._2._
        import pnspn.{_2 => aux}
        assert(`el²` === aux.`el²`)
        assert(pl === aux.pl)
        assert(βl === aux.βl)
        if (sin2θ > 0 && aux.sin2θ < 0 ||
            sin2θ < 0 && aux.sin2θ > 0 ||
            !(sin2θ === aux.sin2θ) || 
            !(cos2θ === aux.cos2θ)) {
          Console.print(s"vallado: ${v.toString()}\npnspn:   ${pnspn.toString()}\n")
        }
//        assert(v._5 === pnspn._5)  // FIXME
//        assert(v._6 === pnspn._6)        
      }
    }
    test(s"TLE ${tle.satelliteNumber} : short period periodic Vallado/Polar Nodals comparison")
    {  
      implicit val toMinus3 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(1E-4)
      resSpp foreach { result =>
        val (pnspn, v) = result
        val pn = pnspn._1
        val vspn = v._1
        assert(vspn.r === pn.r)
        assert(vspn.R === pn.R)
        assert(vspn.Ω === pn.Ω)
        assert(vspn.`Θ/r` === pn.`Θ/r`)
        assert(vspn.I === pn.I)
        //assert(vspn.`Θ/r`*scala.math.cos(vspn.I) === pn.N/pn.r)
        assert(vspn.θ === pn.θ)
      }
    }
//    test(s"TLE ${tle.satelliteNumber} : Eccentric anomaly comparison") {  
//      implicit val toMinus3 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(1.5E-3)
//      resEcc foreach { result =>
//        val (pnE, vE) = result
//        assert(vE === pnE)
//      }
//    }
  }
//    val pn : PolarNodalElems[Double] = pnlpps._1
//    val v : SpecialPolarNodal[Double] = vlpps._1
//    // assert(v.θ === pn.θ)
//    assert(v.r === pn.r)
    
    
  
} 


