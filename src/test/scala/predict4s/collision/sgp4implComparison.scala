package predict4s.collision

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import predict4s.sgp.{NearTLEs}
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.coord.SGPElems
import predict4s.coord.SGPElemsFactory
import predict4s.sgp.vallado.SGP4Vallado
import predict4s.sgp.pn.SGP4PN
import predict4s.coord.PolarNodalElems
import predict4s.coord.SpecialPolarNodal

class Sgp4ImplComparison extends FunSuite with TLE22675 {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants

  import spire.std.any.DoubleAlgebra
  val elem0AndCtx = SGPElemsFactory.sgpElemsAndContext(tle22675)
  val model = BrouwerLaneSecularCorrections(elem0AndCtx)
  val vsgp4 = SGP4Vallado[Double](model)
  val pnsgp4 = SGP4PN[Double](model)
  val results = 
    for (t <- 0 to 360 by 20;
      secularElemt = vsgp4.secularCorrections(t);
      (_, _, pnlpps, pneas) = pnsgp4.periodicCorrections(secularElemt);
      (_, _, vlpps, leas) = vsgp4.periodicCorrections(secularElemt);
      vE = leas.E - secularElemt.ω     // Vallado's solved kepler equation on Lyddane variables => E = u - ω
     ) yield ((pnlpps, vlpps), (pneas.E, vE))
  
  val r2 = results.unzip
  val resLpp = r2._1
  val resEcc = r2._2

  val resL = resLpp.unzip  // now PN and SPN
  test("polar nodals comparison")
  {  
    implicit val toMinus5 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(1.2E-5)
    resLpp foreach { result =>
      val ((pn, _), v) = result
      val vspn = v._1
      assert(vspn.r === pn.r)
      assert(vspn.R === pn.R)
      assert(vspn.Ω === pn.ν)
      assert(vspn.`Θ/r` === pn.Θ/pn.r)
// FIXME     assert(vspn.`Θ/r`*scala.math.cos(vspn.I) === pn.N)
// FIXME     assert(vspn.θ === pn.θ)
    }
  }
  test("Eccentric anomaly comparison") {  
    implicit val toMinus3 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(1.1E-3)
    resEcc foreach { result =>
      val (pnE, vE) = result
      assert(vE === pnE)
    }
  }  
//    val pn : PolarNodalElems[Double] = pnlpps._1
//    val v : SpecialPolarNodal[Double] = vlpps._1
//    // assert(v.θ === pn.θ)
//    assert(v.r === pn.r)
    
    
  
} 


