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

class EccentricAnomalyComparisonCheck extends FunSuite with TLE22675 {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants
  implicit val toMinus3 : Equality[Double]= TolerantNumerics.tolerantDoubleEquality(1.1E-3)

  import spire.std.any.DoubleAlgebra
  val elem0AndCtx = SGPElemsFactory.sgpElemsAndContext(tle22675)
  val model = BrouwerLaneSecularCorrections(elem0AndCtx)
  val vsgp4 = SGP4Vallado[Double](model)
  val pnsgp4 = SGP4PN[Double](model)
  val t = 0
  for (t <- 0 to 360 by 20) {
    // secular elements are common to both algorithms
    val secularElemt = vsgp4.secularCorrections(t)
    
    val (_, _, _, eas) = pnsgp4.periodicCorrections(secularElemt)
    val (_, _, _, leas) = vsgp4.periodicCorrections(secularElemt)
   
    // Vallado's solved kepler equation on Lyddane variables => E = u - ω
    // PN algorithm gives E directly
    val vE = leas.E - secularElemt.ω
    assert(vE === eas.E)
  }
} 


