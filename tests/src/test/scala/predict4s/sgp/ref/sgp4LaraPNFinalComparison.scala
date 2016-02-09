package predict4s.sgp.ref

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import scala.math._
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.coord.SGPElemsConversions
import predict4s.coord.SpecialPolarNodal
import predict4s.collision.TLE22675
import predict4s.collision.TLE24946
import predict4s.coord.LaraConversions._

class Sgp4LaraPNFinalComparison extends FunSuite with TLE22675 with TLE24946 with TLE00005  with TLE06251 with TLE28057 {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants
  val tol = TolerantNumerics.tolerantDoubleEquality(2.5E-5)
  
  import spire.std.any.DoubleAlgebra
  val tles = List(tle00005,tle06251,tle22675,tle24946,tle28057)
  for (tle <- tles) {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs).get
    val model = BrouwerLaneSecularCorrections(elem0AndCtx)
    val pnsgp4 = SGP4PN[Double](model)
    val lasgp4 = SGP4Lara[Double](model)
    val ts = List.range(0, 1000, 1)
    ts foreach { t => 
      pnsgp4.secularCorrections(t) foreach { secularElemt =>
        for {  
           pnspn <- pnsgp4.periodicCorrections(secularElemt)
           laspn <- lasgp4.periodicCorrectionsSPN(secularElemt)
        } yield compareCPN(s"TLE ${tle.satelliteNumber} : Final Corrections in Polar Nodals/Lara Non Singular comparison in SPN at time $t", pnspn._1, laspn._1, tol)
      }
    }
  }

  def compareCPN(msg: String, spn1: SpecialPolarNodal[Double], spn2: SpecialPolarNodal[Double], tolerant: Equality[Double]) = 
    test(msg) {  
      implicit val tolerantVal = tolerant
      assert(spn1.r === spn2.r)
      assert(spn1.R === spn2.R)
//      if (abs(spn1.Ω - spn2.Ω) < 6) {
//        assert(spn1.Ω === spn2.Ω)
//      } else {
//        // different signs, correct one by 2Pi
//        val o1 = if (spn1.Ω < 0) spn1.Ω + 2*Pi else spn1.Ω
//        val o2 = if (spn2.Ω < 0) spn2.Ω + 2*Pi else spn2.Ω
//        assert(o1 === o2) 
//      }
//      assert(spn1.`Θ/r` === spn2.`Θ/r`)
//      assert(spn1.I === spn2.I)
//      if (abs(spn1.θ - spn2.θ) < 6) {
//        assert(spn1.θ === spn2.θ) 
//      } else {
//        // different signs, correct one by 2Pi
//        val o1 = if (spn1.θ < 0) spn1.θ + 2*Pi else spn1.θ
//        val o2 = if (spn2.θ < 0) spn2.θ + 2*Pi else spn2.θ
//        assert(o1 === o2)         
//      }
    }       

} 
