package predict4s
package sgp
package ref

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import spire.std.any.DoubleAlgebra
import spire.math._
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.coord.SGPElemsConversions
import predict4s.coord.SpecialPolarNodal
import predict4s.conjunction.TLE22675
import predict4s.conjunction.TLE24946
import predict4s.coord.LNSConversions._

class Sgp4LaraFinalComparison extends FunSuite with TLE22675 with TLE24946 with TLE00005  with TLE06251 with TLE28057 {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants
  val tol = TolerantNumerics.tolerantDoubleEquality(3.5E-5)
  
  val tles = List(tle00005,tle06251,tle22675,tle24946,tle28057)
  for (tle <- tles) {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs).get
    val model = BrouwerLaneSecularCorrections(elem0AndCtx)
    val lasgp4 = SGP4Lara[Double](model)
    val ts = List.range(0, 1000, 100)
    ts foreach { t => 
      lasgp4.secularCorrections(t) foreach { secularElemt =>
        for {  
           spn1 <- lasgp4.periodicCorrections(secularElemt)
           laspn <- lasgp4.periodicCorrectionsWithN(secularElemt)
        } yield compareSPN(s"TLE ${tle.satelliteNumber} : Final Corrections in Lara Non Singular comparison in SPN at time $t", spn1, laspn, tol)
      }
    }
  }

  def compareSPN(msg: String, spn1: SpecialPolarNodal[Double], spn2: SpecialPolarNodal[Double], tolerant: Equality[Double]) = 
    test(msg) {  
      implicit val tolerantVal = tolerant
      assert(spn1.r === spn2.r)
      assert(spn1.R === spn2.R)
      if (abs(spn1.Ω - spn2.Ω) < 6) {
        assert(spn1.Ω === spn2.Ω)
      } else {
        // different signs, correct one by 2Pi
        val o1 = if (spn1.Ω < 0) spn1.Ω + 2*pi else spn1.Ω
        val o2 = if (spn2.Ω < 0) spn2.Ω + 2*pi else spn2.Ω
        assert(o1 === o2) 
      }
      //assert(spn1.`Θ/r` === spn2.`Θ/r`)
      assert(spn1.I === spn2.I)
      if (abs(spn1.θ - spn2.θ) < 6) {
        assert(spn1.θ === spn2.θ) 
      } else {
        // different signs, correct one by 2Pi
        val o1 = if (spn1.θ < 0) spn1.θ + 2*pi else spn1.θ
        val o2 = if (spn2.θ < 0) spn2.θ + 2*pi else spn2.θ
        assert(o1 === o2)         
      }
    }       

} 
