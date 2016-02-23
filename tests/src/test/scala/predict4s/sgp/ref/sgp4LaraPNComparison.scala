package predict4s
package sgp
package ref

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import spire.math._
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.coord.SGPElemsConversions
import predict4s.coord.CSpecialPolarNodal
import predict4s.conjunction.TLE22675
import predict4s.conjunction.TLE24946
import predict4s.coord.LNSConversions._

class Sgp4LaraPNComparison extends FunSuite with TLE22675 with TLE24946 with TLE00005  with TLE06251 with TLE28057 {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants
  val tol = TolerantNumerics.tolerantDoubleEquality(1.5E-7)
  val `2pi` = 2*pi 
  
  import spire.std.any.DoubleAlgebra
  val tles = List(tle00005,tle06251,tle22675,tle24946,tle28057)
  for (tle <- tles) {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs).get
    val model = BrouwerLaneSecularCorrections(elem0AndCtx)
    val pnsgp4 = SGP4PN[Double](model)
    val lasgp4 = SGP4Lara[Double](model)
    val ts = List.range(0, 30000, 3000)
    ts foreach { t => 
      pnsgp4.secularCorrections(t) foreach { secularElemt =>
        for {  
           pncpn <- pnsgp4.cpnLPPCorrections(secularElemt)
           lacpn <- lasgp4.cpnLPPCorrections(secularElemt)
        } yield compareCPN(s"TLE ${tle.satelliteNumber} : LPP Corrections in Polar Nodals/Lara Non Singular comparison in CPN at time $t", pncpn, lacpn, tol)
      }
    }
  }

  def compareCPN(msg: String, cpn1: CSpecialPolarNodal[Double], cpn2: CSpecialPolarNodal[Double], tolerant: Equality[Double]) = 
    test(msg) {  
      implicit val tolerantVal = tolerant
      assert(cpn1.r === cpn2.r)
      assert(cpn1.R === cpn2.R)
      if (abs(cpn1.Ω - cpn2.Ω) < 6) {
        assert(cpn1.Ω === cpn2.Ω)
      } else {
        // different signs, correct one by 2pi
        val o1 = if (cpn1.Ω < 0) cpn1.Ω + `2pi` else cpn1.Ω
        val o2 = if (cpn2.Ω < 0) cpn2.Ω + `2pi` else cpn2.Ω
        assert(o1 === o2) 
      }
      assert(cpn1.`Θ/r` === cpn2.`Θ/r`)
      assert(cpn1.cosI === cpn2.cosI)
      if (abs(cpn1.θ - cpn2.θ) < 6) {
        assert(cpn1.θ === cpn2.θ) 
      } else {
        // different signs, correct one by 2pi
        val o1 = if (cpn1.θ < 0) cpn1.θ + `2pi` else cpn1.θ
        val o2 = if (cpn2.θ < 0) cpn2.θ + `2pi` else cpn2.θ
        assert(o1 === o2)         
      }
    }       

} 
