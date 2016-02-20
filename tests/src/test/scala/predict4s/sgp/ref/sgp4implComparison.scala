package predict4s
package sgp
package ref

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import scala.math._
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.coord.SGPElemsConversions
import predict4s.coord.SpecialPolarNodal
import predict4s.conjunction.TLE22675
import predict4s.conjunction.TLE24946
import predict4s.coord.LNSConversions._

class Sgp4ImplComparison extends FunSuite with TLE22675 with TLE24946 with TLE00005  with TLE06251 with TLE28057 {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants
  val tol1 = TolerantNumerics.tolerantDoubleEquality(3E-4)
  val tol2 = TolerantNumerics.tolerantDoubleEquality(1.6E-6)
  val tol3 = TolerantNumerics.tolerantDoubleEquality(6E-4)
  
  import spire.std.any.DoubleAlgebra
  val tles = List(tle00005,tle06251,tle22675,tle24946,tle28057)
  for (tle <- tles) {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs).get
    val model = BrouwerLaneSecularCorrections(elem0AndCtx)
    val vasgp4 = SGP4Vallado[Double](model)
    val pnsgp4 = SGP4PN[Double](model)
    val vlsgp4 = SGP4ValladoLong[Double](model)
    val lasgp4 = SGP4Lara[Double](model)
    val ts = List.range(0, 30000, 3000)
    ts foreach { t => 
      vasgp4.secularCorrections(t) foreach { secularElemt =>
        val result = 
          for {  
           vafspn <- vasgp4.periodicCorrections(secularElemt)
           pnfspn <- pnsgp4.periodicCorrections(secularElemt)
           vlfspn <- vlsgp4.periodicCorrections(secularElemt)
           lafspn <- lasgp4.periodicCorrections(secularElemt)
           //lafnsing <- lasgp4.periodicCorrections(secularElemt)
        } yield (vafspn, pnfspn, vlfspn, lafspn) // lafnsing)
        if (result.isGood) {
          val (vafspn, pnfspn, vlfspn, lafspn) = result.get
         // val (vaspnLPP,pnspnLPP, vlspnLPP) = (vafspn._2,pnfspn._2, vlfspn._2)
          //val (vaspn,pnspn, vlspn, laspn) = (vafspn._1,pnfspn._1, vlfspn._1, lafspn._1)
//          val laspn = laraNonSingular2SpecialPolarNodal(lafnsing._1, lafnsing._2._2._1.I)
          
          //compareSPN(s"TLE ${tle.satelliteNumber} : long period periodic Vallado/Polar Nodals comparison at time $t", vaspnLPP, pnspnLPP, tol1);
          compareSPN(s"TLE ${tle.satelliteNumber} : Vallado/Polar Nodals comparison in SPN at time $t", vafspn, pnfspn, tol1);
          compareSPN(s"TLE ${tle.satelliteNumber} : Vallado Long/Polar Nodals comparison in SPN at time $t", vlfspn, pnfspn, tol2);
          
          compareSPN2(s"TLE ${tle.satelliteNumber} : Vallado/Lara Non Singular comparison in SPN at time $t", vafspn, lafspn, tol3);
        }
      }
    }
  }

  def compareSPN(msg: String, spn1: SpecialPolarNodal[Double], spn2: SpecialPolarNodal[Double], tolerant: Equality[Double]) = 
    test(msg) {  
      implicit val tolerantVal = tolerant
      assert(spn1.r === spn2.r)
      assert(spn1.R === spn2.R)
      assert(spn1.Ω === spn2.Ω)
      assert(spn1.`Θ/r` === spn2.`Θ/r`)
      assert(spn1.I === spn2.I)
      if (abs(spn1.θ - spn2.θ) > 6) {
        // different signs, do not compare this one 
        // assert(spn1.θ === - spn2.θ) 
      } else {
        assert(spn1.θ === spn2.θ) 
      }

    }       


  def compareSPN2(msg: String, spn1: SpecialPolarNodal[Double], spn2: SpecialPolarNodal[Double], tolerant: Equality[Double]) = 
    test(msg) {  
      implicit val tolerantVal = tolerant
      assert(spn1.r === spn2.r)
      assert(spn1.R === spn2.R)
      if (abs(spn1.Ω - spn2.Ω) > 6) {
        // different signs, correct one by 2Pi
        val o1 = if (spn1.Ω < 0) spn1.Ω + 2*Pi else spn1.Ω
        val o2 = if (spn2.Ω < 0) spn2.Ω + 2*Pi else spn2.Ω
        assert(o1 === o2) 
      } else {
        assert(spn1.Ω === spn2.Ω)
      }
      assert(spn1.`Θ/r` === spn2.`Θ/r`)
      assert(spn1.I === spn2.I)
      if (abs(spn1.θ - spn2.θ) < 6) {
        assert(spn1.θ === spn2.θ) 
      } else {
        // different signs, correct one by 2Pi
        val o1 = if (spn1.θ < 0) spn1.θ + 2*Pi else spn1.θ
        val o2 = if (spn2.θ < 0) spn2.θ + 2*Pi else spn2.θ
        assert(o1 === o2)         
      }

    }  
} 


