package predict4s.comparison

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.coord.SGPElemsConversions
import predict4s.coord.SpecialPolarNodal
import predict4s.sgp.algo.SGP4Vallado
import predict4s.sgp.algo.SGP4ValladoLong
import predict4s.sgp.algo.SGP4PN
import predict4s.collision.TLE22675
import predict4s.collision.TLE24946

class Sgp4ImplComparison extends FunSuite with TLE22675 with TLE24946 with TLE00005  with TLE06251 with TLE28057 {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants
  val tol1 = TolerantNumerics.tolerantDoubleEquality(3E-4)
  val tol2 = TolerantNumerics.tolerantDoubleEquality(1.6E-6)
  
  import spire.std.any.DoubleAlgebra
  val tles = List(tle00005,tle06251,tle22675,tle24946,tle28057)
  for (tle <- tles) {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs)
    val model = BrouwerLaneSecularCorrections(elem0AndCtx,wgs)
    val vsgp4 = SGP4Vallado[Double](model)
    val pnsgp4 = SGP4PN[Double](model)
    val vlsgp4 = SGP4ValladoLong[Double](model)
    val ts = List.range(0, 30001, 3000)
    ts foreach { t => 
      vsgp4.secularCorrections(t) foreach { secularElemt =>
        val result = 
          for {  
           vfspn <- vsgp4.periodicCorrections(secularElemt)
           fpn <- pnsgp4.periodicCorrections(secularElemt)
           vlfspn <- vlsgp4.periodicCorrections(secularElemt)
        } yield (vfspn, fpn, vlfspn)
        if (result.isGood) {
          val (vfspn, fpn, vlfspn) = result.get
          val (vspnLPP,spnLPP, vlspnLPP) = (vfspn._2,fpn._2, vlfspn._2)
          val (vspn,pn, vlspn) = (vfspn._1,fpn._1, vlfspn._1)
          compareSPN(s"TLE ${tle.satelliteNumber} : long period periodic Vallado/Polar Nodals comparison at time $t", vspnLPP, spnLPP, tol1);
          compareSPN(s"TLE ${tle.satelliteNumber} : Vallado/Polar Nodals comparison in SPN at time $t", vspn, pn, tol1);
          compareSPN(s"TLE ${tle.satelliteNumber} : Vallado Long/Polar Nodals comparison in SPN at time $t", vlspn, pn, tol2);
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
      assert(spn1.θ === spn2.θ) 
    }       

} 


