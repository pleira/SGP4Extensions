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
import predict4s.coord.CartesianElems
import predict4s.conjunction.TLE22675
import predict4s.conjunction.TLE24946
import predict4s.coord.LNSConversions._

class Sgp4ImplComparison extends FunSuite with TLE22675 with TLE24946 with TLE00005  with TLE06251 with TLE28057 {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants
  val tol1 = TolerantNumerics.tolerantDoubleEquality(1.5)
  val tol2 = TolerantNumerics.tolerantDoubleEquality(4E-3)
  val tol3 = TolerantNumerics.tolerantDoubleEquality(1.5)
  val tol4 = TolerantNumerics.tolerantDoubleEquality(1E-3)
  val tol5 = TolerantNumerics.tolerantDoubleEquality(4E-4)
  val tol6 = TolerantNumerics.tolerantDoubleEquality(2E-3)  
  val `2pi` = 2*pi 
  
  import spire.std.any.DoubleAlgebra
  val tles = List(tle00005) // ,tle06251,tle22675,tle24946,tle28057)
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
           vapv <- vasgp4.corrections2CartesianContext(secularElemt)
           pnpv <- pnsgp4.corrections2CartesianContext(secularElemt)
           vlpv <- vlsgp4.corrections2CartesianContext(secularElemt)
           lapv <- lasgp4.corrections2CartesianContext(secularElemt)
        } yield (vapv, pnpv, vlpv, lapv)
        if (result.isGood) {
          val (vapv, pnpv, vlpv, lapv) = result.get
          comparePV(s"TLE ${tle.satelliteNumber} : Vallado/Polar Nodals position comparison in Cartesian at time $t", vapv._1, pnpv._1, tol1);
          comparePV(s"TLE ${tle.satelliteNumber} : Vallado Long/Polar Nodals position comparison in Cartesian at time $t", vlpv._1, pnpv._1, tol2);          
          comparePV(s"TLE ${tle.satelliteNumber} : Vallado/Lara Non Singular position comparison in Cartesian at time $t", vapv._1, lapv._1, tol3);
          compareVel(s"TLE ${tle.satelliteNumber} : Vallado/Polar Nodals velocity comparison in Cartesian at time $t", vapv._1, pnpv._1, tol4);
          compareVel(s"TLE ${tle.satelliteNumber} : Vallado Long/Polar Nodals velocity comparison in Cartesian at time $t", vlpv._1, pnpv._1, tol5);          
          compareVel(s"TLE ${tle.satelliteNumber} : Vallado/Lara Non Singular velocity comparison in Cartesian at time $t", vapv._1, lapv._1, tol6);
        }
      }
    }
  }

  def comparePV(msg: String, pv1: CartesianElems[Double], pv2: CartesianElems[Double], tolerant: Equality[Double]) = 
    test(msg) {  
      implicit val tolerantVal = tolerant
      assert(pv1.x === pv2.x)
      assert(pv1.y === pv2.y)
      assert(pv1.z === pv2.z)
    }       

  def compareVel(msg: String, pv1: CartesianElems[Double], pv2: CartesianElems[Double], tolerant: Equality[Double]) = 
    test(msg) {  
      implicit val tolerantVal = tolerant
      assert(pv1.vx === pv2.vx)
      assert(pv1.vy === pv2.vy)
      assert(pv1.vz === pv2.vz) 
  }  
} 


