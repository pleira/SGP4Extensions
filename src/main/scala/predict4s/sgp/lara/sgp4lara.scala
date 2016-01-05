package predict4s.sgp.lara

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord._
import predict4s.coord.LaraConversions._
import predict4s.coord.SGPElemsConversions._
import predict4s.coord.{SGPElems,AnomalyState}


class SGP4Lara[F : Field : NRoot : Order : Trig](
 sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4(sec) with LaraFirstOrderCorrections[F] {
 
  val wgs = sec.wgs
  val ctx0 = sec.ctx0
    
  // this method uses Special Polar Nodal Variables useful for comparing with 
  // other algorithms
  override def periodicCorrections(secularElemt : SGPElems[F])
      :  (FinalState[F], ShortPeriodState[F], LongPeriodState[F]) = {
    
    val pcLara = periodicCorrectionsNative(secularElemt)
    
    // Remark that the N Polar Nodal variable is an integral of the zonal problem
    // and, therefore, its value is always known from given initial conditions
    // H´´ = G´´cosI = N 
    val N = sqrt(1 - e*e) * cos(secularElemt.I)
    
    val finalPolarNodalt = laraNonSingular2SpecialPolarNodal(pcLara._1._1, N) 
    val spnSppc = laraNonSingular2SpecialPolarNodal(pcLara._1._2, N)
    val spnLppc = laraNonSingular2SpecialPolarNodal(pcLara._2._1, N)
    
    // final state in Polar Nodal coordinates at time t     
    (finalPolarNodalt, (finalPolarNodalt, spnSppc), (spnLppc,pcLara._2._2))
  }

  // this method uses Special Polar Nodal Variables useful for comparing with 
  // other algorithms
  def periodicCorrectionsAsCartesian(secularElemt : SGPElems[F]) :  CartesianElems[F] = {
    
    val pcLara = periodicCorrectionsNative(secularElemt)
    
    // Remark that the N Polar Nodal variable is an integral of the zonal problem
    // and, therefore, its value is always known from given initial conditions
    // H´´ = G´´cosI = N 
    import secularElemt.{I,e,n}
    val `e²` = e*e
    val N : F = sqrt(1 - `e²`) / (n pow 0.33333333) * cos(I)
    
    val unscaledCartesian = laraNonSingular2Cartesian(pcLara._1._1, N)
    val finalCartesian = scale2CartesianElems(unscaledCartesian)
    // final Cartesian coordinates at time t     
    finalCartesian
  }

  // NOTE: use this method for comparing to cartesian results
  def propagate2Cartesian(t: Minutes) : CartesianElems[F] = {  
    val secularElemt = secularCorrections(t)
    periodicCorrectionsAsCartesian(secularElemt)
  }

  def scale2CartesianElems(elems: CartesianElems[F]): CartesianElems[F] = {
      import wgs.{aE,vkmpersec}, elems._
      val (p, v) = ( aE *: pos,  vkmpersec *: vel)
      CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
  }  
}

object SGP4Lara {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) :  SGP4Lara[F] = new SGP4Lara(sec)
  
  def apply[F : Field : NRoot : Order : Trig](tle: TLE, wgs: SGPConstants[F]) :  SGP4Lara[F] =  {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs)
    val secular = BrouwerLaneSecularCorrections(elem0AndCtx, wgs)
    new SGP4Lara[F](secular)
  }
}
