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
    
    // final state in Polar Nodal coordinates at time t         
    val finalPolarNodalt = laraNonSingular2SpecialPolarNodal(pcLara._1._1, secularElemt.I) 
    val spnSppc = laraNonSingular2SpecialPolarNodal(pcLara._1._2, secularElemt.I)
    val spnLppc = laraNonSingular2SpecialPolarNodal(pcLara._2._1, secularElemt.I)
    
    (finalPolarNodalt, (finalPolarNodalt, spnSppc), (spnLppc,pcLara._2._2))
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

