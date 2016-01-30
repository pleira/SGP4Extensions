package predict4s.sgp.vallado

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord.{SGPElems,SpecialPolarNodal}
/*
class SGP4Vallado[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4(sec) with LyddaneLongPeriodCorrections[F] with ShortPeriodPolarNodalCorrections[F] {
 
  val wgs = sec.wgs
  val ictx = sec.inclCtx

  
  override def periodicCorrections(secularElemt : SGPElems[F])
      :  (SpecialPolarNodal[F], SpecialPolarNodal[F]) = {
    val lppSPNContext = lppCorrections(secularElemt)
    val finalPNState = sppCorrections(lppSPNContext)
    (finalPNState, lppSPNContext._1)
  }
  
}

object SGP4Vallado  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) :  SGP4Vallado[F] = new SGP4Vallado(sec)
  
  def apply[F : Field : NRoot : Order : Trig](tle: TLE, wgs: SGPConstants[F]) :  SGP4Vallado[F] =  {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs)
    val secular = BrouwerLaneSecularCorrections(elem0AndCtx, wgs)
    new SGP4Vallado[F](secular)
  }
}
*/