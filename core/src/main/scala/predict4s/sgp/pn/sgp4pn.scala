package predict4s.sgp.pn

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord._


// compute the corrections in polar-nodal variables. 
class SGP4PN[F : Field : NRoot : Order : Trig](
 sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4(sec) with SPNLongPeriodCorrections[F] with ShortPeriodPolarNodalCorrections[F] {
 
  val wgs = sec.wgs
  val ictx = sec.inclCtx   
 
  override def periodicCorrections(secularElemt : SGPElems[F])
      :  (SpecialPolarNodal[F], SpecialPolarNodal[F]) = {
    val lppSPNContext = lppCorrections(secularElemt)
    val finalPNState = sppCorrections(lppSPNContext)
    (finalPNState, lppSPNContext._1)
  } 
  
}

object SGP4PN  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) : SGP4PN[F] = new SGP4PN(sec)
  
  def apply[F : Field : NRoot : Order : Trig](tle: TLE, wgs: SGPConstants[F]) :  SGP4PN[F] =  {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs)
    val secular = BrouwerLaneSecularCorrections(elem0AndCtx, wgs)
    new SGP4PN[F](secular)
  }  
}
