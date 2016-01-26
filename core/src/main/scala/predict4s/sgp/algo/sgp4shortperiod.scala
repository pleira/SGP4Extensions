package predict4s.sgp.algo

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord._

trait LongPeriodCorrections[F] {
  def lppCorrections(secularElemt : SGPElems[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F])  : (SpecialPolarNodal[F], LongPeriodContext[F])
}

abstract class SGP4ShortPeriodPNCorrections[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4(sec) with LongPeriodCorrections[F] with ShortPeriodPolarNodalCorrections[F] {
 
  val wgs = sec.wgs
  val ictx = sec.inclCtx
  
  override def periodicCorrections(secularElemt : SGPElems[F]) :  (SpecialPolarNodal[F], SpecialPolarNodal[F]) = {
    val lppSPNContext = lppCorrections(secularElemt)
    val finalPNState = sppCorrections(lppSPNContext)
    (finalPNState, lppSPNContext._1)
  }
    
}

class SGP4Vallado[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4ShortPeriodPNCorrections(sec) with LyddaneLongPeriodCorrections[F]

class SGP4ValladoLong[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4ShortPeriodPNCorrections(sec) with LyddaneExtraLongPeriodCorrections[F]

class SGP4PN[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4ShortPeriodPNCorrections(sec) with SPNLongPeriodCorrections[F]

object SGP4Vallado  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) :  SGP4Vallado[F] = new SGP4Vallado(sec)
  
  def apply[F : Field : NRoot : Order : Trig](tle: TLE, wgs: SGPConstants[F]) :  SGP4Vallado[F] =  {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs)
    val secular = BrouwerLaneSecularCorrections(elem0AndCtx, wgs)
    new SGP4Vallado[F](secular)
  }
}

object SGP4PN  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) :  SGP4PN[F] = new SGP4PN(sec)
  
  def apply[F : Field : NRoot : Order : Trig](tle: TLE, wgs: SGPConstants[F]) :  SGP4PN[F] =  {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs)
    val secular = BrouwerLaneSecularCorrections(elem0AndCtx, wgs)
    new SGP4PN[F](secular)
  }
}



object SGP4ValladoLong  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) :  SGP4ValladoLong[F] = new SGP4ValladoLong(sec)
  
  def apply[F : Field : NRoot : Order : Trig](tle: TLE, wgs: SGPConstants[F]) :  SGP4ValladoLong[F] =  {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs)
    val secular = BrouwerLaneSecularCorrections(elem0AndCtx, wgs)
    new SGP4ValladoLong[F](secular)
  }
}