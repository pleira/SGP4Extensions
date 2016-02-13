package predict4s
package sgp
package ref

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord._
import org.scalactic.Or
import predict4s.coord.CoordinatesConversions._

trait LongPeriodSPNCorrections[F] {
  def lppCorrections(secularElemt : SGPSecularCtx[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]) 
      : LPPSPNResult[F]
}

abstract class SGP4WithSPNCorrections[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4(sec) with LongPeriodSPNCorrections[F] with ShortPeriodPolarNodalCorrections[F] {

  type PC[_] = SGPSPNCtx[F]

  override def propagate(t: Minutes): SGPPropResult[F] = propagate2CartesianContext(t)
    
  def propagate2CartesianContext(t: Minutes) : SGPPropResult[F] = 
    for {
      spnCtx <- propagate2SPNContext(t)
      finalSPN = spnCtx._1._1
      unitpv = polarNodal2UnitCartesian(finalSPN)
      pv = scale2CartesianElems(unitpv, finalSPN)      
    } yield (pv, unitpv, spnCtx) 
  
  def propagate2SPNContext(t: Minutes): SGPCorrPropResult[F] = 
    for {
     sc <- secularCorrections(t)
     pc <- periodicCorrections(sc)
    } yield (pc, sc)
  
  override def periodicCorrections(secularElemt : SGPSecularCtx[F]) :  SGPSPNResult[F] = 
    for {
      lc <- lppCorrections(secularElemt)
      finalSPN = sppCorrections(lc)
    } yield (finalSPN, lc._1)
    
}

class SGP4Vallado[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4WithSPNCorrections(sec) with LyddaneLongPeriodCorrections[F]

class SGP4ValladoLong[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4WithSPNCorrections(sec) with LyddaneExtraLongPeriodCorrections[F]

class SGP4PN[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4WithSPNCorrections(sec) with SPNLongPeriodCorrections[F] 

object SGP4Vallado  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) : SGP4Vallado[F] = new SGP4Vallado(sec)
  
  def build[F : Field : NRoot : Order : Trig](tle: TLE, wgs: SGPConstants[F]) : SGP4Vallado[F] Or ErrorMessage  =  for {
    elem0AndCtx <- SGPElemsConversions.sgpElemsAndContext(tle, wgs)
    secular = BrouwerLaneSecularCorrections(elem0AndCtx)
  } yield SGP4Vallado[F](secular)
}

object SGP4PN  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) :  SGP4PN[F] = new SGP4PN(sec)
  
  def build[F : Field : NRoot : Order : Trig](tle: TLE, wgs: SGPConstants[F]) : SGP4PN[F] Or ErrorMessage  =  for {
    elem0AndCtx <- SGPElemsConversions.sgpElemsAndContext(tle, wgs)
    secular = BrouwerLaneSecularCorrections(elem0AndCtx)
  } yield SGP4PN[F](secular)

}

object SGP4ValladoLong  {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) :  SGP4ValladoLong[F] = new SGP4ValladoLong(sec)
  
  def build[F : Field : NRoot : Order : Trig](tle: TLE, wgs: SGPConstants[F]) : SGP4ValladoLong[F] Or ErrorMessage = for {
      elem0AndCtx <- SGPElemsConversions.sgpElemsAndContext(tle, wgs)
      secular = BrouwerLaneSecularCorrections(elem0AndCtx)
    } yield SGP4ValladoLong[F](secular)

}