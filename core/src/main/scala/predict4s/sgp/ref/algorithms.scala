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
import predict4s.coord.SGPElemsConversions._


abstract class SGP4WithSPNCorrections[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4(sec) with ShortPeriodPolarNodalCorrections[F] {

  override def periodicCorrections(secularElemt : SGPSecularCtx[F]) :  SGPSPNResult[F] = 
    for {
      lc <- lppCorrections(secularElemt)
      finalSPN = sppCorrections(lc)
    } yield finalSPN
  
   def lppCorrections(secularElemt : SGPSecularCtx[F]) : LPPSPNResult[F]
}

class SGP4Vallado[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4WithSPNCorrections(sec) with LyddaneLongPeriodCorrections[F] with TwoTermsKeplerEq {
  
  def lppCorrections(secularElemt : SGPSecularCtx[F]) : LPPSPNResult[F] = {
    // long period corrections in Lyddane's coordinates
    val lyddaneElems = lylppCorrections(secularElemt)
    for {
      // To transform to Special Polar Nodals, get the eccentric anomaly
      eaState <- solveKeplerEq(lyddaneElems)
      spnctx <- LyddaneConversions.lyddane2SpecialPolarNodal(eaState, lyddaneElems)
    } yield (spnctx._1, spnctx._2, secularElemt)
  }  
}

class SGP4ValladoLong[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4WithSPNCorrections(sec) with LyddaneExtraLongPeriodCorrections[F] with TwoTermsKeplerEq {
 
  def lppCorrections(secularElemt : SGPSecularCtx[F]) : LPPSPNResult[F] = {
    val lyddaneElems = lylppCorrections(secularElemt)
    for {
      // long period corrections in Lyddane's coordinates
      // To transform to Special Polar Nodals, get the eccentric anomaly
      eaState <- solveKeplerEq(lyddaneElems)
      spnctx <- LyddaneConversions.lyddane2SpecialPolarNodal(eaState, lyddaneElems)
    } yield (spnctx._1, spnctx._2, secularElemt)
  }  
}

class SGP4PN[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4WithSPNCorrections(sec) with SPNLongPeriodCorrections[F] with SimpleKeplerEq {
  
  def lppCorrections(secularElemt : SGPSecularCtx[F])  =
      propagateToSPNLPP(secularElemt)
  
  /**
   * long period corrections in SpecialPolarNodal coordinates
   */
   def propagateToSPNLPP(secularElemt : SGPSecularCtx[F]) : LPPSPNResult[F] = {
    val elem = secularElemt._1
    val wgs = secularElemt._3
    for {
      eaState <- solveKeplerEq(elem.e, elem.M)
      spnSecular <- sgpelems2SpecialPolarNodal(eaState, secularElemt)
      lppcorr = lppSPNCorrections((spnSecular, secularElemt))
    } yield (lppcorr._1, lppcorr._2, secularElemt)    
  }
  
  /**
   * long period corrections in CSpecialPolarNodal coordinates
   */
  def propagateToCPNLPP(secularElemt : SGPSecularCtx[F]) : LPPCPNResult[F] = {
    val elem = secularElemt._1
    val wgs = secularElemt._3
    for {
      eaState <- solveKeplerEq(elem.e, elem.M)
      spnSecular <- sgpelems2SpecialPolarNodal(eaState, secularElemt)
      lppcorr = lppCPNCorrections((spnSecular, secularElemt))
    } yield (lppcorr._1, lppcorr._2, secularElemt)    
  }  
}

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