package predict4s
package sgp
package ref

import org.scalactic.Or
import org.scalactic.Good
import org.scalactic.Bad
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.sgp._
import predict4s.coord._
import predict4s.coord.LNSConversions._
import predict4s.coord.SGPElemsConversions._
import predict4s.coord.CoordinatesConversions._

class SGP4Lara[F : Field : NRoot : Order : Trig](
  sec : BrouwerLaneSecularCorrections[F]
  ) extends SGP4(sec) with LaraFirstOrderCorrections[F] with SimpleKeplerEq {
   
//   override def propagate2CartesianContext(t: Minutes) : SGPPropResult[F] = 
//    for {
//      secularElemt <- secularCorrections(t)
//      lns <- periodicCorrectionsLNS(secularElemt)
//      unitpv = lns2UnitCartesian(lns)
//      /// FIXME
//    	_N = lns.`Θ/r`*lns.r*secularElemt._2.c // ictx.c    // N = Θ*cosI , which remains constant      
//      spn = lns2spn(lns, _N)
//      pv = scaleUnitCartesians(unitpv,spn.r)
//    } yield (pv, unitpv, spn) 
    
  override def periodicCorrections(secularElemt : SGPSecularCtx[F]) : SGPSPNResult[F] = {
    val elem = secularElemt._1
		val ictx = secularElemt._2    
    for {
    	eaState <- solveKeplerEq(elem.e, elem.M)
    	spnSecular <- sgpelems2spn(eaState, secularElemt)
    	_N = spnSecular.`Θ/r`*spnSecular.r*ictx.c    // N = Θ*cosI , which remains constant
      lns = spn2lns(spnSecular, ictx)
      corr = allCorrections(lns, secularElemt)   
      finalSPN = lns2spn(corr, _N)
    } yield finalSPN
  }
    
 def periodicCorrectionsWithN(secularElemt : SGPSecularCtx[F]) : SGPSPNResult[F] = {
    val elem = secularElemt._1
		val ictx = secularElemt._2    
    for {
    	eaState <- solveKeplerEq(elem.e, elem.M)
    	spnSecular <- sgpelems2spn(eaState, secularElemt)
    	_N = spnSecular.`Θ/r`*spnSecular.r*ictx.c    // N = Θ*cosI , which remains constant
      lns = spn2lns(spnSecular, ictx)
      corr = allCorrections(lns, secularElemt)   
      finalSPN = lns2spn(corr, _N)
    } yield finalSPN
  }

  // periodic corrections results in Lara Non Singular (LNS)
  def periodicCorrectionsLNS(secularElemt : SGPSecularCtx[F]): SGPLNSResult[F] = {
    val elem = secularElemt._1
		val ictx = secularElemt._2    
    for {
    	eaState <- solveKeplerEq(elem.e, elem.M)
    	spnSecular <- sgpelems2spn(eaState, secularElemt)
 //   	_N = spnSecular.`Θ/r`*spnSecular.r*ictx.c    // N = Θ*cosI , which remains constant
      lns = spn2lns(spnSecular, ictx)
      flns = allCorrections(lns, secularElemt)
    } yield flns
  }
   
  /*
   * This is used/compare to test the LPP corrections in SPN coordinates 
   */
  def propagateToSPNLPP(secularElemt : SGPSecularCtx[F]) : LPPSPNResult[F] = {
    val elem = secularElemt._1
		val ictx = secularElemt._2    
    val wgs = secularElemt._3
    for {
      eaState <- solveKeplerEq(elem.e, elem.M)
      spnSecular <- sgpelems2spn(eaState, secularElemt)
      _N = spnSecular.`Θ/r`*spnSecular.r*ictx.c    // cosI = N/Θ =>  
      lns = spn2lns(spnSecular, ictx)    
      lalppcorr = lppCorrections(lns, secularElemt)
      lppcorr1 = lns2spn(lalppcorr._1, _N)
    } yield (lppcorr1, LongPeriodContext(0.as[F],0.as[F],0.as[F],0.as[F],0.as[F],0.as[F]), secularElemt)    
  }


  /*
   * This is used to test/compare the LPP corrections in CPN coordinates 
   */
  def cpnLPPCorrections(secularElemt : SGPSecularCtx[F]) : LPPCPNResult[F] = {
    val elem = secularElemt._1
		val ictx = secularElemt._2    
    val wgs = secularElemt._3
    for {
      eaState <- solveKeplerEq(elem.e, elem.M)
      spnSecular <- sgpelems2spn(eaState, secularElemt)
      _N = spnSecular.`Θ/r`*spnSecular.r*ictx.c    // cosI = N/Θ, N remains a constant 
      lns = spn2lns(spnSecular, ictx)    
      lalppcorr = lppCorrections(lns, secularElemt)
      lppcorr1 = lns2cpn(lalppcorr._1,_N)
    } yield lppcorr1    
  }

}


object SGP4Lara {
  
  def apply[F : Field : NRoot : Order : Trig](sec: BrouwerLaneSecularCorrections[F]) :  SGP4Lara[F] = new SGP4Lara(sec)
  
  def build[F : Field : NRoot : Order : Trig](tle: TLE, wgs: SGPConstants[F]) :  SGP4Lara[F] Or ErrorMessage = for {
    elem0AndCtx <- SGPElemsConversions.sgpElemsAndContext(tle, wgs)
    secular = BrouwerLaneSecularCorrections(elem0AndCtx)
  } yield SGP4Lara[F](secular)
}