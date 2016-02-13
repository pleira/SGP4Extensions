package predict4s
package sgp

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality
import predict4s.coord._

class GeoPotentialStateCheck extends FunSuite with TLE00005 with TLE06251  {
  
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(1E-9)
  implicit val wgs = SGP72Constants.tleDoubleConstants
  def sgpImpl : String = "Vallado SGP4"
  
  def buildGeoPotential(tle: TLE) = {
    import spire.implicits._
    val elem0Ctx = SGPElemsConversions.sgpElemsAndContext(tle, wgs).get
    val geoPotCtx = BrouwerLaneSecularCorrections.geoPotentialCoefs(elem0Ctx)
    (elem0Ctx, geoPotCtx)
  }

  test(s"${sgpImpl}: compare GeoPotentialState for 00005 when t=0") {
    val (elem0Ctx, geoPotCtx) = buildGeoPotential(tle00005)
    assert(elem0Ctx.isImpacting == false)
    assert(elem0Ctx.isDeepSpace == false)
    checkSgp4GeoPotential_5(elem0Ctx, geoPotCtx)
  }
  
  test(s"${sgpImpl}: compare GeoPotentialState for 06251 when t=0") {
    val (elem0Ctx, geoPotCtx) = buildGeoPotential(tle06251)
    assert(elem0Ctx.isImpacting == false)
    assert(elem0Ctx.isDeepSpace == false)
    checkSgp4GeoPotential_06251(elem0Ctx, geoPotCtx)
  }
  
  def checkSgp4GeoPotential_5(elem0Ctx: SGPElemsCtx[Double], geoPot: GeoPotentialCtx[Double]) = {
    import elem0Ctx.{elem,iCtx,eCtx,wgs,rp} 
    import elem._,iCtx.`3c²-1`,eCtx.`β0²` // rteosq
    import geoPot.{_1=>gcoef,_2=>geoctx},gcoef._,geoctx._
    val ωcof = C3*bStar*math.cos(ω)
   
    assert(  n      ===     0.047206302); assert(   a  ===     1.353899821); 
    assert(  e      ===     0.185966700); 
    assert(  I      ===     0.598092919); assert(   ω  ===     5.790416027); 
    assert(  Ω      ===     6.086385471); assert(   M  ===     0.337309313);
    assert(  bStar  ===     0.000028098); assert( epoch  === 18441.784950620);
    assert(    rp  ===     1.102119539); 
    assert( `3c²-1`  ===     1.048865088); assert(  `β0²` ===    0.965416386); assert(   ωcof ===     0.000000000 ); 
    assert(    C1  ===     0.000000000); assert(    C4  ===     0.000000526); assert(    C5  ===     0.000016465);  
    assert(    D2  ===     0.000000000); assert(    D3  ===     0.000000000); assert(    D4  ===     0.000000000);
  } 


  def checkSgp4GeoPotential_06251(elem0Ctx: SGPElemsCtx[Double], geoPot: GeoPotentialCtx[Double]) = {
    import elem0Ctx.{elem,iCtx,eCtx,wgs,rp} 
    import elem._,iCtx.`3c²-1`,eCtx.`β0²` // rteosq
    import geoPot.{_1=>gcoef,_2=>geoctx},gcoef._,geoctx._
    val ωcof = C3*bStar*math.cos(ω)
    
    assert(  n      ===     0.067918037); assert(   a  ===     1.062338933); 
    assert(  e      ===     0.003003500); 
    assert(  I      ===     1.013301512); assert(   ω  ===     2.428744337); 
    assert(  Ω      ===     0.943219561); assert(   M  ===     3.860413487);
    assert(  bStar  ===     0.000128080); assert( epoch  === 20630.824120140);
    assert(    rp  ===     1.059148199); 
    assert( `3c²-1`  ===  -0.160280193); assert(  `β0²` ===    0.999990979); assert(   ωcof ===    -0.000000052 )
    assert(    C1  ===     0.000000003); assert(    C4  ===     0.000005200); assert(    C5  ===     0.000650194);  
    assert(    D2  ===     0.000000000); assert(    D3  ===     0.000000000); assert(    D4  ===     0.000000000);
  } 

  
}

