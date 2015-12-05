package predict4s.tle
import org.scalatest.FunSuite
import org.scalautils.TolerantNumerics


trait  ValladoTLE00005GeoPotentialCheck { self :  FunSuite => 
  
  implicit def doubleEqualityTLE00005 = TolerantNumerics.tolerantDoubleEquality(1E-9)
   
  def checkSgp4GeoPotential_5(elem0: SGPElems[Double], context0: Context0[Double], geoPot: GeoPotentialCoefs[Double], gctx: GeoPotentialContext[Double], rp: Double, perigeeHeight: Double, isImpacting: Boolean) = {
    import geoPot._,elem0._,context0.{con41,omeosq} // rteosq
    val ωcof = C3*bStar*math.cos(ω)
   
    assert(  n      ===     0.047206302); assert(   a  ===     1.353899821); 
    assert(  e      ===     0.185966700); 
    assert(  I      ===     0.598092919); assert(   ω  ===     5.790416027); 
    assert(  Ω      ===     6.086385471); assert(   M  ===     0.337309313);
    assert(  bStar  ===     0.000028098); assert( epoch  === 18441.784950620);
    assert(    rp  ===     1.102119539); 
    assert( con41  ===     1.048865088); assert(  omeosq ===    0.965416386); assert(   ωcof ===     0.000000000 ); 
    assert(    C1  ===     0.000000000); assert(    C4  ===     0.000000526); assert(    C5  ===     0.000016465);  
    assert(    D2  ===     0.000000000); assert(    D3  ===     0.000000000); assert(    D4  ===     0.000000000);
  } 
}

trait  ValladoTLE06251GeoPotentialCheck { self :  FunSuite => 

  implicit def doubleEqualityTLE06251 = TolerantNumerics.tolerantDoubleEquality(1E-9)

  def checkSgp4GeoPotential_06251(elem0: SGPElems[Double], context0: Context0[Double], geoPot: GeoPotentialCoefs[Double], gctx: GeoPotentialContext[Double], rp: Double, perigeeHeight: Double, isImpacting: Boolean) = {
    import geoPot._,elem0._,context0.{con41,omeosq} // rteosq
    val ωcof = C3*bStar*math.cos(ω)
    assert(  n      ===     0.067918037); assert(   a  ===     1.062338933); 
    assert(  e      ===     0.003003500); 
    assert(  I      ===     1.013301512); assert(   ω  ===     2.428744337); 
    assert(  Ω      ===     0.943219561); assert(   M  ===     3.860413487);
    assert(  bStar  ===     0.000128080); assert( epoch  === 20630.824120140);
    assert(    rp  ===     1.059148199); 
    assert( con41  ===    -0.160280193); assert(  omeosq ===    0.999990979); assert(   ωcof ===    -0.000000052 )
    assert(    C1  ===     0.000000003); assert(    C4  ===     0.000005200); assert(    C5  ===     0.000650194);  
    assert(    D2  ===     0.000000000); assert(    D3  ===     0.000000000); assert(    D4  ===     0.000000000);
  } 
}

class GeoPotentialStateCheck extends FunSuite with NearTLEs with ValladoTLE00005GeoPotentialCheck with ValladoTLE06251GeoPotentialCheck {

  implicit val wgs = SGP72Constants.tleDoubleConstants

  def sgpImpl : String = "Vallado SGP4"
  
  def fixture = new {
    val model = new SGP4Factory {
        def buildGeoPotential(tle: TLE) = {
          import spire.implicits._
          val elemTLE : (SGPElems[Double], Context0[Double]) = SGPElems.sgpElemsAndContext(tle)
          val (elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting) = geoPotentialCoefsAndContexts(elemTLE, wgs)
          (elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting)
        }
    }
  }
  
  test(s"${sgpImpl}: compare GeoPotentialState for 00005 when t=0") {
    val f = fixture
    val (elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting) = f.model.buildGeoPotential(tle00005)
    assert(isImpacting == false)
    //assert(isDeepSpace == false)
    checkSgp4GeoPotential_5(elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting)
  }
  
  test(s"${sgpImpl}: compare GeoPotentialState for 06251 when t=0") {
    val f = fixture
    val (elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting) = f.model.buildGeoPotential(tle06251)
    assert(isImpacting == false)
    //assert(gps.dps.isDeepSpace == false)
    checkSgp4GeoPotential_06251(elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting)
  }
  
}

