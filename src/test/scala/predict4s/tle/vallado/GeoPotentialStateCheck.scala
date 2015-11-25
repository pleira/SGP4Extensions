package predict4s.tle.vallado
import org.scalatest.FunSuite
import org.scalautils.TolerantNumerics
import predict4s.tle.{SGP72Constants,TLE00005,TEME}
import predict4s.tle.TLE
import predict4s.tle.vallado.DpTransform.DpState
import predict4s.tle.NearTLEs


trait  ValladoTLE00005GeoPotentialCheck { self :  FunSuite => 
  
  implicit def doubleEqualityTLE00005 = TolerantNumerics.tolerantDoubleEquality(1E-9)
   
  def checkSgp4GeoPotential_5(ini: GeoPotentialState[Double]) = {
    import ini._
    import gcof._,dps.elem._,dps.perige,dps.rp,dps.isImpactingBis,dps.ctx.con41,dps.ctx.omeosq // rteosq
    val ωcof = C3*bStar*math.cos(ω)
   
    assert(  n      ===     0.047206302); assert(   a  ===     1.353899821); 
    assert(  e      ===     0.185966700); 
    assert(  i      ===     0.598092919); assert(   ω  ===     5.790416027); 
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

  def checkSgp4GeoPotential_06251(ini: GeoPotentialState[Double]) = {
    import ini._
    import gcof._,dps.elem._,dps.perige,dps.rp,dps.isImpactingBis,dps.ctx.con41,dps.ctx.omeosq // rteosq
    val ωcof = C3*bStar*math.cos(ω)
    assert(  n      ===     0.067918037); assert(   a  ===     1.062338933); 
    assert(  e      ===     0.003003500); 
    assert(  i      ===     1.013301512); assert(   ω  ===     2.428744337); 
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
  
  test(s"${sgpImpl}: compare GeoPotentialState for 00005 when t=0") ({
    val gps = buildGeoPotential(tle00005)
    assert(gps.dps.isImpacting == false)
    assert(gps.dps.isDeepSpace == false)
    checkSgp4GeoPotential_5(gps)
  })
  
  test(s"${sgpImpl}: compare GeoPotentialState for 06251 when t=0") ({
    val gps = buildGeoPotential(tle06251)
    assert(gps.dps.isImpacting == false)
    assert(gps.dps.isDeepSpace == false)
    checkSgp4GeoPotential_06251(gps)
  })
  
  def buildGeoPotential(tle: TLE) = {
    import spire.implicits._
    val ini : TEME.SGPElems[Double] = TEME.sgpElems(tle)
    val elemsdp = DpTransform.dpState(ini)
    GeoPotentialState(elemsdp)
  }
}

