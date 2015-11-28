package predict4s.tle.vallado
import org.scalatest.FunSuite
import org.scalautils.TolerantNumerics
import predict4s.tle.{SGP72Constants,NearTLEs,TEME}
import predict4s.tle._

trait ValladoResultCheck extends NearTLEs with ValladoNearTLEsCheck with ValladoNearTLEsPVCheck { self : FunSuite => 

  implicit val wgs = SGP72Constants.tleDoubleConstants
  
  // Propagators for all TLEs
  def propags : List[SGP4Vallado[Double]]
  
  def sgpImpl : String

  // List with List of Propagation results for each TLE and all propagation times 
  def results : List[IndexedSeq[Sgp4Result]] = ((propags zip tles) zip tlesTimes) map { p  =>  // pair of (pair propagator with tle) with the propagation times   
    val sgp4 = p._1._1 ; val tle = p._1._2; val times = p._2
    for (t <- times) yield ValladoResultCheck.sgp4Result(sgp4.propagate(t), sgp4, tle)
  }
  
  test(s"${sgpImpl}: compare Position/Velocity Propagation Results with Vallado's cpp implementation for near TLEs") ({
    // call the checks for the corresponding result 
    (pvNearChecks zip results) foreach  { l =>  // pair with the lists containing the checks and the results
      (l._1 zip l._2) foreach { p => p._1(p._2) } 
    }
  })
}

//class CheckNearTLEsVallado extends FunSuite with ValladoResultCheck {
//  override def sgpImpl : String = "Vallado SGP4"  
//  override def propags : List[SGP4Vallado[Double]]  = tles map {tle => 
//    import spire.std.any.DoubleAlgebra
//    val elem0 = TEME.sgpElems(tle); 
//    SGP4Vallado[Double](elem0)
//  }
//}


class HardcodedValladoCheck extends FunSuite with NearTLEs with ValladoNearTLEsCheck with ValladoNearTLEsPVCheck {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants

  def propags : List[SGP4Vallado[Double]] = tles map {tle => 
    import spire.std.any.DoubleAlgebra
    val elem0 = TEME.sgpElems(tle); 
    SGP4Vallado[Double](elem0)
  }
  def sgpImpl : String = "Vallado SGP4"
  
  val sgps     = propags
  
  def sgp00005 = sgps(0)
  def sgp06251 = sgps(1)
  def sgp28057 = sgps(2)
  
  val results00005 = for (t <- times00005)  yield ValladoResultCheck.sgp4Result(sgp00005.propagate(t), sgps(0), tle00005)
  val results06251 = for (t <- times06251)  yield ValladoResultCheck.sgp4Result(sgp06251.propagate(t), sgps(1), tle06251)
  val results28057 = for (t <- times28057)  yield ValladoResultCheck.sgp4Result(sgp28057.propagate(t), sgps(2), tle28057)

  test(s"${sgpImpl}: compare Intermediate result t=0") ({ 
    checkIntl5(results00005(0))
    checkSgp4Init5(results00005(0))
    checkIntl6251(results06251(0))
    checkSgp4Init6251(results06251(0))
  })
  
  test(s"${sgpImpl}: compare Intermediate Propagation Results with Vallado's cpp implementation for near TLEs") ({
    
    // call the checks for the corresponding result
    check00005 zip results00005 foreach { p => p._1(p._2) }
    check06251 zip results06251 foreach { p => p._1(p._2) }
    check28057 zip results28057 foreach { p => p._1(p._2) }   
   
  })

  test(s"${sgpImpl}: compare Position/Velocity Propagation Results with Vallado's cpp implementation for near TLEs") ({
    
    // call the checks for the corresponding result
    pvCheck00005 zip results00005 foreach { p => p._1(p._2) }
    pvCheck06251 zip results06251 foreach { p => p._1(p._2) }
    pvCheck28057 zip results28057 foreach { p => p._1(p._2) }   
   
  })

} 

object ValladoResultCheck {
  
  def sgp4Result(statett: SGP4State[Double], sgp: SGP4Vallado[Double], tle: TLE) : Sgp4Result = {    
    val r = statett.orbitalState.posVel.pos
    val v = statett.orbitalState.posVel.vel
    
    import statett._
    import sgp.secularEffects.gpState.dps._
    import sgp.secularEffects.gpState.gcof._
    import sgp.secularEffects.{ocofs,lcofs}
    import sgp.secularEffects.gpState.dps.ctx._
    import sgp.secularEffects.gpState.gctx.η
    import statett._
    import sgp.secularEffects.gpState.dps.{elem=>elem0}
    
    new Sgp4Result {
      def satn = tle.satelliteNumber
      def ecco  : Double = elem0.e
      def epoch : Double = elem0.epoch
      def inclo : Double = elem0.I
    
   //   outputs : 
      def method  : Char  = if (isDeepSpace) 'd' else 'n'
    
      def  ainv  : Double    = 1 / elem0.a
      def    ao  : Double    = elem0.a
      def con41  : Double    = x3thm1   // FIXME for d
      def con42  : Double    = sgp.secularEffects.gpState.dps.ctx.con42
      def cosio  : Double    = θ
      def cosio2 : Double    = θsq
      def eccsq  : Double    = e0sq 
      def omeosq : Double    = β0sq
      def  posq  : Double    = ocofs.posq
      def    rp  : Double    = sgp.secularEffects.gpState.dps.rp
      def rteosq : Double    = β0
      def sinio  : Double    = sgp.secularEffects.gpState.dps.ctx.sinio
      def  gsto  : Double    = ocofs.gsto    
      
      // ---
      def      yr  : Int    = tle.epochyear
      def   bstar  : Double = elem0.bStar
      def   argpo  : Double = elem0.ω
      def      mo  : Double = elem0.M
      def   isimp  : Int    = if ((omeosq >= 0.0) || (no >= 0.0))
                                    if (isImpacting || isDeepSpace) 1 else 0
                              else 0
      def   aycof  : Double = statett.sppState.eaState.lppState.secularState.ocofs.aycof // FIXME for d
      def    cc1   : Double =  C1   
      def     cc4  : Double =  C4 
      def     cc5  : Double =  C5   
      def      d2  : Double =  D2
      def      d3  : Double =  D3     
      def      d4  : Double =  D4 
      import ocofs._
      def   delmo  : Double =  delM0
      def     eta  : Double =  η
      def  argpdot : Double =  ωdot 
      def   omgcof : Double =  ωcof 
      def   sinmao : Double =  sinM0.toDouble
      def   t2cof  : Double = lcofs.t2cof
      def   t3cof  : Double = lcofs.t3cof
      def   t4cof  : Double = lcofs.t4cof
      def   t5cof  : Double = lcofs.t5cof
      def  x1mth2  : Double = sgp.secularEffects.gpState.dps.ctx.x1mth2 // FIXME for d
      def  x7thm1  : Double = ocofs.x7thm1 // FIXME for d
      def   xlcof  : Double = ocofs.xlcof // FIXME for d
      def   xmcof  : Double = Mcof
//      def temp1 : Double = 3 * J2 / posq * no / 2  
//      def temp2 : Double = J2/ posq *temp1 / 2
      //def    mdot  : Double = no + 0.5 * temp1 * rteosq * con41 + 0.0625 * temp2 * rteosq * (13.0 - 78.0 * cosio2 + 137.0 * cosio2*cosio2) // tif.ocf.mdot
      // def    mdot  : Double = no + temp1 * rteosq * con41 / 2 + temp2 * rteosq * (13 - 78 * cosio2 + 137 * cosio2*cosio2) / 16 // 
      def    mdot  : Double = ocofs.mdot
      def   nodecf : Double = Ωcof
      def   nodedt : Double = omegadot     
      def        t : Double = statett.orbitalState.t
      def   nodeo  : Double = elem0.Ω
      def      no  : Double = elem0.n
      def xno = no
      val error = 0
      val x = r(0); val y = r(1) ; val z = r(2); val xdot = v(0) ; val ydot = v(1) ; val zdot = v(2) ;
      // FIXME
      def atime = statett.orbitalState.t
      def xli = 0.0
      def xni = 0.0   
    }
  }
}