package predict4s.tle

import predict4s.tle.vallado.SGP4Vallado

/*
 * The traits/classes here are used to compare our results with those from Vallado's software, using the same variable names as in Vallado.
 */
trait Initl {
  def satn: Int
  def ecco : Double
  def epoch : Double
  def inclo : Double
  def    no  : Double   
 //   outputs : 
  def  ainv  : Double     ; def    ao  : Double  ; def con41 : Double  ; def con42  : Double  ; def cosio  : Double  ; def cosio2  : Double    
  def eccsq  : Double  ; def omeosq : Double     ; def  posq  : Double ; def    rp  : Double  ; def rteosq : Double  ; 
  def sinio  : Double  ; def  gsto  : Double     ; 
}


trait  Sgp4Init {
  def satn: Int; def      yr  : Int      ; def   bstar  : Double; def    ecco  : Double; def   epoch  : Double; def   argpo  : Double; 
  def inclo  : Double     ; def      mo  : Double   ; 
  // in and out variables 
  def    no  : Double     ; 
  //    outputs  :
  def   isimp  : Int      ; def  method  : Char       ; def   aycof  : Double    ; 
  def con41  : Double     ; def    cc1   : Double     ; def     cc4  : Double    ; 
  def   cc5  : Double     ; def      d2  : Double     ; def      d3  : Double    ; def      d4  : Double     ; def   delmo  : Double; 
  def   eta  : Double     ; def  argpdot : Double     ; def   omgcof : Double; 
  def sinmao : Double     ; def   t2cof  : Double     ; def   t3cof  : Double    ; 
  def t4cof  : Double     ; def   t5cof  : Double     ; def    gsto  : Double    ; def  x1mth2  : Double ; def  x7thm1  : Double ; def   xlcof  : Double ; 
  def xmcof  : Double     ; def    mdot  : Double     ; def   nodecf : Double    ; def   nodedt : Double  ; 
  //   in and outputs from deep space satellites :
  def     t  : Double     ; def   nodeo  : Double     ; 
}

trait Sgp4Vars extends Initl  with Sgp4Init  

trait Sgp4Near extends Sgp4Vars {
  def xno : Double ;
  def error: Int; def x : Double; def y : Double; def z: Double; def xdot: Double ; def ydot : Double; def zdot: Double;
  def atime: Double; def xli : Double; 
  def xni: Double;
}


case class Sgp4Result(
    sgp: SGP4Vallado[Double], 
    statett: (TEME.CartesianElems[Double], TEME.CartesianElems[Double], (Double,Double,Double,Double,Double,Double), 
        SGP4[Double]#LongPeriodPeriodicState, TEME.SGPElems[Double], SGP4[Double]#EccentricAnomalyState), tle: TLE, t: Double) 
        extends Sgp4Near {   
    val posVel = statett._1 
    val r = posVel.pos
    val v = posVel.vel
    
    val secElemt = statett._5
    
    import sgp.elem0
    import sgp.geoPot._
    import sgp.isImpacting
    import sgp.{secularFreqs,laneCoefs}
//    import sgp.geoPot.dps.ctx._
//    import sgp.geoPot.gctx.η
    import statett._
    

      def satn = tle.satelliteNumber
      def ecco  : Double = elem0.e
      def epoch : Double = elem0.epoch
      def inclo : Double = elem0.I
    
   //   outputs : 
      def method  : Char  = 'n' // FIXME if (isDeepSpace) 'd' else 'n'
    
      def  ainv  : Double    = 1 / elem0.a
      def    ao  : Double    = elem0.a
      def con41  : Double    = sgp.x3thm1   // FIXME for d
      def con42  : Double    = sgp.con42
      def cosio  : Double    = sgp.c // θ
      def cosio2 : Double    = sgp.`c²` // θsq
      def eccsq  : Double    = sgp.`e²` // e0sq 
      def omeosq : Double    = 1 - sgp.`e²` //  β0sq
      def  posq  : Double    = secularFreqs.posq
      def    rp  : Double    = sgp.rp
      def rteosq : Double    = math.sqrt(1 - sgp.`e²`) // β0
      def sinio  : Double    = sgp.s
      def  gsto  : Double    = secularFreqs.gsto    
      
      // ---
      def      yr  : Int    = tle.epochyear
      def   bstar  : Double = elem0.bStar
      def   argpo  : Double = elem0.ω
      def      mo  : Double = elem0.M
      def   isimp  : Int    = if ((omeosq >= 0.0) || (no >= 0.0))
                                      if (isImpacting) 1 else 0
                                    // FIXME: should be if (isImpacting || isDeepSpace) 1 else 0
                              else 0
      def   aycof  : Double = secularFreqs.aycof // FIXME for d
      def    cc1   : Double =  C1   
      def     cc4  : Double =  C4 
      def     cc5  : Double =  C5   
      def      d2  : Double =  D2
      def      d3  : Double =  D3     
      def      d4  : Double =  D4 
      def   delmo  : Double =  secularFreqs.delM0
      def     eta  : Double =  sgp.η
      def  argpdot : Double =  secularFreqs.ωdot 
      def   omgcof : Double =  secularFreqs.ωcof 
      def   sinmao : Double = secularFreqs.sinM0.toDouble
      def   t2cof  : Double = laneCoefs.t2cof
      def   t3cof  : Double = laneCoefs.t3cof
      def   t4cof  : Double = laneCoefs.t4cof
      def   t5cof  : Double = laneCoefs.t5cof
      def  x1mth2  : Double = sgp.x1mth2 // FIXME for d
      def  x7thm1  : Double = secularFreqs.x7thm1 // FIXME for d
      def   xlcof  : Double = secularFreqs.xlcof // FIXME for d
      def   xmcof  : Double = secularFreqs.Mcof
//      def temp1 : Double = 3 * J2 / posq * no / 2  
//      def temp2 : Double = J2/ posq *temp1 / 2
      //def    mdot  : Double = no + 0.5 * temp1 * rteosq * con41 + 0.0625 * temp2 * rteosq * (13.0 - 78.0 * cosio2 + 137.0 * cosio2*cosio2) // tif.ocf.mdot
      // def    mdot  : Double = no + temp1 * rteosq * con41 / 2 + temp2 * rteosq * (13 - 78 * cosio2 + 137 * cosio2*cosio2) / 16 // 
      def    mdot  : Double = secularFreqs.mdot
      def   nodecf : Double = secularFreqs.Ωcof
      def   nodedt : Double = secularFreqs.omegadot     
      def   nodeo  : Double = elem0.Ω
      def      no  : Double = elem0.n
      def xno = no
      val error = 0
      val x = r(0); val y = r(1) ; val z = r(2); val xdot = v(0) ; val ydot = v(1) ; val zdot = v(2) ;
      // FIXME
      def atime = t
      def xli = 0.0
      def xni = 0.0   
  }