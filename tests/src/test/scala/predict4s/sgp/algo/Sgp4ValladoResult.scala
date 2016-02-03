package predict4s.sgp.algo

import predict4s.sgp.Sgp4Near
import predict4s.coord.CartesianElems
import predict4s.sgp.Sgp4Result
import predict4s.coord._

case class Sgp4ValladoResult(
    sgp: SGP4Vallado[Double], 
    statett: (CartesianElems[Double], CartesianElems[Double], ((SpecialPolarNodal[Double], SpecialPolarNodal[Double]), 
 (SGPElems[Double], InclinationCtx[Double], SGPConstants[Double]))), tle: TLE, t: Double) 
        extends Sgp4Result[Double] {
  val posVel = statett._1 
  val r = posVel.pos
  val v = posVel.vel
    
  import sgp.sec.elem0Ctx
  import elem0Ctx._,iCtx._,eCtx._
  // import elem0Ctx.isImpacting
  import sgp.sec.geoPotCtx.{_1=>gcoef,_2=>geoctx}
  import gcoef._
  import sgp.sec.{dragCoefs,secularFreqs,laneCoefs}
  val error = 0
  val x = r(0); 
  val y = r(1) ; 
  val z = r(2); 
  val xdot = v(0) ; 
  val ydot = v(1) ; 
  val zdot = v(2) ;
  def satn = tle.satelliteNumber
  def ecco  : Double = elem.e
  def epoch : Double = elem.epoch
  def inclo : Double = elem.I
  
 //   outputs : 
  // def method  : Char  = 'n' // FIXME if (isDeepSpace) 'd' else 'n'
  
  def  ainv  : Double    = 1 / elem.a
  def    ao  : Double    = elem.a
//  def con41  : Double    = x3thm1   // FIXME for d
  def cosio  : Double    = c // θ
  def cosio2 : Double    = `c²` // θsq
  def eccsq  : Double    = `e²` // e0sq 
  def omeosq : Double    = 1 - `e²` //  β0sq
  //def  posq  : Double    = `p²` // posq
  def    rp  : Double    = sgp.sec.elem0Ctx.rp
  def rteosq : Double    = math.sqrt(1 - `e²`) // β0
  def sinio  : Double    = s
  def  gsto  : Double    = sgp.gsto    
  
  // ---
  def      yr  : Int    = tle.epochyear
  def   bstar  : Double = elem.bStar
  def   argpo  : Double = elem.ω
  def      mo  : Double = elem.M
  def   isimp  : Int    = if ((omeosq >= 0.0) || (no >= 0.0))
                                  if (isImpacting) 1 else 0
                                // FIXME: should be if (isImpacting || isDeepSpace) 1 else 0
                          else 0
  def    cc1   : Double =  C1   
  def     cc4  : Double =  C4 
  def     cc5  : Double =  C5   
  def      d2  : Double =  D2
  def      d3  : Double =  D3     
  def      d4  : Double =  D4 
  def     eta  : Double =  geoctx.η
  def  argpdot : Double =  secularFreqs.ωdot 
  def   omgcof : Double =  dragCoefs.ωcof 
  def   sinmao : Double = math.sin(elem.M)
  def   t2cof  : Double = laneCoefs.T2
  def   t3cof  : Double = laneCoefs.T3
  def   t4cof  : Double = laneCoefs.T4
  def   t5cof  : Double = laneCoefs.T5
  def  x1mth2  : Double = 1 - `c²` // FIXME for d
  def  x7thm1  : Double = 7*`c²` - 1   // x7thm1 // FIXME for d
  def   xmcof  : Double = dragCoefs.Mcof
//      def temp1 : Double = 3 * J2 / posq * no / 2  
//      def temp2 : Double = J2/ posq *temp1 / 2
  //def    mdot  : Double = no + 0.5 * temp1 * rteosq * con41 + 0.0625 * temp2 * rteosq * (13.0 - 78.0 * cosio2 + 137.0 * cosio2*cosio2) // tif.ocf.mdot
  // def    mdot  : Double = no + temp1 * rteosq * con41 / 2 + temp2 * rteosq * (13 - 78 * cosio2 + 137 * cosio2*cosio2) / 16 // 
  def    mdot  : Double = secularFreqs.Mdot
  def   nodecf : Double = dragCoefs.Ωcof
  def   nodedt : Double = secularFreqs.Ωdot     
  def   nodeo  : Double = elem.Ω
  def      no  : Double = elem.n
  // FIXME
  def atime : Double = t
    
}