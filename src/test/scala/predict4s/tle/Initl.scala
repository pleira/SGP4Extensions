package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._

/*
 * The traits/classes here are used to compare our results with those from Vallado's software, using the same variable names.
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

trait Sgp4Result extends Sgp4Vars {
//def aycof  : Double ; def bstar  : Double ;  
//def con41 : Double ; def cc1  : Double ;    
//def cc4 : Double ; def cc5  : Double ;   
//def d2 : Double ; def d3  : Double ;    
//def d4 : Double ; def delmo  : Double ; 
//def ecco : Double ; def eta  : Double ;     
//def argpo : Double ; def argpdot : Double ; 
//def omgcof : Double ; def sinmao : Double ;  
//def t : Double ; def t2cof  : Double ;  
//def t3cof : Double ; def t4cof  : Double ;  
//def t5cof : Double ; def x1mth2 : Double ; 
//def x7thm1 : Double ; def inclo  : Double ;  
//def mo : Double ; def mdot  : Double ;   
def xno : Double ;
//def no : Double ; def nodeo  : Double ; 
//def nodedot : Double ; def xlcof  : Double ; 
//def xmcof : Double ;
//def nodecf : Double ;
def error: Int; def x : Double; def y : Double; def z: Double; def xdot: Double ; def ydot : Double; def zdot: Double;
//def irez; def d2201 ; 
//def d2211; def d3210 ; 
//def d3222;
//def d4410; def d4422 ; 
//def d5220; def d5232 ; 
//def d5421; def d5433 ; 
//def dedt; def del1 ; 
//def del2; def del3 ; 
//def didt; def dmdt ; 
//def dnodt; def domdt ; 
//def e3; def ee2 ;   
//def peo; def pgho ;  
//def pho; def pinco ; 
//def plo; def se2 ;   
//def se3; def sgh2 ;  
//def sgh3; def sgh4 ; 
//def sh2; def sh3 ;  
//def si2; def si3 ;  
//def sl2; def sl3 ;  
//def sl4; def gsto ; 
//def xfact; def xgh2 ; 
//def xgh3; def xgh4 ; 
//def xh2; def xh3 ;  
//def xi2; def xi3 ;   
//def xl2; def xl3 ;   
//def xl4; def xlamo ; 
//def zmol; def zmos ;  
def atime: Double; def xli : Double; 
def xni: Double;
}
  
object Sgp4Result {
  
  
  implicit val wgs = WGS72Constants.tleDoubleConstants

  def apply(tle: TLE)(tt: Double) : Sgp4Result = {
    
    val sgp4    = SGP4Brouwer[Double](tle)
    val statett = sgp4.propagate(tt)
    
    val r = statett.posVel.pos
    val v = statett.posVel.vel
    
    import statett.tif._
    import wgs._
    import i0f._
    import e0f._
    import bmmf._
    import sf._
    import coeff._
    import ilf._
    import ocf._
  
    new Sgp4Result {
      def satn = tle.satelliteNumber
      def ecco = e0
      def epoch : Double = statett.tif.ini.epoch 
      def inclo : Double = i0
    
      //def    no  : Double  = n0 // or n0k
   //   outputs : 
      def method  : Char  = if (isDeepSpace) 'd' else 'n'
    
      def  ainv  : Double    = 1 / a0 
      def    ao  : Double    = a0 
      def con41  : Double    = x3thm1   // FIXME for d
      def con42  : Double    = statett.tif.i0f.con42
      def cosio  : Double    = θ
      def cosio2 : Double    = θsq
      def eccsq  : Double    = e0sq 
      def omeosq : Double    = β0sq
      def  posq  : Double    = statett.tif.sf.posq
      def    rp  : Double    = statett.tif.bmmf.rp
      def rteosq : Double    = β0
      def sinio  : Double    = statett.tif.i0f.sinio
      def  gsto  : Double    = statett.tif.ocf.gsto    
      
      // ---
      def      yr  : Int    = tle.epochyear
      def   bstar  : Double = statett.tif.ini.bStar
      def   argpo  : Double = statett.tif.ini.ω0
      def      mo  : Double = statett.tif.ini.M0
      def   isimp  : Int    = if ((omeosq >= 0.0) || (no >= 0.0))
                                    if (isImpacting || isDeepSpace) 1 else 0
                              else 0
      def   aycof  : Double = statett.tif.ocf.aycof // FIXME for d
      def    cc1   : Double =  C1   
      def     cc4  : Double =  C4 
      def     cc5  : Double =  C5   
      def      d2  : Double =  D2   
      def      d3  : Double =  D3     
      def      d4  : Double =  D4 
      def   delmo  : Double =  delM0
      def     eta  : Double =  η
      def  argpdot : Double =  ωdot 
      def   omgcof : Double =  ωcof 
      def   sinmao : Double =  sinM0.toDouble
      def   t2cof  : Double = statett.tif.ilf.t2cof
      def   t3cof  : Double = statett.tif.ilf.t3cof
      def   t4cof  : Double = statett.tif.ilf.t4cof
      def   t5cof  : Double = statett.tif.ilf.t5cof
      def  x1mth2  : Double = statett.tif.i0f.x1mth2 // FIXME for d
      def  x7thm1  : Double = statett.tif.ocf.x7thm1 // FIXME for d
      def   xlcof  : Double = statett.tif.ocf.xlcof // FIXME for d
      def   xmcof  : Double = Mcof
//      def temp1 : Double = 3 * J2 / posq * no / 2  
//      def temp2 : Double = J2/ posq *temp1 / 2
      //def    mdot  : Double = no + 0.5 * temp1 * rteosq * con41 + 0.0625 * temp2 * rteosq * (13.0 - 78.0 * cosio2 + 137.0 * cosio2*cosio2) // tif.ocf.mdot
      // def    mdot  : Double = no + temp1 * rteosq * con41 / 2 + temp2 * rteosq * (13 - 78 * cosio2 + 137 * cosio2*cosio2) / 16 // 
      def    mdot  : Double = statett.tif.ocf.mdot
      def   nodecf : Double = Ωcof
      def   nodedt : Double = omegadot     
      def        t          = tt
      def   nodeo  : Double = statett.tif.ini.Ω0
      def      no  : Double = statett.tif.bmmf.n0
      def xno = no
      val error = 0
      val x = r(0); val y = r(1) ; val z = r(2); val xdot = v(0) ; val ydot = v(1) ; val zdot = v(2) ;
      // fixme
      def atime = tt
      def xli = 0.0
      def xni = 0.0   
    }
  }
}