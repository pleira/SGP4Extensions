package predict4s.tle

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

trait Sgp4Result extends Sgp4Vars {
  def xno : Double ;
  def error: Int; def x : Double; def y : Double; def z: Double; def xdot: Double ; def ydot : Double; def zdot: Double;
  def atime: Double; def xli : Double; 
  def xni: Double;
}
