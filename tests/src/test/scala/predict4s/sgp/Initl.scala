package predict4s.sgp


/*
 * The traits/classes here are used to compare our results with those from Vallado's software, using the same variable names as in Vallado.
 */
trait Initl[F] {
  def satn: Int
  def ecco : F
  def epoch : F
  def inclo : F
  def    no  : F   
 //   outputs : 
  def  ainv  : F     ; def    ao  : F  ; def cosio  : F  ; def cosio2  : F    
  def eccsq  : F  ; def omeosq : F     ; def    rp  : F  ; def rteosq : F  ; 
  def sinio  : F  ; def  gsto  : F     ; 
}


trait  Sgp4Init[F] {
  def satn: Int; def      yr  : Int      ; def   bstar  : F; def    ecco  : F; def   epoch  : F; def   argpo  : F; 
  def inclo  : F     ; def      mo  : F   ; 
  // in and out variables 
  def    no  : F     ; 
  //    outputs  :
  def   isimp  : Int      ; def  method  : Char       ; def   aycof  : F    ; 
  def con41  : F     ; def    cc1   : F     ; def     cc4  : F    ; 
  def   cc5  : F     ; def      d2  : F     ; def      d3  : F    ; def      d4  : F     ; def   delmo  : F; 
  def   eta  : F     ; def  argpdot : F     ; def   omgcof : F; 
  def sinmao : F     ; def   t2cof  : F     ; def   t3cof  : F    ; 
  def t4cof  : F     ; def   t5cof  : F     ; def    gsto  : F    ; def  x1mth2  : F ; def  x7thm1  : F ; def   xlcof  : F ; 
  def xmcof  : F     ; def    mdot  : F     ; def   nodecf : F    ; def   nodedt : F  ; 
  //   in and outputs from deep space satellites :
  def     t  : F     ; def   nodeo  : F     ; 
}

//trait Sgp4Vars[F] extends Initl[F]  with Sgp4Init[F]
trait PosVel[F] {
  def error: Int; def x : F; def y : F; def z: F; def xdot: F ; def ydot : F; def zdot: F;
}

trait Sgp4Near[F] extends Initl[F] with PosVel[F] {
  def xno : F ;
  def atime: F; 
  def satn: Int; def      yr  : Int      ; def   bstar  : F; def    ecco  : F; def   epoch  : F; def   argpo  : F; 
  def inclo  : F     ; def      mo  : F   ; 
  // in and out variables 
  def    no  : F     ; 
  //    outputs  :
  def   isimp  : Int      ; def  method  : Char       ; 
  def    cc1   : F     ; def     cc4  : F    ; 
  def   cc5  : F     ; def      d2  : F     ; def      d3  : F    ; def      d4  : F     ;
  def   eta  : F     ; def  argpdot : F     ; def   omgcof : F; 
  def sinmao : F     ; def   t2cof  : F     ; def   t3cof  : F    ; 
  def t4cof  : F     ; def   t5cof  : F     ; def    gsto  : F    ; def  x1mth2  : F ; def  x7thm1  : F ; 
  def xmcof  : F     ; def    mdot  : F     ; def   nodecf : F    ; def   nodedt : F  ; 
  //   in and outputs from deep space satellites :
  def     t  : F     ; def   nodeo  : F     ; 

  //  def xli : F; 
//  def xni: F;
}
