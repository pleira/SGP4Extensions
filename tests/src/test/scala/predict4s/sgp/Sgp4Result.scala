package predict4s.sgp

trait Sgp4Result[F] extends Sgp4Near[F] {

  def satn : Int
  def ecco  : F 
  def epoch : F 
  def inclo : F 
  
 //   outputs : 
  def method  : Char  = 'n' // FIXME if (isDeepSpace) 'd' else 'n'
  
  def  ainv  : F    
  def    ao  : F    
  def con41  : F    
  def cosio  : F    
  def cosio2 : F    
  def eccsq  : F     
  def omeosq : F    
  def  posq  : F    
  def    rp  : F    
  def rteosq : F    
  def sinio  : F    
  def  gsto  : F        
  
  // ---
  def      yr  : Int    
  def   bstar  : F 
  def   argpo  : F 
  def      mo  : F 
  def   isimp  : Int    
  def    cc1   : F    
  def     cc4  : F  
  def     cc5  : F    
  def      d2  : F 
  def      d3  : F      
  def      d4  : F  
  def     eta  : F 
  def  argpdot : F  
  def   omgcof : F  
  def   sinmao : F 
  def   t2cof  : F 
  def   t3cof  : F 
  def   t4cof  : F 
  def   t5cof  : F 
  def  x1mth2  : F 
  def  x7thm1  : F 
  def   xmcof  : F 
//      def temp1 : F = 3 * J2 / posq * no / 2  
//      def temp2 : F = J2/ posq *temp1 / 2
  //def    mdot  : F = no + 0.5 * temp1 * rteosq * con41 + 0.0625 * temp2 * rteosq * (13.0 - 78.0 * cosio2 + 137.0 * cosio2*cosio2) // tif.ocf.mdot
  // def    mdot  : F = no + temp1 * rteosq * con41 / 2 + temp2 * rteosq * (13 - 78 * cosio2 + 137 * cosio2*cosio2) / 16 // 
  def    mdot  : F 
  def   nodecf : F 
  def   nodedt : F      
  def   nodeo  : F 
  def      no  : F 
  def xno : F  = no
  // FIXME
  def atime : F 
//  def xli : F  = 0.0.as[F]
//  def xni : F  = 0.0.as[F]   
}
