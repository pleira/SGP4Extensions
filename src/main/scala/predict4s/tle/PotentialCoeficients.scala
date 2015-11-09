package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
  
// TODO: try to express this operation as being part of an AST with a single Context as parameter, 
// returning a description, that is the GeoPotentialCoefs and an updated Context

trait PotentialCoeficients {  
  
  def getCoefs[F: Field : NRoot : Order : Trig](elemsdp : TEME.SGPElems[F], ctx: Context0[F], a0: F)(implicit wgs: SGPConstants[F])  : (GeoPotentialCoefs[F], Context1[F]) = {
  
    // use now double prime variables
    import elemsdp._, wgs.aE
    val e0 = elemsdp.e
    
    // radius of perigee (as a0 (dp) is present, there is a aE term difference with Vallado's)
    val rp    = a0*(1-e0)
    assert (rp > aE)
    
    // perigee height, altitude relative to the earth's surface, so perige instead of perigee 
    val perige =  rp - aE  
    
   
    def S_above156       =  (1 + 78/aE)
    def hs               =  perige - 78   // interpolation, being a number bigger than 20, and smaller that 78
    def S_between_98_156 =  (1 + hs/aE)
    def S_below98        =  (1 + 20/aE)
  
    /* the parameter s is a fitting parameter in density representation.
     * It is determined based of epoch perigee
     * height above a spherical Earth. If perigee height is greater than or equal 156 km, the value of s is
     * fixed to be 78 km plus one Earth radius. For altitudes greater than or equal to 98 km but less
     * than 156 km, s is defined to be perigee height minus 78 km plus one Earth radius. 
     * For altitudes below 98 km, s is 20 km plus one Earth radius.
     */
    def fittingAtmosphericParameter : F = 
       if (perige >= 156)       S_above156
       else if (perige >= 98)   S_between_98_156
       else                     S_below98  
    
    // The parameter q0 is the geocentric reference altitude, 
    // a constant equal to 120 km plus one Earth radius 
    val s    = fittingAtmosphericParameter

    val ctx1 = Context1(elemsdp, a0, s, rp, aE)
    import ctx1.{a0 =>_,_}
    import ctx.k2,ctx.Ke,ctx.A30,ctx.sini0,ctx.`θ²`,ctx.`β0²`

    val coef1 = q0ms_ξ__to4 / (psisq** 3.5)
    val C2 : F = coef1 * n0 *(a0 * (1 + 1.5*`η²` + e0η*(4 + `η²`)) + 3*k2*ξ / 2 / psisq * (3*`θ²` - 1) / 2 * (8 + 3*`η²`*(8 + `η²`)))
    
    val C1 : F = bStar * C2
    val `C1²`  = C1*C1
    def C1sq   = `C1²`
  
    val C3 =  if (e0 > 0.0001.as[F]) q0ms_ξ__to4 * ξ * A30 * n0 * aE * sini0 / e0 / k2  else 0.as[F]
    
    val aterm = 3*(1-3*`θ²`)*(1 + 3*`η²`/2 - 2*e0η - e0η*`η²`/2) + 3*(1-`θ²`)*(2*`η²` - e0η - e0η*`η²`)*cos(2*ω0)/4
    val C4 = 2*a0*`β0²`*coef1*n0*((2*η*(1+e0η) + (e0 + ηto3)/2) - 2*k2*ξ*aterm/(a0*`psi²`))
    val C5 = 2*a0*`β0²`*coef1*(1 + 11*(`η²`+e0η)/4 + e0η*`η²`)
     
    val D2 = 4*a0*C1*C1*ξ
    val D3 = D2*(17*a0+s)*C1*ξ/3
    val D4 = D2*D2*ξ*(221*a0+31*s)/24
    (GeoPotentialCoefs(C1,C2,C3,C4,C5,D2,D3,D4), ctx1)
  }

}

object PotentialCoeficients extends PotentialCoeficients
