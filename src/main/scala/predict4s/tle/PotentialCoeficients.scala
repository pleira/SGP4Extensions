package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.tle.DpTransform._


/**
 * The initialization process provides a series of coefficients needed
 * to apply drag secular corrections as computed from Lane’s theory.
 * 
 * This class appears in the time expansion, where tX are powers of the time.
 */
case class LaneCoefs[F : Field](gcof : GeoPotentialCoefs[F]) {
   import gcof._
   val `C1²` = C1*C1
   val t2cof = 3*C1/2
   val t3cof = D2 + 2*`C1²`
   val t4cof = (3*D3 + C1*(12*D2 + 10 * `C1²`))/4
   val t5cof = (3*D4 + 12*C1*D3 + 6*D2*D2 + 15*`C1²`*(2*D2+`C1²`))/5
}

case class GeoPotentialContext[F: Field: NRoot : Order: Trig](elemsdp : TEME.SGPElems[F], s: F, rp: F, aE: F) {
    import elemsdp.{n => n0,e => e0, a => a0}
  
    val ξ    = 1 / (a0 - s)  // tsi
    val `ξ²` = ξ*ξ
    val `ξ³` = ξ**3
    val `ξ⁴` = ξ**4
    
    def ξsq  = ξ*ξ
    def ξto3 = ξsq*ξ
    def ξto4 = ξsq*ξsq
    def ξto5 = ξto4*ξ
  
    val η    = a0*e0*ξ   // eta
    val `η²` = η*η
    val `η³` = η**3
    val `η⁴` = η**4
    def ηsq  = η*η       // etasq
    def ηto3 = ηsq*η
    def ηto4 = ηsq*ηsq
    
    val e0η   = e0*η      // eeta 
    def psisq = abs[F](1-ηsq)  // Vallado uses fabs
    val `psi²`= psisq
    
    // The parameter q0 is the geocentric reference altitude, 
    // a constant equal to 120 km plus one Earth radius 
    val q0   = 1 + 120/aE 
    val q0ms_to4 = (q0 - s)**4 
    
    // q0 minus s ξ  all to 4 
    val q0ms_ξ__to4 = q0ms_to4*(ξ**4)  

  }

trait FittingAtmosphericParameter {
  // AtmosphericCoef
    // import ctx.wgs.aE, dp.{perige => perigeeHeight}
  
    def S_above156[F: Field](aE: F) : F      =  (1 + 78/aE)
    def hs[F: Field](perigeeHeight: F)  : F       =  perigeeHeight - 78   // interpolation, being a number bigger than 20, and smaller that 78
    def S_between_98_156[F: Field](perigeeHeight: F, aE: F) : F =  (1 + hs(perigeeHeight)/aE)
    def S_below98[F: Field](aE: F) : F =  (1 + 20/aE)
  
    def fittingAtmosphericParameter[F: Field : Order](perigeeHeight: F, aE: F) : F = 
       if (perigeeHeight >= 156)       S_above156(aE)
       else if (perigeeHeight >= 98)   S_between_98_156(perigeeHeight,aE)
       else                            S_below98(aE)  
  
//    def s[F: Field: Order](perigeeHeight: F, aE: F) : F = fittingAtmosphericParameter(perigeeHeight,aE)
  //--------------------------------
}
    
trait GeoPotentialModel extends FittingAtmosphericParameter {

  def geoPotentialCoefs[F: Field : NRoot : Order : Trig](elem: TEME.SGPElems[F], ctx: Context0[F], gctx: GeoPotentialContext[F], aE: F) 
      : GeoPotentialCoefs[F] = {

    import elem.{a => a0,e => e0,n => n0,ω => ω0, bStar}
    import gctx._
    import ctx.wgs.{J2,J3}
    import ctx.sinI0,ctx.`θ²`,ctx.`β0²`,ctx.β0sq,ctx.θsq
  
    val coef1 : F = q0ms_ξ__to4 / (psisq** 3.5)
  
    val C2 : F = coef1 * n0 *(a0 * (1 + 1.5*`η²` + e0η*(4 + `η²`)) + 0.375*J2*ξ / psisq * (3*`θ²` - 1) * (8 + 3*`η²`*(8 + `η²`)))
      // coef1 * n0 *(a0 * (1 + 1.5*`η²` + e0η*(4 + `η²`)) + 3*J2*ξ / 2 / psisq * (3*`θ²` - 1) / 2 * (8 + 3*`η²`*(8 + `η²`)))
    
    val C1 : F = bStar * C2
    val `C1²`  = C1*C1
    def C1sq   = `C1²`
  
    val C3 =  if (e0 > 0.0001.as[F]) -2 * q0ms_ξ__to4 * ξ * (J3/J2) * n0 * sinI0 / e0 else 0.as[F]
    val aterm = 3*(1-3*`θ²`)*(1 + 3*`η²`/2 - 2*e0η - e0η*`η²`/2) + 3*(1-`θ²`)*(2*`η²` - e0η - e0η*`η²`)*cos(2*ω0)/4
    val C4 = 2*a0*`β0²`*coef1*n0*((2*η*(1+e0η) + (e0 + ηto3)/2) - J2*ξ*aterm/(a0*`psi²`))
    val C5 = 2*a0*`β0²`*coef1*(1 + 11*(`η²`+e0η)/4 + e0η*`η²`)
    val D2 = 4*a0*`C1²`*ξ
    val D3 = D2*(17*a0+s)*C1*ξ/3
    val D4 = D2*D2*ξ*(221*a0+31*s)/24
    GeoPotentialCoefs(C1,C2,C3,C4,C5,D2,D3,D4)
  }

}
