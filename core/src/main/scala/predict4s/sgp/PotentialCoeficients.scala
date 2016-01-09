package predict4s.sgp

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.coord.SGPElems
import predict4s.coord.Context0
import predict4s.coord.SGPConstants
import predict4s.coord.Context0

// The notation used in the formulas here correspond to that used in SPACETRACK Report n 3, Hoots.

trait GeoPotentialAndAtmosphere2ndOrderModel {

  def geoPotentialCoefs[F: Field : NRoot : Order : Trig](elem0: SGPElems[F], ctx: Context0[F], gctx: GeoPotentialContext[F], wgs: SGPConstants[F]) 
      : GeoPotentialCoefs[F] = {
    import elem0.{a => a0,e => e0,n => n0,ω => ω0, bStar}
    import gctx._
    import wgs.{J2,J3,`J3/J2`}
    import ctx.sinI0,ctx.`θ²`,ctx.`β0²`
  
    val coef1 : F = `ξ⁴(q0-s)⁴` / (`1-η²`** 3.5)
  
    val C2 : F = coef1 * n0 *(a0 * (1 + 1.5*`η²` + e0η*(4 + `η²`)) + 0.375*J2*ξ / `1-η²` * (3*`θ²` - 1) * (8 + 3*`η²`*(8 + `η²`)))
      // coef1 * n0 *(a0 * (1 + 1.5*`η²` + e0η*(4 + `η²`)) + 3*J2*ξ / 2 / psisq * (3*`θ²` - 1) / 2 * (8 + 3*`η²`*(8 + `η²`)))
    
    val C1 : F = bStar * C2
    val `C1²`  = C1*C1
  
    val C3 =  if (e0 > 0.0001.as[F]) -2 * `ξ⁴(q0-s)⁴` * ξ * `J3/J2` * n0 * sinI0 / e0 else 0.as[F]
    val aterm = 3*(1-3*`θ²`)*(1 + 3*`η²`/2 - 2*e0η - e0η*`η²`/2) + 3*(1-`θ²`)*(2*`η²` - e0η - e0η*`η²`)*cos(2*ω0)/4
    val C4 = 2*a0*`β0²`*coef1*n0*((2*η*(1 + e0η) + (e0 + `η³`)/2) - J2*ξ*aterm/(a0*`1-η²`))
    val C5 = 2*a0*`β0²`*coef1*(1 + 11*(`η²`+e0η)/4 + e0η*`η²`)
    val D2 = 4*a0*`C1²`*ξ
    val D3 = D2*(17*a0+s)*C1*ξ/3
    val D4 = D2*D2*ξ*(221*a0+31*s)/24
    GeoPotentialCoefs(C1,C2,C3,C4,C5,D2,D3,D4)
  }
  
}

trait FittingAtmosphericParameter[F] {
  
  val aE : F
  def S_above156(implicit ev: Field[F]) : F = 1 + 78/aE
  def hs(perigeeHeight: F)(implicit ev: Field[F]) : F =  perigeeHeight - 78   // interpolation, being a number bigger than 20, and smaller that 78
  def S_between_98_156(perigeeHeight: F)(implicit ev: Field[F]) : F =  (1 + hs(perigeeHeight)/aE)
  def S_below98(implicit ev: Field[F]) : F =  (1 + 20/aE)
  
  def fittingAtmosphericParameter(perigeeHeight: F)(implicit ev: Field[F], o: Order[F]) : F =
       if (perigeeHeight >= 156)       S_above156
       else if (perigeeHeight >= 98)   S_between_98_156(perigeeHeight)
       else                            S_below98

}

case class GeoPotentialCoefs[F](C1: F, C2: F, C3: F, C4: F, C5: F, D2: F, D3: F, D4: F)

case class GeoPotentialContext[F: Field: NRoot : Order: Trig](
    elem0 : SGPElems[F],   // original elements with a0 and n0 
    s: F,                  // atmospheric coefficient
    rp: F,                 // perigee radius (km)
    aE: F                  // earths radius  (km)
    ) {
  import elem0.{n => n0,e => e0, a => a0}

  val ξ = 1 / (a0 - s)  // tsi
  val `ξ²` = ξ*ξ
  val `ξ³` = ξ*`ξ²`
  val `ξ⁴` = `ξ²`*`ξ²`

  val η = a0*e0*ξ   // eta
  val `η²` = η*η
  val `η³` = η*`η²`
  val `η⁴` = `η²`*`η²`
  
  val e0η = e0*η      // eeta 
  val `1-η²` = abs[F](1-`η²`) 
  // The parameter q0 is the geocentric reference altitude, 
  // a constant equal to 120 km plus one Earth radius 
  val q0   = 1 + 120/aE 
  val `(q0-s)⁴` = (q0 - s)**4 
  val `ξ⁴(q0-s)⁴` = `(q0-s)⁴` * `ξ⁴`
}
