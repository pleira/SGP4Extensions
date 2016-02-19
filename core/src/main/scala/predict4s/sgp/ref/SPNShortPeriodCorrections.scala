package predict4s
package sgp.ref

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.coord._

trait ShortPeriodPolarNodalCorrections[F] {
  
  def sppCorrections(@sp(Double) ctx: LPPSPNCtx[F])(implicit ev: Field[F], trig: Trig[F], ro: NRoot[F] )
      : SpecialPolarNodal[F] = {
    import ctx.{_1 => lppPN,_2 => lppc,_3 => secular}
    import lppPN.r,lppc.{pl,βl,`√pl`,cos2θ,sin2θ}
    import secular.{_2=>ictx,_3 => wgs}
    import wgs.J2, ictx.{c,s,`c²`,`s²`}

    val `3c²-1` = 3*`c²`-1
    val `βl³/√pl` = βl*βl*βl/`√pl`
    val ϵ2 = - J2 / pl / pl / 4

    val δI = - 3*ϵ2*c*s*cos2θ
    val δθ =     ϵ2*(7*`c²`-1)*sin2θ / 2
    val δΩ = - 3*ϵ2*c*sin2θ
    val δr =     ϵ2* (3*r*βl*`3c²-1` - pl*`s²`*cos2θ)      
    val δR =   2*ϵ2*`s²`*sin2θ*`βl³/√pl` // rdot, angular velocity
    val δrvdot= -ϵ2 *(2*`s²`*cos2θ + 3*`3c²-1`) * `βl³/√pl` 
//    val δR = - `J2/p/2` * `s²` * sin2θ * n / KE  // rdot, angular velocity
//    val δrvdot =  `J2/p/2` * (`s²` * cos2θ + 1.5 * `3c²-1`) * n / KE

    val δspp = SpecialPolarNodal(δI,δθ,δΩ,δr,δR,δrvdot)
    lppPN + δspp
  }
 
}