package sgp4
package propagation

import spire.algebra._
import spire.implicits._
import sgp4.coord._

trait SPNShortPeriodCorrections[F] {

  def sppCorrections(@sp(Double) ctx: LPPSPNCtx[F])(implicit ev: Field[F], trig: Trig[F]): SpecialPolarNodal[F] = {
    import ctx.{spn => lppPN, lppCtx => lppc, secularCtx => secular}
    import lppPN.r, lppc.{pl, βl, `√pl`, cos2θ, sin2θ}
    import secular.{iCtx => ictx, wgs}
    import wgs.J2, ictx.{c, s, `3c²-1`, `7c²-1`, `s²`}
    val `βl³/√pl` = βl * βl * βl / `√pl`
    val ϵ2 = -J2 / pl / pl / 4

    val δI = -3 * ϵ2 * c * s * cos2θ
    val δθ = ϵ2 * `7c²-1` * sin2θ / 2
    val δΩ = -3 * ϵ2 * c * sin2θ
    val δr = ϵ2 * (3 * r * βl * `3c²-1` - pl * `s²` * cos2θ)
    val δR = 2 * ϵ2 * `s²` * sin2θ * `βl³/√pl` // rdot, angular velocity
    val δrvdot = -ϵ2 * (2 * `s²` * cos2θ + 3 * `3c²-1`) * `βl³/√pl`
//    val δR = - `J2/p/2` * `s²` * sin2θ * n / KE  // rdot, angular velocity
//    val δrvdot =  `J2/p/2` * (`s²` * cos2θ + 1.5 * `3c²-1`) * n / KE

    // the short period corrections are directly added to the long period result,
    // avoiding an intermediate SpecialPolarNodal
    SpecialPolarNodal(
      lppPN.I + δI,
      lppPN.θ + δθ,
      lppPN.Ω + δΩ,
      lppPN.r + δr,
      lppPN.R + δR,
      lppPN.`Θ/r` + δrvdot
    )
  }

}
