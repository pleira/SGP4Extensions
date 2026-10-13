package sgp4.coord
import sgp4.io.TLE
import sgp4.earth_gravity.*

import spire.algebra.*
import spire.math.{sqrt, atan2}
import spire.implicits.*

case class AnomalyState[@sp(Double) F](E: F, cosE: F, sinE: F, ecosE: F, esinE: F):
  // define alternative methods to allow expressing a different Anomaly U (not the Eccentric Anomaly)
  def U: F = E; def cosU: F = cosE; def sinU: F = sinE; def ecosU: F = ecosE; def esinU: F = esinE;

case class SPNAuxVariables[@sp(Double) F](p: F, κ: F, σ: F, n: F, β: F)

object SGPElemsConversions:

  /** Builds SGPElems with original mean motion (n0'', n0dp) and semimajor axis (a0'' , a0dp).
    */
  def sgpElemsAndContext[@sp(Double) F: Field: Trig: NRoot: Order: TruncatedDivision: FromDouble](
      tle: TLE,
      wgs: SGPConstants[F]
  ): SGPElemsResult[F] =
    val e0 = asF[F](tle.eccentricity.toDouble)
    val i0 = asF[F](tle.inclination.toDouble.toRadians)
    val pa = asF[F](tle.argumentOfPeriapsis.toDouble.toRadians)
    val raan = asF[F](tle.rightAscension.toDouble.toRadians)
    val meanAnomaly = asF[F](tle.meanAnomaly.toDouble.toRadians)
    val meanMotionPerDay = asF[F](tle.meanMotion.toDouble)
    val bStar = asF[F](tle.atmosphericDragCoefficient.toDouble)
    // in julian days
    val epoch: F =
      val mdhms = TimeUtils.days2mdhms(tle.year, tle.epoch.toDouble)
      asF[F]((TimeUtils.jday(tle.year, mdhms._1, mdhms._2, mdhms._3, mdhms._4, mdhms._5) - 2433281.5))
    val ω0 = pa
    val Ω0 = raan
    val M0 = meanAnomaly
    val radPerMin0 = TimeUtils.revPerDay2RadPerMin(meanMotionPerDay)
    calcContextAndOriginalMotionAndSemimajorAxis(e0, i0, ω0, Ω0, M0, bStar, epoch, radPerMin0, wgs)

  private def calcContextAndOriginalMotionAndSemimajorAxis[F: Field: NRoot: Order: Trig: TruncatedDivision: FromDouble](
      e: F,
      I: F,
      ω: F,
      Ω: F,
      M: F,
      bStar: F,
      epoch: F,
      radPerMin: F,
      wgs: SGPConstants[F]
  ): SGPElemsResult[F] =
    for
      eCtx <- EccentricityCtx.elliptical(e)
      iCtx = InclinationCtx(I)
      n_and_a = calcOriginalMotionAndSemimajorAxis(radPerMin, iCtx, eCtx, wgs)
      (n, a) = (n_and_a._1, n_and_a._2)
    yield SGPElemsCtx(SGPElems(n, e, I, ω, Ω, M, a, bStar, epoch), iCtx, eCtx, wgs)

  private def calcOriginalMotionAndSemimajorAxis[
      @sp(Double) F: Field: NRoot: Order: Trig: TruncatedDivision: FromDouble
  ](
      n: F,
      iCtx: InclinationCtx[F],
      eCtx: EccentricityCtx[F],
      wgs: SGPConstants[F]
  ): (F, F) =
    import wgs.{KE, J2, `2/3`, `1/3`}
    import iCtx.`3c²-1`, eCtx.`β0³`

    val a1: F = (KE / n) fpow `2/3` // (Ke / n0) pow 1.5
    val tval = 3 * J2 * `3c²-1` / `β0³` / 4 // 3 * k2 * (3*`cos²I0` - 1) / ((1-`e0²`) pow 1.5) / 4
    val δ1 = tval / (a1 * a1)
    val a0 = a1 * (1 - δ1 * (`1/3` + δ1 * (1 + 134 * δ1 / 81)))
    val δ0 = tval / (a0 * a0)
    val n0dp = n / (1 + δ0)
    val a0dp = (KE / n0dp) fpow `2/3` // a0   / (1 - δ0)
    (n0dp, a0dp)

  def sgpelems2spn[@sp(Double) F: Field: Trig: NRoot: Order: TruncatedDivision: FromDouble](
      eaState: AnomalyState[F],
      secularCtx: SGPSecularCtx[F]
  ): SpecialPolarNodal[F] =
    import eaState._
    import secularCtx.elem
    import elem.{a, I, e, ω, Ω}
    import secularCtx.wgs
    import wgs.`2pi`

    val `e²` = e * e
    val β = sqrt(1 - `e²`)
    val `√a` = sqrt(a)

    val r = a * (1 - ecosE) // r´
    val R = `√a` / r * esinE // R´ = L/r *esinE, L=sqrt(nu*a), MU=1 in SGP4 variables, later applied
    val Θ = `√a` * β // MU=1
    val numer = β * sinE
    val denom = (cosE - e)
    val sinf = a / r * numer
    val cosf = a / r * denom
    // comes from -pi,pi
    val f = atan2(sinf, cosf)
    val θ = (f + ω) tmod asF[F](`2pi`) // if (θ0to2pi > asF[F](pi)) θ0to2pi - `2pi` else θ0to2pi
    SpecialPolarNodal(I, θ, Ω, r, R, Θ / r)
