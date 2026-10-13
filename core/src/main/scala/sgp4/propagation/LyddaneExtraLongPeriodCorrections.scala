package sgp4.propagation

import spire.algebra.*
import spire.math.{abs, sqrt, sin, cos}
import spire.implicits.*
import sgp4.coord.*

trait LyddaneExtraLongPeriodCorrections[@sp(Double) F]:

  def lppLydCorrections(
      secularElemt: SGPSecularCtx[F]
  )(using ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F], fd: FromDouble[F]): LyddaneElems[F] =

    import secularElemt.{elem, wgs, iCtx => ictx}
    import ictx.{c, s}, wgs.`J3/J2`, elem.{a, e, ω, M, I, Ω}

    // Brouwer long-period gravitational corrections are reformulated in Lyddane’s (F,S,C,a,h,I).
    val `η²` = (1 - e * e)
    val η = sqrt(`η²`)
    val p = a * `η²` // p´´ = a´´*(1 − e´´²)
    val ϵ3 = `J3/J2` / p / 2

    val esinω = e * sin(ω) // S´´ = e´´*sin(g´´) , g´´ = ω
    val ecosω = e * cos(ω) // C´´ = e´´ * cos(g´´), here, we have corrections for C
    val `ecosω²` = ecosω ** 2

    // Then, apply the long-period corrections to compute the "prime" variables

    // a similar fix as Vallado's fix for divide by zero with I = 180 deg
    // sgp4fix for divide by zero with I = 180 deg
    val `1+c`: F =
      if abs(1 + c) > asF[F](1.5e-12) then (1 + c) else asF[F](1.5e-12)

    // Lyddane elements including 2nd order corrections // FIXME: note the singularity for (sinI) s = 0
    val s_ =
      if s < asF[F](1.5e-12) then asF[F](1.5e-12) else s

    val Il = I + ϵ3 * esinω * c
    val h = Ω - ϵ3 * ecosω * c / s_
    val C = ecosω * (1 - ϵ3 * esinω * (1 / s_ - 2 * s_))
    val S = esinω - ϵ3 * ((`η²` + 2 * `ecosω²`) * s_ - `ecosω²` / s_)
    val F =
      M + ω + Ω - ϵ3 * s_ * ecosω * ((1 + 2 * c) / `1+c` + `η²` / (1 + η)) // Lyddane F' = (M´´ + g´´ + h´´) +  ...

    LyddaneElems(Il, a, h, C, S, F)
