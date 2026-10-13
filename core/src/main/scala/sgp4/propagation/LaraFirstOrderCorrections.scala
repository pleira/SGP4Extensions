package sgp4.propagation

import spire.algebra.*
import spire.implicits.*
import sgp4.coord.*

trait LaraFirstOrderCorrections[@sp(Double) F]:

  def lppCorrections(lns: LaraNonSingular[F], secularElemt: SGPSecularCtx[F])(implicit
      ev: Field[F]
  ): (LaraNonSingular[F], SGPSecularCtx[F]) =
    val elem = secularElemt.elem
    val ictx = secularElemt.iCtx
    val wgs = secularElemt.wgs
    import lns._
    import elem.{a, e}, wgs.`J3/J2`, ictx.{c, `c²`}

    val `e²` = e * e
    val p = a * (1 - `e²`) // semilatus rectum , as MU=1
    val `p/r` = p / r
    val ϵ3 = `J3/J2` / p / 2
    val κ = `p/r` - 1
    val σ = p * R / Θ

    val `χ²` = χ * χ
    val `ξ²` = ξ * ξ

    val δψ = ϵ3 * (2 * χ + (κ * χ - c * σ * ξ) / (1 + c))
    val δξ = ϵ3 * (2 * `χ²` + κ * (1 - `ξ²`))
    val δχ = -ϵ3 * (`c²` * σ + (2 + κ) * ξ * χ)
    val δr = ϵ3 * p * ξ
    val δR = ϵ3 * (Θ / r) * (1 + κ) * χ
    val δΘ = ϵ3 * Θ * (κ * ξ - σ * χ)

    val rl = r + δr
    val Rl = R + δR
    val Θl = δΘ + Θ
    (LaraNonSingular(ψ + δψ, ξ + δξ, χ + δχ, rl, Rl, Θl), secularElemt)

  def sppCorrections(lppState: (LaraNonSingular[F], SGPSecularCtx[F]))(using ev: Field[F]): LaraNonSingular[F] =
    import lppState.{_1 => lns, _2 => secularElemt}
    val ictx = secularElemt.iCtx
    val wgs = secularElemt.wgs
    import wgs.J2, ictx.{`c²`, `(1+7c)/(1+c)`}
    import lns._

    val p = Θ * Θ
    val ϵ2 = -J2 / p / p / 4

    val `χ²` = χ * χ
    val `ξ²` = ξ * ξ
    val δψ = -ϵ2 * `(1+7c)/(1+c)` * ξ * χ
    val δξ = -ϵ2 * (`χ²` - 3 * `c²`) * ξ
    val δχ = ϵ2 * (`ξ²` - 3 * `c²`) * χ
    val δr = ϵ2 * 2 * r * (`ξ²` - 2 + 5 * `c²`)
    val δR = ϵ2 * 4 * (Θ / r) * ξ * χ
    val δΘ = ϵ2 * 3 * Θ * (`ξ²` - `χ²`)
    LaraNonSingular(ψ + δψ, ξ + δξ, χ + δχ, r + δr, R + δR, Θ + δΘ)

  def allCorrections(lns: LaraNonSingular[F], secularElemt: SGPSecularCtx[F])(implicit
      ev: Field[F]
  ): LaraNonSingular[F] =
    val elem = secularElemt.elem
    val ictx = secularElemt.iCtx
    val wgs = secularElemt.wgs
    import lns._
    import elem.{a, e}, wgs.{`J3/J2`, J2}, ictx.{c, `c²`, `(1+7c)/(1+c)`}

    val `e²` = e * e
    val p = a * (1 - `e²`) // semilatus rectum , as MU=1
    val `p/r` = p / r
    val ϵ3 = `J3/J2` / p / 2
    val κ = `p/r` - 1
    val σ = p * R / Θ
    val `χ²` = χ * χ
    val `ξ²` = ξ * ξ

    // long period corrections
    val δψl = ϵ3 * (2 * χ + (κ * χ - c * ξ * σ) / (1 + c))
    val δξl = ϵ3 * (2 * `χ²` + κ * (1 - `ξ²`))
    val δχl = -ϵ3 * (`c²` * σ + (2 + κ) * χ * ξ)
    val δrl = ϵ3 * p * ξ
    val δRl = ϵ3 * (Θ / r) * (1 + κ) * χ
    val δΘl = ϵ3 * Θ * (κ * ξ - σ * χ)

    // LPP corrections in Lara Non-Sigular variables
    val ψl = ψ + δψl
    val ξl = ξ + δξl
    val χl = χ + δχl
    val rl = r + δrl
    val Rl = R + δRl
    val Θl = Θ + δΘl

    // now, use the LPP corrections variables to calculate SPP corrections
    val pl = Θl * Θl
    val ϵ2 = -J2 / pl / pl / 4
    val `χl²` = χl * χl
    val `ξl²` = ξl * ξl
    val δψs = -ϵ2 * `(1+7c)/(1+c)` * ξl * χl
    val δξs = -ϵ2 * (`χl²` - 3 * `c²`) * ξl
    val δχs = ϵ2 * (`ξl²` - 3 * `c²`) * χl
    val δrs = ϵ2 * 2 * r * (`ξ²` - 2 + 5 * `c²`)
    val δRs = ϵ2 * 4 * (Θl / rl) * ξl * χl
    val δΘs = ϵ2 * 3 * Θl * (`ξl²` - `χl²`)
    LaraNonSingular(ψl + δψs, ξl + δξs, χl + δχs, rl + δrs, Rl + δRs, Θl + δΘs)
