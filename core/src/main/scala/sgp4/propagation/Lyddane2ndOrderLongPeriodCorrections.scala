package sgp4.propagation

import spire.algebra.*
import spire.implicits.*
import sgp4.coord.*
import sgp4.coord.LyddaneElems

trait Lyddane2ndOrderLongPeriodCorrections[@sp(Double) F] extends LyddaneLongPeriodCorrections[F]:

  override def lylppCorrectionsCtx(
      secularCtx: SGPSecularCtx[F]
  )(using ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F], fd: FromDouble[F]): LyddaneResult[F] =

    import secularCtx.iCtx.{c, s}

    // apply the first order long-period corrections to compute the "prime" variables
    val lyddaneResult = super.lylppCorrectionsCtx(secularCtx)
    val ly1 = lyddaneResult.elems
    import lyddaneResult.ctx._
    val `η²` = η * η
    val ecosω = ly1.C
    val `ecosω²` = ecosω * ecosω

    // 2nd order corrections
    // note the singularity for (sinI) s = 0
    // use s_ only in terms that would "explode"
    val s_ = if s < asF[F](1.5e-12) then asF[F](1.5e-12) else s

    val δI = ϵ3 * esinω * c
    val δh = -ϵ3 * ecosω * c / s_
    val δC = -ϵ3 * ecosω * esinω * (1 / s_ - 2 * s_)
    val δS = ϵ3 * s_ - ϵ3 * `η²` * s_ - 2 * ϵ3 * `ecosω²` * s_ + ϵ3 * `ecosω²` / s_
    val δF = ϵ3 * s * ecosω * (1 - 2 * `η²` / (1 + η)) / 2 // Lyddane F' = (M´´ + g´´ + h´´) +  ...

    // add 2nd order corrections to the first order corrections
    LyddaneResult(LyddaneElems(ly1.I + δI, ly1.a, ly1.h + δh, ly1.C + δC, ly1.S + δS, ly1.F + δF), lyddaneResult.ctx)
