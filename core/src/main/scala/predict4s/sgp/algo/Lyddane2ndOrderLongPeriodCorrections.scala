package predict4s.sgp.algo

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.coord._
import predict4s.coord.LyddaneElems

trait Lyddane2ndOrderLongPeriodCorrections[F] extends LyddaneLongPeriodCorrections[F] {
  
  override def lylppCorrections(secularElemt : SGPElems[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F]) : LyddaneElems[F] = {
    import secularElemt._,wgs.`J3/J2`,ictx.{c,s}
    
    // apply the first order long-period corrections to compute the "prime" variables
    val ly = super.lylppCorrectionsCtx(secularElemt)
    val ly1 = ly._1
    val `η²`  = ly._2._1 
    val ϵ3 =  ly._2._2
    val esinω = ly._2._3
    val η  = ly._2._4 
    val ecosω = ly1.C
    val `ecosω²` = ecosω*ecosω
    
    // 2nd order corrections    // FIXME: note the singularity for s = 0 
    val δI =   ϵ3 * esinω * c
    val δa =   0
    val δh = - ϵ3 * ecosω * c/s
    val δC = - ϵ3 * ecosω * esinω * (1/s - 2*s)
    val δS =   ϵ3 * s - ϵ3*`η²`*s - 2*ϵ3*`ecosω²`*s + ϵ3*`ecosω²`/s
    val δF =   ϵ3 * s * ecosω * (1 - 2*`η²`/(1+η)) / 2    // Lyddane F' = (M´´ + g´´ + h´´) +  ...
    
    // add 2nd order corrections to the first order corrections
    LyddaneElems(ly1.I + δI, ly1.a, ly1.h+δh, ly1.C+δC, ly1.S+δS, ly1.F+δF)
  }
  
}