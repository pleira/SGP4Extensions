package predict4s.sgp.algo

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s.coord._
import predict4s.coord.LyddaneElems

trait Lyddane2ndOrderLongPeriodCorrections[F] extends LyddaneLongPeriodCorrections[F] {
      
  override def lylppCorrectionsCtx(secularCtx : SGPSecularCtx[F])(implicit ev: Field[F], trig: Trig[F], or: Order[F], nr: NRoot[F])
      : LyddaneResult[F]  = {
    
    import secularCtx.{_1 => elem,_3 => wgs,_2 => ictx}
    import ictx.{c,s},wgs.`J3/J2`,elem.{a,e,ω,M,I,Ω}
   
    // apply the first order long-period corrections to compute the "prime" variables
    val ly = super.lylppCorrectionsCtx(secularCtx)
    val ly1 = ly._1
    val ϵ3 =  ly._2._1
    val esinω = ly._2._3
    val η  = ly._2._2 
    val `η²`  = η*η
    val ecosω = ly1.C
    val `ecosω²` = ecosω*ecosω
    
    // 2nd order corrections
    // note the singularity for (sinI) s = 0 
    // use s_ only in terms that would "explode" 
    val s_ = if (s < 1.5e-12.as[F]) 1.5e-12.as[F] else s
      
    val δI =   ϵ3 * esinω * c
    val δa =   0
    val δh = - ϵ3 * ecosω * c/s_
    val δC = - ϵ3 * ecosω * esinω * (1/s_ - 2*s_)
    val δS =   ϵ3 * s_ - ϵ3*`η²`*s_ - 2*ϵ3*`ecosω²`*s_ + ϵ3*`ecosω²`/s_
    val δF =   ϵ3 * s * ecosω * (1 - 2*`η²`/(1+η)) / 2    // Lyddane F' = (M´´ + g´´ + h´´) +  ...
    
    // add 2nd order corrections to the first order corrections
    (LyddaneElems(ly1.I + δI, ly1.a, ly1.h+δh, ly1.C+δC, ly1.S+δS, ly1.F+δF), (ϵ3,η,esinω)) 
  }
  
}