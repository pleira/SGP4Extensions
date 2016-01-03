package predict4s.sgp

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.coord.SpecialPolarNodal
import predict4s.coord.Context0
import predict4s.coord.SGPConstants
import predict4s.coord.Context0

trait ShortPeriodPolarNodalCorrections[F] {
  
  val wgs: SGPConstants[F]
  val ctx0: Context0[F]
  
  def sppCorrections(lppState: (SpecialPolarNodal[F], LongPeriodContext[F]))
    (implicit ev: Field[F], trig: Trig[F] ): (SpecialPolarNodal[F], SpecialPolarNodal[F]) = {
    import lppState.{_1 => lppPN,_2 => lppc}
    import lppPN._,lppc._
    import wgs.{J2,KE}
    import ctx0.{c,s,`7c²-1`,`s²`,`3c²-1`}
 
    val `J2/p/2` = J2 / pl / 2
    val ϵ2 = - `J2/p/2` / pl / 2
    val δI = - 3 * ϵ2 * c * s * cos2θ
    val δθ =       ϵ2 * `7c²-1` * sin2θ / 2
    val δΩ = - 3 * ϵ2 * c * sin2θ
    val δr =       ϵ2 * (3 * r * βl * `3c²-1` - pl* `s²` * cos2θ)   
      
    val δR = - n * `J2/p/2` * `s²` * sin2θ / KE  // rdot, angular velocity
    val δrvdot = n * `J2/p/2` * (`s²` * cos2θ + 1.5 * `3c²-1`) / KE 
    val δspp = SpecialPolarNodal(δI,δθ,δΩ,δr,δR,δrvdot)
    val finalPN = lppPN + δspp
    (finalPN, δspp)
  }
  
}