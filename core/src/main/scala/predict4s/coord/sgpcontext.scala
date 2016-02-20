package predict4s
package coord

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import org.scalactic.Or
import org.scalactic.Good
import org.scalactic.Bad

// this class has the first informations related to the TLE processing
case class SGPElemsCtx[@sp(Double) F: Field: Order](elem: SGPElems[F], iCtx: InclinationCtx[F], eCtx: EccentricityCtx[F], wgs: SGPConstants[F]) {
    import elem.{a,e,n},wgs.{aE,`2pi`}
    
    /**
     *  radius of perigee
     */
    def rp = a*(1-e)
      
    /**
     *  perigee height, altitude relative to the earth's surface
     */
    def perigeeHeight =  (rp - 1) * aE
    
    def isImpacting : Boolean = rp < (220/aE + 1.as[F])  
    
    def isDeepSpace : Boolean = (`2pi`.as[F]/n) >= 225.as[F] 
}

case class InclinationCtx[@sp(Double) F: Field](c : F, s : F) {
  lazy val `s²` = s * s
  lazy val `c²` = c * c
  lazy val `c³` = `c²` * c
  lazy val `c⁴` = `c²` * `c²`
  lazy val `3c²-1` = 3 * `c²` - 1
  def cosI0 = c
  def `cos²I0` = `c²`
  def sinI0 = s
  // these are used in Brouwers notation
  def θ = c
  def `θ²` = `c²` // `cos²I0`
  def `θ³` = `c³` // cosI0 * `cos²I0`
  def `θ⁴` = `c⁴` // `cos²I0` * `cos²I0`
  def θsq = `c²` // `cos²I0`
}

object InclinationCtx {
  def apply[@sp(Double) F: Field: Trig](I: F) : InclinationCtx[F] = InclinationCtx(cos(I), sin(I))
}

case class EccentricityCtx[@sp(Double) F: Field: NRoot](eccentricity : F, `e²`: F, `β0²`: F) {
  val  β0 : F = sqrt(`β0²`) 
  val `β0³`: F = β0*`β0²`
  val `β0⁴`: F = `β0²`*`β0²`  
}


object EccentricityCtx {
    // valid interval for eccentricity calculations
    def checkEccentricityValidInterval[@sp(Double) F: Field: Order](e: F) : Boolean = Interval.open(0.as[F],1.as[F]).contains(e)
  
    def apply[@sp(Double) F: Field: Order: NRoot](e: F) : EccentricityCtx[F] =  EccentricityCtx(e,e*e, 1-e*e)
    
    def elliptical[@sp(Double) F: Field: NRoot : Order](e: F) : EccentricityCtx[F] Or ErrorMessage = 
      if (e > 0.as[F] && e < 1.as[F]) 
        Good(EccentricityCtx(e,e*e, 1-e*e))
      else 
        Bad(s"Problem with eccentricity $e")
        
}

