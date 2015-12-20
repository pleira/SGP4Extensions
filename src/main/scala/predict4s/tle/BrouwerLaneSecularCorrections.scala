package predict4s.tle
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.coord.CartesianElems
  

  
class BrouwerLaneSecularCorrections[F : Field : NRoot : Order : Trig]( 
    val elem0: SGPElems[F],    
    val wgs: SGPConstants[F],
    val ctx0: Context0[F],
    val geoPot: GeoPotentialCoefs[F],
    val gctx: GeoPotentialContext[F],
    val laneCoefs : LaneCoefs[F],
    val secularFreqs : SecularFrequencies[F], 
    val dragCoefs : DragSecularCoefs[F],
    val isImpacting: Boolean,
    val rp: F
    ) extends HelperTypes[F] with SecularCorrections[F] {

  // valid interval for eccentricity calculations
  val eValidInterval = Interval.open(0.as[F],1.as[F])

  case class DelauneyMixedVars(l: F, g: F, h: F, a: F, e: F, I: F) {
    def M = l; def ω = g; def Ω = h;
  }
 
  def sgpelem2MixedVars(el : SGPElems[F]) = {
    import el._
    val ℓ = M  // mean anomaly
    val g = ω  // argument of the periapsis
    val h = Ω  // RAAN
    DelauneyMixedVars(ℓ,g,h,el.a,el.e,el.I)
  }    
 

  case class DelauneyVars(l: F, g: F, h: F, L: F, G: F, H: F) {
    def sgpelem2Delauney(elem : SGPElems[F]) = {
      import elem._
      val ℓ = M  // mean anomaly
      val g = ω  // argument of the periapsis
      val h = Ω  // RAAN
      val L = sqrt(a)  // Delaunay action, in reality sqrt(nu*a) but nu is taken out to be added later
      val G = L * sqrt(1 - e*e)
      val H = G * cos(I)
      DelauneyVars(ℓ,g,h,L,G,H)
    }    
  }  
 
  case class LyddaneVars(F: F, S: F, C: F, L: F, h: F, cosI: CosI) {
    def a = L*L
  }
  
  def delauney2Lyddane(vars: DelauneyVars) = {
      import vars._
      val F = l + g + h
      val S = e*sin(g)
      val C = e*cos(g)
      val cosI : CosI = H/G
      LyddaneVars(F,S,C,L,h,cosI)      
  } 
  
  /**
   * Secular terms in SGP4 are computed in Delauney variables, that is, we obtain ℓ, 𝘨, 𝘩, 𝐿, 	𝘎, 𝐻
   * but we use DelauneyMixedVars ℓ, 𝘨, 𝘩, a, e, I
   */
//  def delauneySecularCorrections(t: Minutes): DelauneyMixedVars = {
//    import secularFreqs._  // {ωdot,Ωdot,mdot=>Mdot,Ωcof}
//    import dragCoefs._  
//    import elem0._, wgs._
// 
//    // Brouwer’s gravitational corrections are applied first
//    // ωdot is gdot, Mdot is ℓdot, and  Ωdot is hdot. 
//    val ωdf  : F = ω + ωdot*t
//    val Ωdf  : F = Ω + Ωdot*t
//    val Mdf  : F = M + Mdot*t    
//    
//    // Next, the secular corrections due to the atmospheric drag are incorporated
//    // which also take long period terms from drag;
//    // in particular δh, δL, δe, δℓ 
//   
//    val (δL, δe, δℓ, ωm, δM) : (F,F,F,F,F) = dragSecularCorrections(t, ωdf, Mdf)
//    val `t²` : F = t**2
// 
//    // Compute the secular elements (not exactly secular as they mix long-period terms from drag)
//    val am : F  = ((KE/n) fpow (2.0/3.0).as[F]) * δL * δL // a * tempa**2    
//    val em_ : F = e - δe
//    val Ωm  : F = Ωdf + Ωcof*`t²` 
//    
//    // fix tolerance for error recognition
//    // sgp4fix am is fixed from the previous nm check
//    if (!eValidInterval.contains(em_))
//      {
//        // sgp4fix to return if there is an error in eccentricity
//        // FIXME: we should move to use Either
//        // return SGPElems(nm, em_, I, ωm, Ωm, mp, am, bStar, epoch) 
//        DelauneyMixedVars(δM,ωm,Ωm,am,em_,I) 
//      }
//
//    // sgp4fix fix tolerance to avoid a divide by zero
//    val em = if (em_ < 1.0e-6.as[F]) 1.0e-6.as[F] else em_ 
//    
//    val Mm_  = δM + n*δℓ
//     
//    // modulus so that the angles are in the range 0,2pi
//    val Ω_ = Ωm  % twopi
//    val ω_ = ωm  % twopi
//    
//    // Lyddane's variables and back 
//    val ℓm = Mm_ + ωm + Ωm
//    val lm = ℓm  % twopi
//    val Mm = (lm - ω_ - Ω_) % twopi   
//    DelauneyMixedVars(Mm,ω_,Ω_,am,em,I)  
//  }
  
  override def secularCorrections(t: Minutes): SGPElems[F] = {
    
    import secularFreqs._  // {ωdot,Ωdot,mdot=>Mdot,Ωcof}
    import dragCoefs._  
    import elem0._, wgs._
 
    // Brouwer’s gravitational corrections are applied first
    // Note that his theory relies on Delaunays variables, 
    // ωdot is gdot, Mdot is ℓdot, and  Ωdot is hdot.
    val ωdf  : F = ω + ωdot*t
    val Ωdf  : F = Ω + Ωdot*t
    val Mdf  : F = M + Mdot*t    
    
    // Next, the secular corrections due to the atmospheric drag are incorporated
    // which also take long period terms from drag;
    // in particular δh, δL, δe, δℓ 
   
    val (δL, δe, δℓ, ωm, mp) : (F,F,F,F,F) = dragSecularCorrections(t, ωdf, Mdf)

    val `t²` : F = t**2
    // Compute the secular elements (not exactly secular as they mix long-period terms from drag)
    val am : F  = ((KE/n) fpow (2.0/3.0).as[F]) * δL * δL // a * tempa**2  
    val nm : F  = KE / (am pow 1.5)
    val em_ : F = e - δe
    val Ωm  : F = Ωdf + Ωcof*`t²` 
    
    // fix tolerance for error recognition
    // sgp4fix am is fixed from the previous nm check
    if (!eValidInterval.contains(em_))
      {
        // sgp4fix to return if there is an error in eccentricity
        // FIXME: we should move to use Either
        return SGPElems(nm, em_, I, ωm, Ωm, mp, am, bStar, epoch) 
      }

    // sgp4fix fix tolerance to avoid a divide by zero
    val em = if (em_ < 1.0e-6.as[F]) 1.0e-6.as[F] else em_ 
    
    val Mm_  = mp + n*δℓ
     
    // modulus so that the angles are in the range 0,2pi
    val Ω_      = Ωm  % twopi
    val ω_      = ωm  % twopi
    
    // Lyddane's variables and back 
    val ℓm      = Mm_ + ωm + Ωm
    val lm      = ℓm  % twopi
    val Mm      = (lm - ω_ - Ω_) % twopi   
    SGPElems(nm, em, I, ω_, Ω_, Mm, am, bStar, epoch)
  }
   
  def dragSecularCorrections(t: Minutes, ωdf: F, Mdf: F): (F,F,F,F,F) = {

    import laneCoefs._
    import dragCoefs._ // {ωcof,delM0,sinM0,Mcof}    
    import geoPot._ 
    import gctx.η 
    import elem0.{bStar,M}
    
    val `t²` : F = t**2    
 
    // It should be noted that when epoch perigee height is less than
    // 220 kilometers, the equations for a and Lane's are truncated after the C1 term, 
    // and the terms involving C5 , δω, and δM are dropped.    
    if (isImpacting) 
      return (1 - C1*t, bStar*C4*t, t2cof*`t²`, ωdf, Mdf)
    
    val `t³` = `t²`*t
    val `t⁴` = `t²`*`t²`
    val δω : F = ωcof*t
    val δM : F = Mcof*( (1+η*cos(Mdf))**3 - delM0)
    val Mpm_ : F = Mdf + δω + δM
    val ωm_  : F = ωdf - δω - δM
    
    val δL = 1 - C1*t - D2*`t²` - D3*`t³` - D4*`t⁴`  // (L´´/L0) 
    val δe = bStar*(C4*t + C5*(sin(Mpm_) - sin(M)))  // sin(M) === sin(M0)
    val δℓ =  t2cof*`t²` + t3cof*`t³` + `t⁴` * (t4cof + t*t5cof)   // (ℓ´´ - ℓj´´)/ n0
    
    (δL, δe, δℓ, ωm_, Mpm_)
  }

}

case class SecularFrequencies[F](Mdot: F, ωdot: F, Ωdot: F)

case class DragSecularCoefs[F](Mcof: F, ωcof: F, Ωcof: F, xlcof: F, aycof: F, delM0: F)

case class LaneCoefs[F](t2cof: F, t3cof: F, t4cof: F, t5cof: F)


object BrouwerLaneSecularCorrections extends SecularCorrectionsFactory {
  
  def apply[F : Field : NRoot : Order : Trig](elem0Ctx0: (SGPElems[F], Context0[F]))(implicit wgs0: SGPConstants[F]) :  BrouwerLaneSecularCorrections[F] = {
    val (elem, wgs, ctx0, geoPot, gctx, laneCoefs, secularFreqs, dragCoefs, isImpacting, rp) = from(elem0Ctx0)
    new BrouwerLaneSecularCorrections(elem, wgs, ctx0, geoPot, gctx, laneCoefs, secularFreqs, dragCoefs, isImpacting, rp)
  }
  
}