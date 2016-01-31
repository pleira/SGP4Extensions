package predict4s.sgp
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.coord._
import org.scalactic.Or
import org.scalactic.Good
import org.scalactic.Bad

case class SecularFrequencies[F](Mdot: F, ωdot: F, Ωdot: F)

case class DragSecularCoefs[F](Mcof: F, ωcof: F, Ωcof: F)

case class LaneCoefs[F](T2: F, T3: F, T4: F, T5: F)

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
    )  {

  type Minutes = F // type to remember dealing with minutes from epoch

  // valid interval for eccentricity calculations
  val eValidInterval = Interval.open(0.as[F],1.as[F])
     
  def secularCorrections(t: Minutes): SGPSecularResult[F] = { // : Either[String, SGPElems[F]] = {
    
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
   
    val (δL, δe, δℓ, ωm, xMp, xΩm) : (F,F,F,F,F,F) = dragSecularCorrections(t, ωdf, Mdf, Ωdf)
    val Ωm = xΩm  // it seems Scala has some trouble with certain variable names
    val Mp = xMp

    // Compute the secular elements (not exactly secular as they mix long-period terms from drag)
    val am : F  = ((KE/n) fpow (2.0/3.0).as[F]) * δL * δL // a * tempa**2  
    val nm : F  = KE / (am pow 1.5)
    val em_ : F = e - δe
    
    // fix tolerance for error recognition
    // sgp4fix am is fixed from the previous nm check
    if (!eValidInterval.contains(em_))
      {
        // sgp4fix to return if there is an error in eccentricity
        return Bad(s"Secular eccentricity $em_ outside valid range")
      }

    // sgp4fix fix tolerance to avoid a divide by zero
    val em = if (em_ < 1.0e-6.as[F]) 1.0e-6.as[F] else em_ 
    
    val Mm_ = Mp + n*δℓ
     
    // modulus so that the angles are in the range 0,2pi
    val Ω_ = Ωm  % `2pi`
    val ω_ = ωm  % `2pi`
    
    // Lyddane's variables and back 
    val ℓm = Mm_ + ωm + Ωm
    val lm = ℓm  % `2pi`
    val Mm = (lm - ω_ - Ω_) % `2pi`
    val elems = SGPElems(nm, em, I, ω_, Ω_, Mm, am, bStar, epoch)
    val elemCtx = (elems,ctx0.inclinationCtx,wgs)
    Good(elemCtx)
  }
  
  /*
   * The secular corrections due to atmospheric drag are calculated
   * by Brouwer/Lane in Delauneys variables. All the necessary constants and coefficients
   * have been evaluated at the initialization stage.
   * δL,δe,δℓ,δh	
   */
  def dragSecularCorrections(t: Minutes, ωdf: F, Mdf: F, Ωdf: F): (F,F,F,F,F,F) = {

    import laneCoefs._
    import dragCoefs._ // {ωcof,delM0,sinM0,Mcof}    
    import geoPot._ 
    import gctx.η 
    import elem0.{bStar,M}
    
    val `t²` : F = t*t    
    val Ωm  : F = Ωdf + Ωcof*`t²` 
 
    // It should be noted that when epoch perigee height is less than
    // 220 kilometers, the equations for a and Lane's are truncated after the C1 term, 
    // and the terms involving C5 , δω, and δM are dropped.    
    if (isImpacting) 
      return (1 - C1*t, bStar*C4*t, T2*`t²`, ωdf, Mdf, Ωm)
    
    val `t³` = `t²`*t
    val `t⁴` = `t²`*`t²`
    val delM0 = (1+η*cos(M))**3
    val δω : F = ωcof*t
    val δM : F = Mcof*( (1+η*cos(Mdf))**3 - delM0)
    val Mpm_ : F = Mdf + δω + δM
    val ωm_  : F = ωdf - δω - δM
    
    val δL = 1 - C1*t - D2*`t²` - D3*`t³` - D4*`t⁴`  // (L´´/L0) 
    val δe = bStar*(C4*t + C5*(sin(Mpm_) - sin(M)))  // sin(M) === sin(M0)
    val δℓ =  T2*`t²` + T3*`t³` + `t⁴` * (T4 + t*T5) // (ℓ´´ - ℓj´´)/ n0

    (δL, δe, δℓ, ωm_, Mpm_, Ωm)
  }
  
  def inclCtx = ctx0.inclinationCtx
}

object BrouwerLaneSecularCorrections {
  
  def apply[F : Field : NRoot : Order : Trig](elem0Ctx0: (SGPElems[F], Context0[F]), wgs0: SGPConstants[F]) :  BrouwerLaneSecularCorrections[F] = {
    val (elem, wgs, ctx0, geoPot, gctx, laneCoefs, secularFreqs, dragCoefs, isImpacting, rp) = Factory2ndOrderSecularCorrectionsTerms.from(elem0Ctx0, wgs0)
    new BrouwerLaneSecularCorrections(elem, wgs, ctx0, geoPot, gctx, laneCoefs, secularFreqs, dragCoefs, isImpacting, rp)
  }
  
  def build[F : Field : NRoot : Order : Trig](tle: TLE, wgs: SGPConstants[F]) :  BrouwerLaneSecularCorrections[F] Or ErrorMessage = for {
    elem0AndCtx <- SGPElemsConversions.sgpElemsAndContext(tle, wgs)
  } yield BrouwerLaneSecularCorrections(elem0AndCtx, wgs)
    
}