package predict4s.tle.lara

import spire.algebra.Field
import spire.algebra.NRoot
import spire.algebra.Order
import spire.algebra.Trig
import spire.implicits._
import spire.math._
import spire.syntax.primitives._
import predict4s.tle.SGPConstants
import predict4s.tle.TEME
import predict4s.tle._

/**
 *  Secular Effects of Earth Zonal Harmonics and Atmospheric Drag.
 *
 * Brouwer’s theory for finding the GeoPotential relies on Delaunay variables
 * • ℓ = M : mean anomaly
 * • g = ω: argument of the periapsis
 * • h = Ω: RAAN
 * • L = √(μ a): Delaunay action, where a is the semi-major axis
 * • G = √(μ p): total angular momentum, where p = a(1 − e²) is the orbit
 *       parameter (semilatus rectum), and e is the orbit eccentricity
 * • H = G cosI: projection of the angular momentum on the earth’s
 *       rotation axis, where I is the orbit inclination
 * 
 * From these initial elements, Brouwer’s theory provides the secular frequencies 
 * ℓdot, gdot, hdot as functions of (n0, e0, I0) due to the earth’s gravitational potential.
 * where the dot means time derivative. These frequencies remain constant for the evaluation 
 * of the theory at different dates.
 *      
 */
class SecularEffects[F : Field: NRoot : Order: Trig](val gpState : GeoPotentialState[F]) { 
  
  val eValidInterval = Interval.open(0.as[F],1.as[F])
  
  val ocofs = OtherCoefs(gpState)
  val lcofs = LaneCoefs(gpState)

  /** 
   *  This calculation updates the secular elements at epoch to the desired date given by the time t.
   *  t is the duration in minutes from the epoch */
  def calculate(t: F): SecularState[F] = {
    import gpState._
    import gcof._, dps.elem._
    import ocofs.{ωdot,Ωdot,mdot=>Mdot,Ωcof}
    import dps.ctx.wgs.KE
        
    // type safety: this is julian days +  min in day units 
    val refepoch = epoch + t / 1440.0
    
    val `t²` = t*t
    // val t2   = t*t

    // Gravity corrections
    // Brouwer’s gravitational corrections are applied first
    // here we are with Delaunays variables, 
    // ωdot is gdot, Mdot is ℓdot, and  Ωdot is hdot.
    
    val ωdf  = ω + ωdot*t
    val Ωdf  = Ω + Ωdot*t
    val Mdf  = M + Mdot*t
    
    // Next, the secular corrections due to the atmospheric drag are incorporated;
    // in particular δh, δL, δe, δℓ (in Delaunay's)
    
    // It should be noted that when epoch perigee height is less than
    // 220 kilometers, the equations for a and Lane's are truncated after the C1 term, 
    // and the terms involving C5 , δω, and δM are dropped.

    import dps.isImpacting,gctx.η
    import lcofs._
    import ocofs.{ωcof,delM0,sinM0,Mcof,twopi}

    val (tempa,tempe,templ,ωm, mp) : (F,F,F,F,F) = 
      if (isImpacting) (1 - C1*t, bStar*C4*t, t2cof*`t²`, ωdf, Mdf)
      else {
        val `t³` = `t²`*t
        val `t⁴` = `t²`*`t²`
        val δω  = ωcof*t
        val δM : F = Mcof*( (1+η*cos(Mdf))**3 - delM0)
        val Mpm_ = Mdf + δω + δM
        val ωm_  = ωdf - δω - δM
       
        (1 - C1*t - D2*`t²` - D3*`t³` - D4*`t⁴`, 
          bStar*(C4*t + C5*(sin(Mpm_) - sinM0)), 
          t2cof*`t²` + t3cof*`t³` + `t⁴` * (t4cof + t*t5cof),
          ωm_, 
          Mpm_)
      }

    // Compute the secular elements (not exactly secular: they mix long-period terms from drag)

    val am : F  = ((KE/n) fpow (2.0/3.0).as[F]) * tempa*tempa // a * tempa**2  
    val nm : F  = KE / (am pow 1.5)
    val em_  = e - tempe
    val Ωm   = Ωdf + Ωcof*`t²` 
    
     // fix tolerance for error recognition
     // sgp4fix am is fixed from the previous nm check
    // FIXME: can we use intervals?
    if (!eValidInterval.contains(em_))
       {
         // sgp4fix to return if there is an error in eccentricity
         // FIXME: we should move to use Either
        // return TEME.SGPElems[F](nm, em_, i, ωm, Ωm, mp, am, bStar, epoch)  
        return SecularState(t, TEME.SGPElems(nm, em_, i, ωm, Ωm, mp, am, bStar, epoch), ocofs) 
       }

     // sgp4fix fix tolerance to avoid a divide by zero
     val em = if (em_ < 1.0e-6.as[F]) 1.0e-6.as[F] else em_ 
    
     val Mm_  = mp + n*templ
     
     // modulus so that the angles are in the range 0,2pi
     val Ω_      = Ωm  % twopi
     val ω_      = ωm  % twopi
     
     val ℓm      = Mm_ + ωm + Ωm
     val lm      = ℓm  % twopi
     val Mm      = (lm - ω_ - Ω_) % twopi
     
    // Return a different structure here for the long periodic effects
    SecularState(t, TEME.SGPElems(nm, em, i, ω_, Ω_, Mm, am, bStar, epoch), ocofs) 
  }
   
}

object SecularEffects {
  def apply[F : Field: NRoot : Order: Trig](gpState : GeoPotentialState[F]) : SecularEffects[F] = new SecularEffects(gpState)
}

// case class SecularState[F](t: F, elems: TEME.SGPElems[F], ocofs : OtherCoefs[F], lcofs : LaneCoefs[F])

case class SecularState[F](t: F, elems: TEME.SGPElems[F], ocofs : OtherCoefs[F])
