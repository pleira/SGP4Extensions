package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._

import TEME._

case class SGP4HootsContext[F](t: F, elem: TEME.SGPElems[F], posVel: TEME.CartesianElems[F], 
    tif : SGP4TIF[F], sec: HootsSecularEffects[F], wgs: SGPConstants[F])

case class SGP4Hoots[F : Field : NRoot : Order : Trig](state0: SGP4HootsContext[F])  { // extends SGP4[F]
    
  implicit val wgs: SGPConstants[F] = state0.wgs
  val secularEffects =  state0.sec
  val ocoef = secularEffects.ocoefs
  val tif = state0.tif
  val ctx1 = secularEffects.ctx1
  
  def propagate(t: F) : OrbitalState[F] = {
    val el = secularEffects.secularEffects(t)
    val (nodep, axnl, aynl, xl) = SGP4LongPeriodicEffects.calcHootsSGP4LongPeriodicEffects(tif, el, ocoef)
    val (eo1,ecosE,esinE) = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(nodep, axnl, aynl, xl)
    val nm    = el.n
    val xincp = el.i
    val cosip = cos(xincp)
    val sinip = sin(xincp)
    // here, should be something returned before in other coordinates 
    val posVel = ShortPeriodPeriodicPerturbations.calcPositionVelocity(tif, ocoef, ctx1, nm, xincp, cosip, sinip, el.a, nodep, axnl, aynl, xl, eo1)
    OrbitalState(t, posVel)
  }
}

object SGP4Hoots {

  def apply[F : Field : NRoot : Order : Trig](tle: TLE)(implicit wgs: SGPConstants[F]) : SGP4Hoots[F] = {
    
    val elem0 = TEME.sgpElems[F](tle)
    val tif   = SGP4TIF(elem0)
    val (potentialCoefs, ctx1) = tif.potentialCOEFs0
    val secularEffects = HootsSecularEffects(potentialCoefs, tif, ctx1)
    val ocoef = secularEffects.ocoefs
    val t0 = 0.as[F]
    val el = secularEffects.secularEffects(t0)

    val (nodep, axnl, aynl, xl) = SGP4LongPeriodicEffects.calcHootsSGP4LongPeriodicEffects(tif, el, ocoef)
    
    val (eo1,ecosE,esinE) = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(nodep, axnl, aynl, xl)
    val nm    = el.n
    val xincp = el.i
    val cosip = cos(xincp)
    val sinip = sin(xincp)
    // here, should be something returned before in other coordinates 
    val posVel0 = ShortPeriodPeriodicPerturbations.calcPositionVelocity(tif, ocoef, ctx1, nm, xincp, cosip, sinip, el.a, nodep, axnl, aynl, xl, eo1)
    val context0 = SGP4HootsContext(t0, elem0, posVel0, tif, secularEffects, wgs)
    SGP4Hoots(context0)
  }
  
}


/**
 *  Secular Effects of Earth Zonal Harmonics and Atmospheric Drag 
 */
case class HootsSecularEffects[F : Field: NRoot : Order: Trig](potential : GeoPotentialCoefs[F], tif: SGP4TIF[F], ctx1: Context1[F]) { 
  
  val eValidInterval = Interval.open(0.as[F],1.as[F])
  
  val ocoefs = HootsOtherCoefs(ctx1.elemsdp, tif.ctx, ctx1, potential)

  // TODO: try to express this operation as being part of an AST with a single Context and the time in minutes as parameters, 
  // returning a description, that is the secular effect perturbed elements and an updated Context
  /** t is the duration in minutes from the epoch */
  def secularEffects(t: F) : TEME.SGPElems[F] = {
    import ctx1.elemsdp._
    import ocoefs.{ωdot,Ωdot,mdot=>Mdot,Ωcof}
    
    // type safety: this is julian days +  min in day units 
    val refepoch = tif.ini.epoch + t / 1440.0
    
    val `t²` = t*t
    val t2   = t*t

    val ωdf  = ω + ωdot*t
    val Ωdf  = Ω + Ωdot*t
    val Ωm   = Ω + Ωdot*t + Ωcof*`t²` 
    val Mdf  = M + Mdot*t
    
    // TODO See if Delaunays variables can be introduced 
    
    // It should be noted that when epoch perigee height is less than
    // 220 kilometers, the equations for a and IL are truncated after the C1 term, 
    // and the terms involving C5 , δω, and δM are dropped.

    import potential._
    import ctx1.{isImpacting,η}
    import ocoefs.{ωcof,delM0,sinM0,Mcof,t2cof,t3cof,t4cof,t5cof,twopi}
    //import tif.wgs.{MU=>Ke}
    val Ke : F = tif.wgs.MU
    
    // FIXME: the isImpacting term is always calculated, see if that can be expressed
    val (tempa,tempe,templ,ωm, Mp) : (F,F,F,F,F) = 
      if (isImpacting) (1 - C1*t, bStar*C4*t, t2cof*`t²`, ωdf, Mdf)
      else {
        val `t³` = `t²`*t
        val `t⁴` = `t²`*`t²`
        val δω  = ωcof*t
        val δM  = Mcof*( (1+η*cos(Mdf))**3 - delM0)
        val Mpm_ = Mdf + δω + δM
        val ωm_  = ωdf - δω - δM
       
        (1 - C1*t - D2*`t²` - D3*`t³` - D4*`t⁴`, 
          bStar*(C4*t + C5*(sin(Mpm_) - sinM0)), 
          t2cof*`t²` + t3cof*`t³` + `t⁴` * (t4cof + t*t5cof),
          ωm_, 
          Mpm_)
      }
    
    val am   = a * tempa**2  
    val nm   = Ke / (am pow 1.5)
    val em_  = e - tempe
    
     // fix tolerance for error recognition
     // sgp4fix am is fixed from the previous nm check
    // FIXME: can we use intervals?
    if (!eValidInterval.contains(em_))
       {
         // sgp4fix to return if there is an error in eccentricity
         // FIXME: we should move to use Either
        return TEME.SGPElems[F](nm, em_, i, ωm, Ωm, Mp, am, bStar, refepoch)  
       }

     // sgp4fix fix tolerance to avoid a divide by zero
     val em = if (em_ < 1.0e-6.as[F]) 1.0e-6.as[F] else em_ 
    
     val Mm_  = Mp + n*templ
     val ℓm   = Mm_ + ωm + Ωm

     // modulus so that the angles are in the range 0,2pi
     val Ω_      = Ωm  % twopi
     val ω_      = ωm  % twopi
     val lm      = ℓm  % twopi
     val Mm      = (lm - ω_ - Ω_) % twopi
     
     // Better SGPElems (radpm0,e0,i0,pa,raan,M0,bStar,refepoch)
    TEME.SGPElems[F](nm, em, i, ω_, Ω_, Mm, am, bStar, refepoch)  
    // Return a different structure here for the long periodic effects
    
  }
   
}


object HootsSecularEffects // extends HootsSecularEffects


