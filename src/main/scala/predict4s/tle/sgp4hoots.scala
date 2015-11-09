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
    val (el, am) = secularEffects.secularEffects(t)
    val (nodep, axnl, aynl, xl) = SGP4LongPeriodicEffects.calcHootsSGP4LongPeriodicEffects(tif, el, ocoef, am)
    val (eo1,ecosE,esinE) = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(nodep, axnl, aynl, xl)
    val nm    = el.n0
    val xincp = el.i0
    val cosip = cos(xincp)
    val sinip = sin(xincp)
    // here, should be something returned before in other coordinates 
    val posVel = ShortPeriodPeriodicPerturbations.calcPositionVelocity(tif, ocoef, ctx1, nm, xincp, cosip, sinip, am, nodep, axnl, aynl, xl, eo1)
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
    val (el, am) = secularEffects.secularEffects(t0)

    val (nodep, axnl, aynl, xl) = SGP4LongPeriodicEffects.calcHootsSGP4LongPeriodicEffects(tif, el, ocoef, am)
    
    val (eo1,ecosE,esinE) = NewtonRaphsonKeplerSolver.solveEccentricAnomaly(nodep, axnl, aynl, xl)
    val nm    = el.n0
    val xincp = el.i0
    val cosip = cos(xincp)
    val sinip = sin(xincp)
    // here, should be something returned before in other coordinates 
    val posVel0 = ShortPeriodPeriodicPerturbations.calcPositionVelocity(tif, ocoef, ctx1, nm, xincp, cosip, sinip, am, nodep, axnl, aynl, xl, eo1)
    val context0 = SGP4HootsContext(t0, elem0, posVel0, tif, secularEffects, wgs)
    SGP4Hoots(context0)
  }
  
}



/**
 *  Secular Effects of Earth Zonal Harmonics and Atmospheric Drag 
 */
case class HootsSecularEffects[F : Field: NRoot : Order: Trig](potential : GeoPotentialCoefs[F], tif: SGP4TIF[F], ctx1: Context1[F]) { 
  
  import tif._, ctx1._
  import ctx._, elemsdp._
  
  val ocoefs = HootsOtherCoefs(elemsdp, ctx, ctx1, potential)

  // TODO: try to express this operation as being part of an AST with a single Context and the time in minutes as parameters, 
  // returning a description, that is the secular effect perturbed elements and an updated Context
  /** t is the duration in minutes from the epoch */
  def secularEffects(t: F) : (TEME.SGPElems[F], F) = {
    import ocoefs._
    import ctx._, elemsdp._
    import potential._
    
    // type safety: this is julian days +  min in day units 
    val refepoch = tif.ini.epoch + t / 1440.0
    
    val ωdf : F = ω0 + ωdot*t
    val Ωdf : F = Ω0 + Ωdot*t
    val Mdf : F = M0 + mdot*t
    
    val t2 = t*t
    val Ωm = Ωdf + Ωcof*t2 // nodem, right asc of ascending node
    
    // TODO See if Delaunays variables can be introduced 
    
    // It should be noted that when epoch perigee height is less than
    // 220 kilometers, the equations for a and IL are truncated after the C1 term, 
    // and the terms involving C5 , δω, and δM are dropped.

    // FIXME: the isImpacting term is always calculated, see if that can be expressed
    val (tempa,tempe,templ,ωm, _Mp) : (F,F,F,F,F) = 
      if (isImpacting) (1 - C1*t, bStar*C4*t, t2cof*t2, ωdf, Mdf)
      else {
        val t3 = t2*t
        val t4 = t2*t2
//         delomg = satrec.omgcof * satrec.t;
//         delm   = satrec.xmcof *
//                  (pow((1.0 + satrec.eta * cos(xmdf)), 3) -
//                  satrec.delmo);
//         temp   = delomg + delm;
//         mm     = xmdf + temp;
//         argpm  = argpdf - temp;
//         t3     = t2 * satrec.t;
//         t4     = t3 * satrec.t;
//         tempa  = tempa - satrec.d2 * t2 - satrec.d3 * t3 -
//                          satrec.d4 * t4;
//         tempe  = tempe + satrec.bstar * satrec.cc5 * (sin(mm) -
//                          satrec.sinmao);
//         templ  = templ + satrec.t3cof * t3 + t4 * (satrec.t4cof +
//                          satrec.t * satrec.t5cof);
        val δω  = ωcof*t
        val δM  = Mcof*( (1+η*cos(Mdf))**3 - delM0)
        val Mpm = Mdf + δω + δM
        val ωm  = ωdf - δω - δM
        
        (1 - C1*t - D2*t2 - D3*t3 - D4*t4, 
          bStar*(C4*t + C5*(sin(Mpm) - sinM0)), 
          t2cof*t2 + t3cof*t3 + t4 * (t4cof + t*t5cof),
          ωm, Mpm)
      }
    // tempe = bStar*C4*t - B*C5(sinMp - sinM0)
//     am = pow((xke / nm),x2o3) * tempa * tempa;
//     nm = xke / pow(am, 1.5)
    
    val am = a0 * tempa**2  
    
    val nm = Ke / (am pow 1.5) // mean motion
    val e  = e0 - tempe
    
    // val emt = e  
     // fix tolerance for error recognition
     // sgp4fix am is fixed from the previous nm check
    // FIXME: can we use intervals?
     if ((e >= 1.as[F]) || (e < -0.001.as[F]))
       {
         // sgp4fix to return if there is an error in eccentricity
         // return false;
        return (TEME.SGPElems[F](am, e, i0, ωm, Ωm, _Mp, bStar, refepoch), am)  
       }

     // sgp4fix fix tolerance to avoid a divide by zero
     val em = if (e < 1.0e-6.as[F]) 1.0e-6.as[F] else e 
    
     val mm   = _Mp + n0*templ
     val xlm  = mm + ωm + Ωm
     val emsq = em * em
     val temp = 1 - emsq

     // modulus so that the angles are in the range 0,2pi
     val Ω       = Ωm  % twopi
     val ω       = ωm  % twopi
     val lm      = xlm % twopi
     val Mm      = (lm - ω - Ω) % twopi
     
     // Better SGPElems (radpm0,e0,i0,pa,raan,M0,bStar,refepoch)
    (TEME.SGPElems[F](nm, em, i0, ω, Ω, Mm,bStar, refepoch), am)  
    // Return a different structure here for the long periodic effects
    
  }
   
}


object HootsSecularEffects // extends HootsSecularEffects


