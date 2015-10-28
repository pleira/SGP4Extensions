package predict4s.tle

import spire.algebra.Field
import spire.algebra.NRoot
import spire.algebra.Order
import spire.algebra.Trig
import spire.implicits._
import spire.math._
import spire.syntax.primitives._


/**
 *  Secular Effects of Earth Zonal Harmonics and Atmospheric Drag 
 */
trait EarthHarmonicsAndDragSecularEffects { 
  
  /** t is the duration in minutes from the epoch , then the SGP4 Time Independent Functions */
  def propagate[F: Field: NRoot : Order: Trig](t: F)(tind: SGP4TimeIndependentFunctions[F])
  (implicit wgs: WGSConstants[F])
         : (SGP4TimeIndependentFunctions[F], TEME.SGPElems[F], F) = {
    import tind._
    import tind.i0f._
    import tind.e0f._
    import tind.ocf._
    import tind.coeff._
    import tind.sf._
    import tind.bmmf._
    import tind.ilf._
    import tind.ini.{M0,Ω0}
    import wgs.KE
    
    val refepoch = tind.ini.epoch + t
    
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
          bStar*C4*t + bStar*C5*(sin(Mpm) - sinM0), 
          t2cof*t2 + t3cof*t3 + t4 * (t4cof + t*t5cof),
          ωm, Mpm)
      }
    // tempe = bStar*C4*t - B*C5(sinMp - sinM0)
//     am = pow((xke / nm),x2o3) * tempa * tempa;
//     nm = xke / pow(am, 1.5)
    
    // am as in Vallado's code. Check as well Hoot's: val am = a0 * tempa**2 
    val am = (KE/n0) fpow (2.0/3.0).as[F] * tempa*tempa  
    
    val nm = KE / (am fpow 1.5.as[F]) // mean motion
    val emt = e0 - tempe
     // fix tolerance for error recognition
     // sgp4fix am is fixed from the previous nm check
    // FIXME: can we use intervals?
     if ((emt >= 1.as[F]) || (emt < -0.001.as[F]))
       {
         // sgp4fix to return if there is an error in eccentricity
         // return false;
        return (tind, TEME.SGPElems[F](am, emt, i0, ωm, Ωm, _Mp, bStar, refepoch), am)  
       }

     // sgp4fix fix tolerance to avoid a divide by zero
     val em = if (emt < 1.0e-6.as[F]) 1.0e-6.as[F] else emt  // eccentricity
    
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
    (tind, TEME.SGPElems[F](nm, em, i0, ω, Ω, Mm,bStar, refepoch), am)  
    // Return a different structure here for the long periodic effects
    
  }
   
}


object SecularEffects extends EarthHarmonicsAndDragSecularEffects

  