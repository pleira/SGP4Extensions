package predict4s.tle.vallado

import spire.algebra._
import spire.implicits._
import spire.math._
import spire.syntax.primitives._
import predict4s.tle._

/**
 * Brouwer’s theory finds trouble when evaluating the short-period
 * corrections for the lower eccentricity orbits. The trouble is artificial, and is
 * related to the singularity of Delaunay variables for circular orbits. Hence,
 * SGP4 implements a different set of elements based on Lyddane’s approach
 * which completely avoids that trouble. In particular, the elements used in
 * the computation of short-period corrections are
 * F = ℓ + g + h, C = e*cosω, S = e*sinω, a, I, h
 */
trait ShortPeriodPeriodicEffects {
  

   /* --------- position and velocity (in km and km/sec) ---------- */
  def convertUnitVectors[F: Field: NRoot : Order: Trig](pos : Vector[F], vel : Vector[F], mrt: F, mvt: F, rvdot: F)
  (implicit wgs: SGPConstants[F]) : (Vector[F], Vector[F]) = {
      import wgs._      
     ( (aE*mrt) *: pos,  vkmpersec *: (mvt *: pos + rvdot *: vel))
  }
  
  def calculate[F: Field: NRoot : Order: Trig](eaState: EccentricAnomalyState[F]) = {
    
    import eaState._ 
    import lppState._
    import secularState._
    import elems.{n=>nm,Ω => nodep,a => am,M=>mp,ω=>argpp,i=>xincp,e=>ep,_}, ocofs._
    import gpState._
    import dps._
    import ctx.{cosI0=>cosIp,sinI0=>sinIp,_}
    import wgs._
  
     /* ------------- short period preliminary quantities ----------- */  
     // Compute polar variables
     // Change from Lyddane non-singular variables to polar-nodal variables
    
     val el2   = axnl*axnl + aynl*aynl
     val pl    = am*(1 - el2)                          // pl is C², where replacing C² by μa(1 − e²) gives simplified eq between dt and dr.
     if (pl < 0.as[F]) throw new Exception("pl: " + pl)

     // References done to the formulas given in Handbook of Satellite Orbits, Chapter 4
     // Consider the plane containing the position and velocity vectors of the satellite centered in the ellipse focus as our Earth)
    
     // It follows the usual transformation to polar-nodal variables
     // (r, θ, R, Θ) −→ (F, C, S, a)  with C' = e'cosg and  S' = e'sing
     // Note: Vallado's SGP4 uses rθdot = Θ/r instead of Θ
    
     val    rl     = am * (1 - ecosE)                  // 4.64, change of variable to E related to r, as in 4.63
     val    rdotl  = sqrt(am) * esinE/rl               // 4.67, simple manIpulations rdot (note missing √μ factor)
     val    rvdotl = sqrt(pl) / rl                     // ??? 4.68, r·rdot = √(μa)* esinE 
     val    betal  = sqrt(1 - el2)
     val    temp0  = esinE / (1 + betal)
     
     // ???? u is the true anomaly that can be defined immediately as the polar angle = (Ox, OS), x along the semimajor axis, S sat position
     val    sinu   = am / rl * (sineo1 - aynl - axnl * temp0)             // ??? 4.71,  y = r sinu = a * sqrt(1 − e2) * sinE
     val    cosu   = am / rl * (coseo1 - axnl + aynl * temp0)             // ??? 4.70,  x = r cosu = a(cosE − e)
     val    su0    = atan2(sinu, cosu)
     val    sin2u  = (cosu + cosu) * sinu
     val    cos2u  = 1 - 2.0 * sinu * sinu
     val    temp   = 1/ pl
     val    temp1  = 0.5 * J2 * temp
     val    temp2  = temp1 * temp

         /* -------------- update for short period gravitational periodics ------------ */
//         if (satrec.method == 'd')
//           {
//             cosisq                 = cosIp * cosIp
//             satrec.con41  = 3.0*cosisq - 1.0
//             satrec.x1mth2 = 1- cosisq
//             satrec.x7thm1 = 7.0*cosisq - 1.0
//           }
     val    mrt   = rl * (1 - 1.5 * temp2 * betal * con41) + 0.5 * temp1 * x1mth2 * cos2u
     val    su    = su0 - 0.25 * temp2 * x7thm1 * sin2u
     val    xnode = nodep + 1.5 * temp2 * cosIp * sin2u
     val    xinc  = xincp + 1.5 * temp2 * cosIp * sinIp * cos2u
     val    mvt   = rdotl - nm * temp1 * x1mth2 * sin2u / KE
     val    rvdot = rvdotl + nm * temp1 * (x1mth2 * cos2u + 1.5 * con41) / KE
     
     val  elem = TEME.SGPElems(nm, ep, xinc, argpp, xnode, mp, am, bStar, epoch) 
     ShortPeriodPeriodicState(elem, xinc, su, xnode, mrt, mvt, rvdot, eaState)
  }

}

object ShortPeriodPeriodicEffects extends ShortPeriodPeriodicEffects


case class ShortPeriodPeriodicState[F](
    elem: TEME.SGPElems[F], 
    I: F,     // inclination 
    R: F,     // Radial velocity    
    Ω: F,     // argument of the node
    mrt: F, 
    mvt: F, 
    rvdot: F, 
    eaState: EccentricAnomalyState[F])