package predict4s.tle

import spire.algebra._
import spire.implicits._
import spire.math._
import spire.syntax.primitives._

// TODO: give physical meaning to the variables

case class ShortPeriodPeriodicState[F](elem: TEME.SGPElems[F], xinc: F, su: F, xnode: F, mrt: F, mvt: F, rvdot: F, eaState: EccentricAnomalyState[F])

// TODO: express the operations here as being part of an AST with a single Context as parameter, 
// returning a description, that is the new perturbed elements after short period periodic effects and an updated Context

trait ShortPeriodPeriodicPerturbations {
  
  
  def calcPositionVelocity[F: Field: NRoot : Order: Trig](tind: SGP4TimeIndependentFunctions[F], nm: F, xincp: F, cosip: F, sinip: F, am: F, nodep: F, axnl: F, aynl: F, xl : F, eo1: F)
  (implicit wgs: SGPConstants[F])  = {
     
    import tind._
    import i0f._
    import wgs._
    import ocf._
  
     /* ------------- short period preliminary quantities ----------- */
     // these can come from the kepler solver 
     val coseo1 = cos(eo1)
     val sineo1 = sin(eo1)
     val ecose = axnl*coseo1 + aynl*sineo1
     val esine = axnl*sineo1 - aynl*coseo1
     
     
     val el2   = axnl*axnl + aynl*aynl
     val pl    = am*(1 - el2)
     if (pl < 0.as[F]) throw new Exception("pl: " + pl)

     val    rl     = am * (1 - ecose)
     val    rdotl  = sqrt(am) * esine/rl
     val    rvdotl = sqrt(pl) / rl
     val    betal  = sqrt(1 - el2)
     val    temp0  = esine / (1 + betal)
     val    sinu   = am / rl * (sineo1 - aynl - axnl * temp0)
     val    cosu   = am / rl * (coseo1 - axnl + aynl * temp0)
     val    su0    = atan2(sinu, cosu)
     val    sin2u  = (cosu + cosu) * sinu
     val    cos2u  = 1 - 2.0 * sinu * sinu
     val    temp   = 1/ pl
     val    temp1  = 0.5 * J2 * temp
     val    temp2  = temp1 * temp

         /* -------------- update for short period periodics ------------ */
//         if (satrec.method == 'd')
//           {
//             cosisq                 = cosip * cosip
//             satrec.con41  = 3.0*cosisq - 1.0
//             satrec.x1mth2 = 1- cosisq
//             satrec.x7thm1 = 7.0*cosisq - 1.0
//           }
     val    mrt   = rl * (1 - 1.5 * temp2 * betal * con41) + 0.5 * temp1 * x1mth2 * cos2u
     val    su    = su0 - 0.25 * temp2 * x7thm1 * sin2u
     val    xnode = nodep + 1.5 * temp2 * cosip * sin2u
     val    xinc  = xincp + 1.5 * temp2 * cosip * sinip * cos2u
     val    mvt   = rdotl - nm * temp1 * x1mth2 * sin2u / KE
     val    rvdot = rvdotl + nm * temp1 * (x1mth2 * cos2u + 1.5 * con41) / KE
         // here
    
     // unit position and velocity 
    val PV: TEME.CartesianElems[F] = TEME.coord2UnitCartesian(xinc, su, xnode)
    // return position and velocity (in km and km/sec)
    val (p, v) = convertUnitVectors(PV.pos, PV.vel, mrt, mvt, rvdot)
    TEME.CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
  }



   /* --------- position and velocity (in km and km/sec) ---------- */
  def convertUnitVectors[F: Field: NRoot : Order: Trig](pos : Vector[F], vel : Vector[F], mrt: F, mvt: F, rvdot: F)
  (implicit wgs: SGPConstants[F]) : (Vector[F], Vector[F]) = {
      import wgs._      
     ( (aE*mrt) *: pos,  vkmpersec *: (mvt *: pos + rvdot *: vel))
  }

  def hootsConvertUnitVectors[F: Field: NRoot : Order: Trig](pos : Vector[F], vel : Vector[F], mrt: F, mvt: F, rvdot: F) 
      : (Vector[F], Vector[F]) = {      
     ( mrt *: pos,  (1.0/60.0).as[F] *: (mvt *: pos + rvdot *: vel))
  }

  
  def calcPositionVelocity[F: Field: NRoot : Order: Trig](eaState: EccentricAnomalyState[F]) = {
      // tif: SGP4TIF[F], ocf: HootsOtherCoefs[F], ctx1 : Context1[F], nm: F, xincp: F, cosip: F, sinip: F, am: F, nodep: F, axnl: F, aynl: F, xl : F, eo1: F) = {
    
    import eaState._ 
    import lppState._
    import secularState._
    import elems.{n=>nm,Ω => nodep,a => am,M=>mp,ω=>argpp,i=>xincp,e=>ep,_}, ocofs._
    import gpState._
    import dps._
    import ctx.{cosi0=>cosip,sini0=>sinip,_}
    import wgs._
  
     /* ------------- short period preliminary quantities ----------- */  
     
     val el2   = axnl*axnl + aynl*aynl
     val pl    = am*(1 - el2)                          // pl is C2 (2 is squared), where replacing C2 by μa(1 − e2 ) gives simplified eq between dt and dr.
     if (pl < 0.as[F]) throw new Exception("pl: " + pl)

     // References done to the formulas given in Handbook of Satellite Orbits, Chapter 4
     // Consider the plane containing the position and velocity vectors of the satellite centered in the ellipse focus as our Earth)
    
     val    rl     = am * (1 - ecosE)                  // 4.64, change of variable to E related to r, as in 4.63
     val    rdotl  = sqrt(am) * esinE/rl               // 4.67, simple manipulations rdot (note missing √μ factor)
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

         /* -------------- update for short period periodics ------------ */
//         if (satrec.method == 'd')
//           {
//             cosisq                 = cosip * cosip
//             satrec.con41  = 3.0*cosisq - 1.0
//             satrec.x1mth2 = 1- cosisq
//             satrec.x7thm1 = 7.0*cosisq - 1.0
//           }
     val    mrt   = rl * (1 - 1.5 * temp2 * betal * con41) + 0.5 * temp1 * x1mth2 * cos2u
     val    su    = su0 - 0.25 * temp2 * x7thm1 * sin2u
     val    xnode = nodep + 1.5 * temp2 * cosip * sin2u
     val    xinc  = xincp + 1.5 * temp2 * cosip * sinip * cos2u
     val    mvt   = rdotl - nm * temp1 * x1mth2 * sin2u / KE
     val    rvdot = rvdotl + nm * temp1 * (x1mth2 * cos2u + 1.5 * con41) / KE
     
     val  el = TEME.SGPElems(nm, ep, xinc, argpp, xnode, mp, am, bStar, epoch) 
     ShortPeriodPeriodicState(el, xinc, su, xnode, mrt, mvt, rvdot, eaState)
  }

}

object ShortPeriodPeriodicPerturbations extends ShortPeriodPeriodicPerturbations

