package predict4s.tle.vallado

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._
import predict4s.tle.GeoPotentialCoefs
import predict4s.tle._
import TEME._   
import predict4s.tle.LaneCoefs

    
class SGP4Vallado[F : Field : NRoot : Order : Trig](
    elem0: TEME.SGPElems[F],
    wgs: SGPConstants[F],
    val geoPot: GeoPotentialCoefs[F],
    val gctx: GeoPotentialContext[F],
    val laneCoefs : LaneCoefs[F],
    val otherCoefs : OtherCoefs[F],
    val isImpacting: Boolean,
    val rp: F
  )  extends SGP4(elem0, wgs) {
   
  val eValidInterval = Interval.open(0.as[F],1.as[F])
   
  import elem0._, wgs._
 
  override def propagatePolarNodalAndContext(t: Minutes)
      : ((F,F,F,F,F,F), LongPeriodPeriodicState, TEME.SGPElems[F], EccentricAnomalyState) = {
    val secularElemt = secularCorrections(t)
    val lppState = lppCorrections(secularElemt)
    val eaState = solveKeplerEq(secularElemt, lppState)
    val (elem, finalPolarNodal) = sppCorrections(s, c, `c²`, eaState, lppState, secularElemt)
    (finalPolarNodal, lppState, secularElemt, eaState)   
  }
  
   
  /** 
   *  This calculation updates the secular elements at epoch to the desired date given by 
   *  the time t in minutes from the epoch 
   */
  def secularCorrections(t: Minutes): TEME.SGPElems[F] = {
    
    import otherCoefs.{ωdot,Ωdot,mdot=>Mdot,Ωcof}
    
    // Gravity corrections
    // Brouwer’s gravitational corrections are applied first
    // TBC: implementation here with Delaunays (or with Lydanne's ?) variables, 
    // ωdot is gdot, Mdot is ℓdot, and  Ωdot is hdot.
    val `t²` : F = t**2    
    val ωdf  : F = ω + ωdot*t
    val Ωdf  : F = Ω + Ωdot*t
    val Mdf  : F = M + Mdot*t
    
    // Next, the secular corrections due to the atmospheric drag are incorporated;
    // in particular δh, δL, δe, δℓ 
   
    val (tempa,tempe,templ,ωm,mp) : (F,F,F,F,F) = tempTerms(t, ωdf, Mdf)

    // Compute the secular elements (not exactly secular: they mix long-period terms from drag)

    val am : F  = ((KE/n) fpow (2.0/3.0).as[F]) * tempa*tempa // a * tempa**2  
    val nm : F  = KE / (am pow 1.5)
    val em_ : F = e - tempe
    val Ωm  : F = Ωdf + Ωcof*`t²` 
    
    // fix tolerance for error recognition
    // sgp4fix am is fixed from the previous nm check
    if (!eValidInterval.contains(em_))
      {
        // sgp4fix to return if there is an error in eccentricity
        // FIXME: we should move to use Either
        return TEME.SGPElems(nm, em_, I, ωm, Ωm, mp, am, bStar, epoch) 
      }

    // sgp4fix fix tolerance to avoid a divide by zero
    // TBC:  is this needed in Lara's version
    val em = if (em_ < 1.0e-6.as[F]) 1.0e-6.as[F] else em_ 
    
    val Mm_  = mp + n*templ
     
    // modulus so that the angles are in the range 0,2pi
    val Ω_      = Ωm  % twopi
    val ω_      = ωm  % twopi
    
    // Lyddane's variables and back 
    val ℓm      = Mm_ + ωm + Ωm
    val lm      = ℓm  % twopi
    val Mm      = (lm - ω_ - Ω_) % twopi   
    TEME.SGPElems(nm, em, I, ω_, Ω_, Mm, am, bStar, epoch)
  }

  def tempTerms(t: Minutes, ωdf: F, Mdf: F) : (F,F,F,F,F) = {

    import laneCoefs._
    import otherCoefs.{ωcof,delM0,sinM0,Mcof}    
    import geoPot._        
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
       
    (1 - C1*t - D2 * `t²` - D3 * `t³` - D4 * `t⁴`, 
     bStar*(C4*t + C5*(sin(Mpm_) - sinM0)), 
     t2cof*`t²` + t3cof*`t³` + `t⁴` * (t4cof + t*t5cof),
     ωm_, 
     Mpm_)
  }
  
  /**
   * Solve the Kepler equation 
   * 		ℓ = E - e sinE
   * where E is the eccentric anomaly.
   * TBC: We are using Delauney's elements as variables.
   */
  def solveKeplerEq(elem : TEME.SGPElems[F], lppState: LongPeriodPeriodicState): EccentricAnomalyState = {
       
    import elem.{e,Ω,ω,M,a}, wgs.twopi, lppState._
         
    /* --------------------- solve kepler's equation  M = E - e sin E     --------------- */
    // Nodep (or M) is the mean anomaly, E is the eccentric anomaly, and e is the eccentricity.
    var ktr : Int = 1
    val u    = Field[F].mod(xl - Ω, twopi.as[F])
    var eo1  = u
    var tem5 : F = 9999.9.as[F]     //   sgp4fix for kepler iteration
    var ecosE : F = 0.as[F]
    var esinE : F = 0.as[F]
    var coseo1 : F = 0.as[F]
    var sineo1 : F = 0.as[F]
     
    //   the following iteration needs better limits on corrections
    while ((abs(tem5) >= 1e-12.as[F]) && (ktr <= 10)) {
      sineo1 = sin(eo1)
      coseo1 = cos(eo1)
      ecosE = axnl * coseo1 + aynl * sineo1
      esinE = axnl * sineo1 - aynl * coseo1

      val fdot   = 1 - ecosE
      val f = (u + esinE - eo1)
      tem5   = f / fdot  // delta value
      if(abs(tem5) >= 0.95.as[F]) {
          tem5 = if (tem5 > 0.as[F]) 0.95.as[F]  else -0.95.as[F]
      }
       eo1    = eo1 + tem5
       ktr = ktr + 1
     }
     EccentricAnomalyState(eo1,coseo1,sineo1,ecosE,esinE)   
  }
  
  def lppCorrections(secularElem : TEME.SGPElems[F]) : LongPeriodPeriodicState = {
    import secularElem._
    import otherCoefs.{aycof,xlcof}
    val axnl = e * cos(ω)
    val temp = 1 / (a * (1 - e * e))
    
    // TBC: LPPE added     
    val aynl : F = e * sin(ω) + temp * aycof
    val xl: F = M + ω + Ω + temp * xlcof * axnl

    LongPeriodPeriodicState(axnl, aynl, xl)
  }
  
  def sppCorrections(s: SinI, c: CosI, `c²`: CosI, eaState: EccentricAnomalyState, lppState: LongPeriodPeriodicState, secularElem: TEME.SGPElems[F]) 
      : (TEME.SGPElems[F], (F,F,F,F,F,F)) = { // PolarNodalElems[F]) = {
    import eaState._ 
    import lppState._
    import secularElem._ // {n,e,I,ω,Ω,M,a}
 
    /* ------------- short period preliminary quantities ----------- */  
     // Compute polar variables
     // Change from Lyddane non-singular variables to polar-nodal variables
    
    val el2   = axnl*axnl + aynl*aynl
    val pl    = a*(1 - el2)                          // pl is C², where replacing C² by μa(1 − e²) gives simplified eq between dt and dr.
    if (pl < 0.as[F]) throw new Exception("pl: " + pl)

    // References done to the formulas given in Handbook of Satellite Orbits, Chapter 4
    // Consider the plane containing the position and velocity vectors of the satellite centered in the ellipse focus as our Earth)
    
    // It follows the usual transformation to polar-nodal variables
    // (r, θ, R, Θ) −→ (F, C, S, a)  with C' = e'cosg and  S' = e'sing
    // Note: Vallado's SGP4 uses rθdot = Θ/r instead of Θ
   
    val    rl     = a * (1 - ecosE)                  // 4.64, change of variable to E related to r, as in 4.63
    val    rdotl  = sqrt(a) * esinE/rl               // 4.67, simple manIpulations rdot (note missing √μ factor)
    val    rvdotl = sqrt(pl) / rl                     // ??? 4.68, r·rdot = √(μa)* esinE 
    val    betal  = sqrt(1 - el2)
    val    temp0  = esinE / (1 + betal)
     
    // ???? u is the true anomaly that can be defined immediately as the polar angle = (Ox, OS), x along the semimajor axis, S sat position
    val    sinu   = a / rl * (sineo1 - aynl - axnl * temp0)             // ??? 4.71,  y = r sinu = a * sqrt(1 − e2) * sinE
    val    cosu   = a / rl * (coseo1 - axnl + aynl * temp0)             // ??? 4.70,  x = r cosu = a(cosE − e)
    val    su0    = atan2(sinu, cosu)
    val    sin2u  = (cosu + cosu) * sinu
    val    cos2u  = 1 - 2.0 * sinu * sinu
    val    temp   = 1/ pl
    val    temp1  = 0.5 * J2 * temp
    val    temp2  = temp1 * temp

    /* -------------- update for short period gravitational periodics ------------ */

    import otherCoefs._
    val    mrt   = rl * (1 - 1.5 * temp2 * betal * con41) + 0.5 * temp1 * x1mth2 * cos2u
    val    su    = su0 - 0.25 * temp2 * x7thm1 * sin2u
    val    xnode = Ω + 1.5 * temp2 * c * sin2u
    val    xinc  = I + 1.5 * temp2 * c * s * cos2u
    val    mvt   = rdotl - n * temp1 * x1mth2 * sin2u / KE
    val    rvdot = rvdotl + n * temp1 * (x1mth2 * cos2u + 1.5 * con41) / KE  
    val  elem = TEME.SGPElems(n, e, xinc, ω, xnode, M, a, bStar, epoch) 
    val polarNodalXX = (xinc, su, xnode, mrt, mvt, rvdot)
    (elem, polarNodalXX)
  }

}

object SGP4Vallado extends SGP4Factory {
  
  def apply[F : Field : NRoot : Order : Trig](elemTLE: TEME.SGPElems[F])(implicit wgs0: SGPConstants[F]) :  SGP4Vallado[F] = {
    val (elem, wgs, geoPot, gctx, laneCoefs, otherCoefs, isImpacting, rp) = from(elemTLE)
    new SGP4Vallado(elem, wgs, geoPot, gctx, laneCoefs, otherCoefs, isImpacting, rp)
  }
  
}
