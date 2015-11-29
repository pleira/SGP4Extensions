package predict4s.tle.vallado

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
import predict4s._
import predict4s.tle.GeoPotentialCoefs
import predict4s.tle.OrbitalState
import predict4s.tle._
import TEME._   
import predict4s.tle.DpTransform
import predict4s.tle.LaneCoefs
import predict4s.tle.GeoPotentialState

    
class SGP4Vallado[F : Field : NRoot : Order : Trig](
    val elem0: TEME.SGPElems[F],
    val wgs: SGPConstants[F],
    val geoPot: GeoPotentialState[F],
    val laneCoefs : LaneCoefs[F],
    val otherCoefs : OtherCoefs[F]
  )  {
  
  type SinI = F  // type to remember dealing with the sine   of the Inclination 
  type CosI = F  // type to remember dealing with the cosine of the Inclination 
  type Minutes = F // type to remember dealing with minutes from epoch
 
  val eValidInterval = Interval.open(0.as[F],1.as[F])
   
  import elem0._, wgs._
  val `e²` : F = e**2
  val s : SinI = sin(I)
  val c : CosI = cos(I)
  val `c²` : CosI = c**2
  val `s²` : SinI = s**2
  val p : F = a * (1 - `e²`)            // semilatus rectum , which also is G²/μ, with G as the Delauney's action, the total angular momentum
  val `α/p` : F = α/p
  val ϵ2 : F = -J2*(`α/p`**2) / 4
  val ϵ3 : F = (`J2/J3`)*`α/p` / 2      // or (`C30/C20`)*`α/p` / 2 
  val η : F = (1 - `e²`).sqrt           // eccentricity function G/L, with G as the Delauney's action, the total angular momentum , and L = √(μ a)
  val x3thm1     = 3*`c²` - 1
  val con41      = x3thm1
  val con42      = 1 - 5*`c²`
  val x1mth2     = 1 - `c²`

    
  def propagate(t: Minutes) : SGP4State[F] = {
    val secularElemt = secularCorrections(t)
    val lppState = lppCorrections(secularElemt)
    val eaState = solveKeplerEq(secularElemt, lppState)
    val finalPolarNodal = sppCorrections(s, c, `c²`, eaState, lppState, secularElemt)
      
    // unit position and velocity 
    import finalPolarNodal._
    val uPV: TEME.CartesianElems[F] = TEME.polarNodal2UnitCartesian(I, R, Ω)
    
    // return position and velocity (in km and km/sec)
    val (p, v) = convertUnitVectors(uPV.pos, uPV.vel, mrt, mvt, rvdot)
    val posVel = TEME.CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
    val orbitalState = OrbitalState(t, posVel)
    SGP4State(orbitalState, uPV)
  }

  /**
   * Vallado's code works with internal units of length LU (units of earth’s radius  
   * R⊕ in km) and time TU (units of the orbit’s period in min) 
   * TU = 60 * sqrt( (R⊕ km)³ /(μ km³ /s² ) ) min
   * where μ is the earth’s gravitational constant; μ = 1 UL³/UT² in internal units.    
   */
  def convertUnitVectors(pos : Vector[F], vel : Vector[F], mrt: F, mvt: F, rvdot: F)
      : (Vector[F], Vector[F]) = {
      import wgs._
      ( (aE*mrt) *: pos,  vkmpersec *: (mvt *: pos + rvdot *: vel))
  }  


  case class LongPeriodPeriodicState(axnl: F, aynl: F, xl: F)
  case class EccentricAnomalyState(eo1 : F, coseo1: F, sineo1: F, ecosE: F, esinE: F)  
  case class ShortPeriodPeriodicState(
    elem: TEME.SGPElems[F], 
    I: F,     // inclination 
    R: F,     // Radial velocity    
    Ω: F,     // argument of the node
    mrt: F, 
    mvt: F, 
    rvdot: F)
    
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

    import geoPot.dps.isImpacting
    import laneCoefs._
    import otherCoefs.{ωcof,delM0,sinM0,Mcof}    
    import geoPot.gcof._        
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
  
  def sppCorrections(s: SinI, c: CosI, `c²`: CosI, eaState: EccentricAnomalyState, lppState: LongPeriodPeriodicState, secularElem: TEME.SGPElems[F]) : ShortPeriodPeriodicState = {
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
    ShortPeriodPeriodicState(elem, xinc, su, xnode, mrt, mvt, rvdot)
  }
    
}

object SGP4Vallado {
  
  /**
   * Here we are starting with a SGP Elements directly obtained from a TLE. 
   * The method returns a Vallado SGP4 Propagator where some secular related coeficients are already calculated.
   */
  def apply[F : Field : NRoot : Order : Trig](elem0: TEME.SGPElems[F])(implicit wgs: SGPConstants[F]) : SGP4Vallado[F] = {
    val dpState = DpTransform.dpState(elem0)
    val geoPot  = GeoPotentialState(dpState)
    val laneCoefs = LaneCoefs(geoPot)
    val otherCofs = OtherCoefs(geoPot)
    new SGP4Vallado(dpState.elem, wgs, geoPot, laneCoefs, otherCofs)
  }
  // FIXME
//  def dpState[F: Field: Trig](tle: TLE)(implicit wgs: SGPConstants[F]) :  DpTransform.DpState[F] = 
//    DpTransform.dpState(TEME.sgpElems(tle))
//
//  def geoState[F: Field: Trig](tle: TLE)(implicit wgs: SGPConstants[F]) : GeoPotentialState[F] =
//    GeoPotentialState(dpState(tle))
  
}

case class SGP4State[F](orbitalState: OrbitalState[F], uPV: TEME.CartesianElems[F]) 
