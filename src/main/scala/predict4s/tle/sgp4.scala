package predict4s.tle
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.tle.TEME._ 

/** 
 * The SGP-4 theory is applied for all orbits with periods of T <= 225 min. 
 * It performs a propagation in time of doubly averaged elements according to their
 * secular rates of change due to the zonal harmonics J2 and J4 of the Earth potential,
 * and due to drag perturbations in an atmosphere with a power-law altitude profile of air density. 
 * The propagated, doubly averaged elements at epoch are subsequently
 * converted into singly averaged elements, by overlaying long-periodic
 * perturbations due to J3, before a final conversion step to osculating elements by superimposition
 * of first-order, short-period perturbation amplitudes due to J2. 
 * (from Space Debris, by H. Klinkrad, pag 216).
 */
abstract class SGP4[F : Field : NRoot : Order : Trig](
    val elem0: SGPElems[F],    
    val wgs: SGPConstants[F],
    val ctx0: Context0[F],
    val geoPot: GeoPotentialCoefs[F],
    val gctx: GeoPotentialContext[F],
    val laneCoefs : LaneCoefs[F],
    val secularTerms : (SecularFrequencies[F], DragSecularCoefs[F]),
    val isImpacting: Boolean,
    val rp: F
    ) {

  // valid interval for eccentricity calculations
  val eValidInterval = Interval.open(0.as[F],1.as[F])

  def gsto : F = predict4s.tle.gstime(elem0.epoch + 2433281.5) 
  
  type SinI = F  // type to remember dealing with the sine   of the Inclination 
  type CosI = F  // type to remember dealing with the cosine of the Inclination 
  type Minutes = F // type to remember dealing with minutes from epoch

  type FinalState 
  type ShortPeriodState
  type LongPeriodState
  type EccentricAState
  
  def propagate2PolarNodalContext(t: Minutes): ((FinalState, ShortPeriodState, LongPeriodState, EccentricAState), SGPElems[F]) = {
    val secularElemt = secularCorrections(t)
    (periodicCorrections(secularElemt, secularTerms._2), secularElemt)
  }
  
  def propagate2PolarNodal(t: Minutes) = {
    val ((finalPolarNodal, _, _, _), _) = propagate2PolarNodalContext(t)
    finalPolarNodal
  }

  def propagate2CartesianContext(t: Minutes) : 
    (TEME.CartesianElems[F], TEME.CartesianElems[F], FinalState, ShortPeriodState, LongPeriodState, EccentricAState)

  def propagate2Cartesian(t: Minutes) : TEME.CartesianElems[F] = {  
    val (posVel, _, _,_,_,_) = propagate2CartesianContext(t)
    posVel
  }
  
  def propagate(t: Minutes)  = propagate2CartesianContext(t)

  
  def periodicCorrections(secularElemt : SGPElems[F], secularDragCoefs: DragSecularCoefs[F]): 
    (FinalState, ShortPeriodState, LongPeriodState, EccentricAState)

  case class EccentricAnomalyState(eo1 : F, coseo1: F, sineo1: F, ecosE: F, esinE: F)  


  /**
   * Vallado's code works with internal units of length LU (units of earth’s radius  
   * R⊕ in km) and time TU (units of the orbit’s period in min) 
   * TU = 60 * sqrt( (R⊕ km)³ /(μ km³ /s²) ) min
   * where μ is the earth’s gravitational constant; μ = 1 UL³/UT² in internal units.    
   */
  def convertAndScale2UnitVectors(pos : Vector[F], vel : Vector[F], mrt: F, mvt: F, rvdot: F): (Vector[F], Vector[F]) = {
      import wgs.{aE,vkmpersec}
      ( (aE*mrt) *: pos,  vkmpersec *: (mvt *: pos + rvdot *: vel))
  }  

  /** 
   *  This calculation updates the secular elements at epoch to the desired date given by 
   *  the time t in minutes from the epoch 
   */
  def secularCorrections(t: Minutes): SGPElems[F] = {
    
    import secularTerms._1._ // secularFrequencies._  // {ωdot,Ωdot,mdot=>Mdot,Ωcof}
    import secularTerms._2._ // dragSecularCoefs._  
    import elem0._, wgs._
 
    // Brouwer’s gravitational corrections are applied first
    // Note that his theory relies on Delaunays variables, 
    // ωdot is gdot, Mdot is ℓdot, and  Ωdot is hdot.
    val `t²` : F = t**2    
    val ωdf  : F = ω + ωdot*t
    val Ωdf  : F = Ω + Ωdot*t
    val Mdf  : F = M + Mdot*t    
    
    // Next, the secular corrections due to the atmospheric drag are incorporated
    // which also take long period terms from drag;
    // in particular δh, δL, δe, δℓ 
   
    val (δL, δe, δℓ, ωm, mp) : (F,F,F,F,F) = dragSecularCorrections(t, ωdf, Mdf)

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
    // TBC:  is this needed in Lara's version
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
    import secularTerms._2._ // dragSecularCoefs._ // {ωcof,delM0,sinM0,Mcof}    
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



