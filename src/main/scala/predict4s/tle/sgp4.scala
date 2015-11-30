package predict4s.tle
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._

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
    val wgs: SGPConstants[F]
    ){
  
  type SinI = F  // type to remember dealing with the sine   of the Inclination 
  type CosI = F  // type to remember dealing with the cosine of the Inclination 
  type Minutes = F // type to remember dealing with minutes from epoch
 
  
  def propagatePolarNodalAndContext(t: Minutes) : ((F,F,F,F,F,F), LongPeriodPeriodicState, TEME.SGPElems[F], EccentricAnomalyState)
  
  def propagatePolarNodal(t: Minutes) = {
    val (finalPolarNodal, _, _, _) = propagatePolarNodalAndContext(t)
    finalPolarNodal
  }

  def propagate2CartesianAndContext(t: Minutes) = {
    val (finalPolarNodalXX, lppState, secularElemt, eaState) = propagatePolarNodalAndContext(t)
    import finalPolarNodalXX.{_1=>I,_2=>R,_3=> Ω, _4=>mrt,_5=>mvt,_6=>rvdot}
    val uPV: TEME.CartesianElems[F] = TEME.polarNodal2UnitCartesian(I, R, Ω)
    val (p, v) = convertUnitVectors(uPV.pos, uPV.vel, mrt, mvt, rvdot)
    val posVel = TEME.CartesianElems(p(0),p(1),p(2),v(0),v(1),v(2))
    (posVel, uPV, finalPolarNodalXX, lppState, secularElemt, eaState)    
  }

  def propagate2Cartesian(t: Minutes) : TEME.CartesianElems[F] = {  
    val (posVel, _, _,_,_,_) = propagate2CartesianAndContext(t)
    posVel
  }
  
  def propagate(t: Minutes)  = propagate2CartesianAndContext(t)

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
    
}



trait SGP4Factory extends GeoPotentialModel { 
  
  /**
   * Recover original mean motion (n0'', n0dp) and semimajor axis (a0'' , a0dp).
   * 
   */
  def originalElems[F: Field: NRoot : Order: Trig](elemTLE : TEME.SGPElems[F])(implicit wgs: SGPConstants[F]) : TEME.SGPElems[F]= 
    calcOriginalElems(elemTLE, Context0(elemTLE))
  
  /**
   * Recover original mean motion (n0'', n0dp) and semimajor axis (a0'' , a0dp) 
   * and some context data to avoid repeating calculations.
   * 
   */
  def originalElemsAndContext[F: Field: NRoot : Order: Trig](elemTLE : TEME.SGPElems[F])(implicit wgs: SGPConstants[F])
    : (TEME.SGPElems[F], Context0[F]) = {
    val context0 = Context0(elemTLE)
    val elem0 = calcOriginalElems(elemTLE, context0)
    (elem0, context0)
  }
  
  /**
   * Recover original mean motion (n0'', n0dp) and semimajor axis (a0'' , a0dp).
   * 
   */
  private def calcOriginalElems[F: Field: NRoot : Order: Trig](elemTLE : TEME.SGPElems[F], context0: Context0[F]) = {
    import elemTLE._,context0._ 
    import wgs.{KE,J2}
    
    val a1 : F   = (KE / n) fpow (2.0/3.0).as[F]  // (Ke / n0) pow 1.5   
    val tval : F = (3.0/4.0) * J2 * x3thm1 / β0to3  // 3 * k2 * (3*`cos²I0` - 1) / ((1-`e0²`) pow 1.5) / 4 
    val δ1   = tval / (a1*a1)
    val a0   = a1 * (1 - δ1 * (1.0/3.0 + δ1 * (1 + 134 * δ1 / 81)))
    val δ0   = tval / (a0 * a0)  
    val n0dp = n   / (1 + δ0) 
    val a0dp = (KE / n0dp) fpow (2.0/3.0).as[F]  // a0   / (1 - δ0)
    TEME.SGPElems(n0dp, e, I, ω, Ω, M, a0dp, bStar, epoch)
  }
  
  /**
   * Factory method to produce all inputs needed to create a SGP4 propagator.
   * Here we are starting with a SGP Elements directly obtained from a TLE. 
   */
  def from[F : Field : NRoot : Order : Trig](elemTLE: TEME.SGPElems[F])(implicit wgs: SGPConstants[F]) = {
    val (elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting) = geoPotentialCoefsAndContexts(elemTLE)    
    val laneCoefs = LaneCoefs(geoPot)
    val otherCofs = OtherCoefs(elem0, context0, geoPot, gctx)
    (elem0, wgs, geoPot, laneCoefs, otherCofs, isImpacting)
  }

  
  def calcGeoPotentialCoefs[F : Field : NRoot : Order : Trig](elemTLE: TEME.SGPElems[F])(implicit wgs: SGPConstants[F]) 
    : GeoPotentialCoefs[F] = {
    val (_,_,geoPot,_,_,_,_) = geoPotentialCoefsAndContexts(elemTLE)
    geoPot
  }
  
  def geoPotentialCoefsAndContexts[F : Field : NRoot : Order : Trig](elemTLE: TEME.SGPElems[F])(implicit wgs: SGPConstants[F]) 
    : (TEME.SGPElems[F], Context0[F], GeoPotentialCoefs[F], GeoPotentialContext[F], F, F, Boolean) = {
    val (elem0, context0) = originalElemsAndContext(elemTLE)
    import elem0.{a,e},wgs.aE
    
    // radius of perigee
    val rp : F = a*(1-e)
    assert (rp > 1)
      
    // perigee height, altitude relative to the earth's surface, so perige instead of perigee 
    val perigeeHeight =  (rp - 1) * aE
    val isImpacting : Boolean    = rp < (220/aE + 1) 
    val s = fittingAtmosphericParameter(perigeeHeight, aE)
    val gctx = GeoPotentialContext(elem0, s, rp, aE)

    val geoPot  = geoPotentialCoefs(elem0, context0, gctx, aE)
    (elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting)
  }  
  
}

//case class OrbitalState[F](t: F, posVel: TEME.CartesianElems[F])

case class GeoPotentialCoefs[F](C1: F, C2: F, C3: F, C4: F, C5: F, D2: F, D3: F, D4: F)

//case class EccentricAnomalyState[F](eo1 : F, coseo1: F, sineo1: F, ecosE: F, esinE: F, lppState: LongPeriodPeriodicState[F])  
//
//case class SGP4State[F](orbitalState: OrbitalState[F], uPV: TEME.CartesianElems[F], elem: TEME.SGPElems[F], 
//    sppState : ShortPeriodPeriodicState[F], wgs: SGPConstants[F])

// case class LongPeriodPeriodicState[F](axnl: F, aynl: F, xl: F, secularState: SecularState[F])

//case class ShortPeriodPeriodicState[F](
//    elem: TEME.SGPElems[F], 
//    I: F,     // inclination 
//    R: F,     // Radial velocity    
//    Ω: F,     // argument of the node
//    mrt: F, 
//    mvt: F, 
//    rvdot: F) 
//    eaState: EccentricAnomalyState[F])
    

//
//trait PotentialCoeficients[F] {
//  def C1: F;  def C2: F;  def C3: F;  def C4: F;  def C5: F
//  def D2: F;  def D3: F;  def D4: F
//}

