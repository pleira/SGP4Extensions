package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._

class SGP4Factory extends GeoPotentialModel { 
  
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
    val secularFreqs = new SecularFrequencies(elem0, context0, geoPot, gctx)
    (elem0, wgs, geoPot, gctx, laneCoefs, secularFreqs, isImpacting, rp)
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
