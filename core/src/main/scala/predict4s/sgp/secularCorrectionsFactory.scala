package predict4s.sgp

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.coord._

object Factory2ndOrderSecularCorrectionsTerms extends GeoPotentialAndAtmosphere2ndOrderModel { 
  
  /**
   * Factory method to produce all inputs needed to create the propagator for SecularCorrections.
   * Here we are starting with a SGP Elements directly obtained from a TLE. 
   */
  def from[F : Field : NRoot : Order : Trig](elem0Ctx: SGPElemsCtx[F]) = {
    import elem0Ctx.{elem,iCtx,eCtx,wgs} 
    val (geoPot, gctx) = geoPotentialCoefsAndContexts(elem0Ctx)    
    val laneCoefs = calcLaneCoefs(geoPot)
    val (secularFreqs, dragCoefs) = calcSecularFrequenciesAndCoefs(elem0Ctx, geoPot, gctx)
    (geoPot, gctx, laneCoefs, secularFreqs, dragCoefs)
  }
  
  /**
   * Factory method to produce all inputs needed to create the propagator for SecularCorrections.
   * Here we are starting with a SGP Elements directly obtained from a TLE. 
   */
  def calcSecularTerms[F : Field : NRoot : Order : Trig](elem0Ctx: SGPElemsCtx[F]) = {
    import elem0Ctx.{elem,iCtx,eCtx,wgs} 
    val (geoPot, gctx) = geoPotentialCoefsAndContexts(elem0Ctx)    
    val laneCoefs = calcLaneCoefs(geoPot)
    val (secularFreqs, dragCoefs) = calcSecularFrequenciesAndCoefs(elem0Ctx, geoPot, gctx)
    (elem0Ctx, geoPot, gctx, laneCoefs, secularFreqs, dragCoefs)
  }
  
  def geoPotentialCoefsAndContexts[F : Field : NRoot : Order : Trig](elem0Ctx: SGPElemsCtx[F]) 
      : (GeoPotentialCoefs[F], GeoPotentialContext[F]) = {
    import elem0Ctx.{elem,iCtx,eCtx,wgs,perigeeHeight} 
    import elem.{a,e},wgs.aE
    
    val s = fittingAtmosphericParameter(perigeeHeight, aE)
    val gctx = GeoPotentialContext(elem, s, aE)

    val geoPot  = geoPotentialCoefs(elem0Ctx, gctx)
    (geoPot, gctx)
  }  
  
  def calcSecularFrequencies[F : Field : NRoot : Order : Trig](elem0Ctx: SGPElemsCtx[F], gcof : GeoPotentialCoefs[F], gctx : GeoPotentialContext[F])
      : (SecularFrequencies[F], F) = {
    import elem0Ctx.{elem,iCtx,eCtx,wgs} 
    import gcof._,gctx._
    import wgs._,iCtx._,eCtx._
    import elem.{e => e0,n => n0,a => a0,ω => ω0, M => M0,bStar}

    val p = a0 * `β0²` // a0 * (1 - `e²`) // semilatus rectum , which also is G²/μ, with G as the Delauney's action, the total angular momentum
    val `p²` = p*p
    val `p⁴` = `p²`*`p²`
    
    val ϵ2 = - J2/`p²`/ 4  // note missing aE² as in Lara's
    val temp3 = -0.46875 * J4 * n0 / `p⁴`
    val `3n0ϵ2` = 3*ϵ2*n0
    val hdot =  2*`3n0ϵ2`*c 
    val secularFrequencies = 
      if (n0 >= 0.as[F] || `β0²` >= 0.as[F])
        SecularFrequencies(// derivative of M 
          n0 - `3n0ϵ2` * β0 * (`3c²-1` - ϵ2 * (13 - 78 * `c²` + 137 * `c⁴`) / 4),
        // derivative of the perigee argument
        `3n0ϵ2` * (1 - 5*`c²` + ϵ2*(7 - 114*`c²` + 395*`c⁴`)/4) + temp3*(3 - 36*`c²` + 49*`c⁴`),
        // derivative of the raan
        //(2* `3n0ϵ2` * (1 + (ϵ2*(4 - 19*`c²`)) + 2*temp3 * (3 - 7*`c²`)))*c
        hdot + ϵ2*hdot* (4 - 19*`c²`) + 2*temp3*c* (3 - 7*`c²`)
        )
      else 
        SecularFrequencies(0.as[F],0.as[F],0.as[F])
        
    (secularFrequencies, hdot)
  }
  
  def calcSecularDragCoefs[F : Field : NRoot : Order : Trig](hdot: F, elem0Ctx: SGPElemsCtx[F], gcof : GeoPotentialCoefs[F], gctx : GeoPotentialContext[F])
      : DragSecularCoefs[F] = {
    
    import gcof._,gctx._
    import elem0Ctx.{elem,iCtx,eCtx,wgs} 
    import wgs._,iCtx._,eCtx._
    import elem.{e => e0,n => n0,a => a0,ω => ω0, bStar}
    
    // other derived coeficients and variables that are used related to drag corrections
    DragSecularCoefs(    
      if (e0 > 0.0001.as[F]) - 2*`ξ⁴(q0-s)⁴` * bStar / e0η / 3 else 0.as[F],
      bStar*C3*cos(ω0),
      7 * `β0²` * hdot * C1 / 2 )    
  }
  
  def calcSecularFrequenciesAndCoefs[F : Field : NRoot : Order : Trig](elem0Ctx: SGPElemsCtx[F], gcof : GeoPotentialCoefs[F], gctx : GeoPotentialContext[F])
      : (SecularFrequencies[F], DragSecularCoefs[F]) = {
    val (secularFrequencies, hdot) = calcSecularFrequencies(elem0Ctx, gcof, gctx)
    val dragSecularCoefs = calcSecularDragCoefs(hdot, elem0Ctx, gcof, gctx)
    (secularFrequencies, dragSecularCoefs)
  }
  
  /**
   * The initialization process provides a series of coefficients needed
   * to apply drag secular corrections as computed from Lane’s theory.
   */
  def calcLaneCoefs[F : Field](gcof : GeoPotentialCoefs[F]) : LaneCoefs[F] = {
    import gcof._
    val `C1²` = C1*C1
    LaneCoefs(
         3*C1/2, 
         D2 + 2*`C1²`, 
         (3*D3 + C1*(12*D2 + 10 * `C1²`))/4,  
         (3*D4 + 12*C1*D3 + 6*D2*D2 + 15*`C1²`*(2*D2+`C1²`))/5)
  }
   
  def fittingAtmosphericParameter[F: Field : Order](perigeeHeight: F, aE: F) : F = {
    def S_above156 : F = 1 + 78/aE
    // def hs(perigeeHeight: F)(implicit ev: Field[F]) : F =  perigeeHeight - 78   // interpolation, being a number bigger than 20, and smaller that 78
    def S_between_98_156 : F =  (1 + (perigeeHeight - 78)/aE)
    def S_below98: F =  (1 + 20/aE)
       if (perigeeHeight >= 156)       S_above156
       else if (perigeeHeight >= 98)   S_between_98_156
       else                            S_below98
  }
   
}

