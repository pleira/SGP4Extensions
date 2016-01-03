package predict4s.sgp

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.coord.SGPElems
import predict4s.coord.Context0
import predict4s.coord.SGPConstants


class Factory2ndOrderSecularCorrectionsTerms[F : Field : NRoot : Order : Trig](wgs: SGPConstants[F]) extends GeoPotentialAndAtmosphere2ndOrderModel with FittingAtmosphericParameter[F] { 
  
  val aE = wgs.aE
  
  /**
   * Factory method to produce all inputs needed to create the propagator for SecularCorrections.
   * Here we are starting with a SGP Elements directly obtained from a TLE. 
   */
  def from(elem0Ctx0: (SGPElems[F], Context0[F])) = {
    val (elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting) = geoPotentialCoefsAndContexts(elem0Ctx0)    
    val laneCoefs = calcLaneCoefs(geoPot)
    val (secularFreqs, dragCoefs) = calcSecularFrequenciesAndCoefs(elem0, context0, geoPot, gctx)
    (elem0, wgs, context0, geoPot, gctx, laneCoefs, secularFreqs, dragCoefs, isImpacting, rp)
  }
  
  /**
   * Factory method to produce all inputs needed to create the propagator for SecularCorrections.
   * Here we are starting with a SGP Elements directly obtained from a TLE. 
   */
  def calcSecularTerms(elem0Ctx0: (SGPElems[F], Context0[F])) = {
    val (elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting) = geoPotentialCoefsAndContexts(elem0Ctx0)    
    val laneCoefs = calcLaneCoefs(geoPot)
    val (secularFreqs, dragCoefs) = calcSecularFrequenciesAndCoefs(elem0, context0, geoPot, gctx)
    (elem0, wgs, geoPot, gctx, laneCoefs, secularFreqs, dragCoefs, isImpacting, rp)
  }
  
  def geoPotentialCoefsAndContexts(elem0Ctx0: (SGPElems[F], Context0[F])) 
      : (SGPElems[F], Context0[F], GeoPotentialCoefs[F], GeoPotentialContext[F], F, F, Boolean) = {
    //val (elem0, context0) = originalElemsAndContext(elemTLE)
    import elem0Ctx0.{_1 => elem0, _2 => context0}
    import elem0.{a,e}
    
    // radius of perigee
    val rp : F = a*(1-e)
    assert (rp > 1)
      
    // perigee height, altitude relative to the earth's surface, so perige instead of perigee 
    val perigeeHeight =  (rp - 1) * aE
    val isImpacting : Boolean = rp < (220/aE + 1) 
    val s = fittingAtmosphericParameter(perigeeHeight)
    val gctx = GeoPotentialContext(elem0, s, rp, aE)

    val geoPot  = geoPotentialCoefs(elem0, context0, gctx, wgs)
    (elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting)
  }  
  
  def calcSecularFrequencies(elem: SGPElems[F], ctx0: Context0[F], gcof : GeoPotentialCoefs[F], gctx : GeoPotentialContext[F])
      : (SecularFrequencies[F], F) = {
  
    import gcof._,gctx._,ctx0._
    import wgs._
    import elem.{e => e0,n => n0,a => a0,ω => ω0, M => M0,bStar}
    
    val ϵ2 = - J2/`p²`/ 4  // note missing aE² as in Lara's
    val temp3 = -0.46875 * J4 * n0 / `p⁴`
    val `3n0ϵ2` = 3*ϵ2*n0
    val hdot =  2*`3n0ϵ2`*θ 
    val secularFrequencies = 
      if (n0 >= 0.as[F] || `β0²` >= 0.as[F])
        SecularFrequencies(// derivative of M 
          n0 - `3n0ϵ2` * β0 * (`3c²-1` - ϵ2 * (13 - 78 * `θ²` + 137 * `θ⁴`) / 4),
        // derivative of the perigee argument
        `3n0ϵ2` * (`1-5c²` + ϵ2*(7 - 114*`θ²` + 395*`θ⁴`)/4) + temp3*(3 - 36*`θ²` + 49*`θ⁴`),
        // derivative of the raan
        //(2* `3n0ϵ2` * (1 + (ϵ2*(4 - 19*`θ²`)) + 2*temp3 * (3 - 7*`θ²`)))*θ
        hdot + ϵ2*hdot* (4 - 19*`θ²`) + 2*temp3*θ* (3 - 7*`θ²`)
        )
      else 
        SecularFrequencies(0.as[F],0.as[F],0.as[F])
        
    (secularFrequencies, hdot)
  }
  
  def calcSecularDragCoefs(hdot: F, elem: SGPElems[F], ctx0: Context0[F], gcof : GeoPotentialCoefs[F], gctx : GeoPotentialContext[F])
      : DragSecularCoefs[F] = {
    
    import gcof._,gctx._,ctx0._
    import wgs._
    import elem.{e => e0,n => n0,a => a0,ω => ω0, M => M0,bStar}
     // sgp4fix for divide by zero with I = 180 deg, // FIXME: not valid for deep space
    val xlcof  : F  =  
      if (abs(θ+1) > 1.5e-12.as[F]) 
        - `J3/J2` * sinI0 * (3 + 5*θ) / (1 + θ) / 4
      else
        - `J3/J2` * sinI0 * (3 + 5*θ) / 1.5e-12 / 4
    
    // other derived coeficients and variables that are used related to drag corrections
    DragSecularCoefs(    
      if (e0 > 0.0001.as[F]) - 2*`ξ⁴(q0-s)⁴` * bStar / e0η / 3 else 0.as[F],
      bStar*C3*cos(ω0),
      7 * `β0²` * hdot * C1 / 2,
      xlcof,
       - `J3/J2` * sinI0 / 2,
      (1+η*cos(M0))**3)    
  }
  
  def calcSecularFrequenciesAndCoefs(elem: SGPElems[F], ctx0: Context0[F], gcof : GeoPotentialCoefs[F], gctx : GeoPotentialContext[F])
      : (SecularFrequencies[F], DragSecularCoefs[F]) = {
    val (secularFrequencies, hdot) = calcSecularFrequencies(elem, ctx0, gcof, gctx)
    val dragSecularCoefs = calcSecularDragCoefs(hdot, elem, ctx0, gcof, gctx)
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
}
