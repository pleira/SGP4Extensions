package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._


class SGP4Factory extends GeoPotentialModel { 
  
  /**
   * Factory method to produce all inputs needed to create a SGP4 propagator.
   * Here we are starting with a SGP Elements directly obtained from a TLE. 
   */
  def from[F : Field : NRoot : Order : Trig](elem0Ctx0: (SGPElems[F], Context0[F]))(implicit wgs: SGPConstants[F]) = {
    val (elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting) = geoPotentialCoefsAndContexts(elem0Ctx0, wgs)    
    val laneCoefs = calcLaneCoefs(geoPot)
    val secularFreqs = calcSecularFrequenciesAndCoefs(elem0, context0, geoPot, gctx, wgs)
    (elem0, wgs, geoPot, gctx, laneCoefs, secularFreqs, isImpacting, rp)
  }
  
  /**
   * Factory method to produce all inputs needed to create a SGP4 propagator.
   * Here we are starting with a SGP Elements directly obtained from a TLE. 
   */
  def calcSecularTerms[F : Field : NRoot : Order : Trig](elem0Ctx0: (SGPElems[F], Context0[F]))(implicit wgs: SGPConstants[F]) = {
    val (elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting) = geoPotentialCoefsAndContexts(elem0Ctx0, wgs)    
    val laneCoefs = calcLaneCoefs(geoPot)
    val secularFreqs = calcSecularFrequenciesAndCoefs(elem0, context0, geoPot, gctx, wgs)
    (elem0, wgs, geoPot, gctx, laneCoefs, secularFreqs, isImpacting, rp)
  }
  
//  def calcGeoPotentialCoefs[F : Field : NRoot : Order : Trig](elemTLE: SGPElems[F], wgs: SGPConstants[F]) 
//      : GeoPotentialCoefs[F] = {
//    val (_,_,geoPot,_,_,_,_) = geoPotentialCoefsAndContexts(elemTLE)
//    geoPot
//  }
  
  def geoPotentialCoefsAndContexts[F : Field : NRoot : Order : Trig](elem0Ctx0: (SGPElems[F], Context0[F]), wgs: SGPConstants[F]) 
      : (SGPElems[F], Context0[F], GeoPotentialCoefs[F], GeoPotentialContext[F], F, F, Boolean) = {
    //val (elem0, context0) = originalElemsAndContext(elemTLE)
    import elem0Ctx0.{_1 => elem0, _2 => context0}
    import elem0.{a,e},wgs.aE
    
    // radius of perigee
    val rp : F = a*(1-e)
    assert (rp > 1)
      
    // perigee height, altitude relative to the earth's surface, so perige instead of perigee 
    val perigeeHeight =  (rp - 1) * aE
    val isImpacting : Boolean    = rp < (220/aE + 1) 
    val s = fittingAtmosphericParameter(perigeeHeight, aE)
    val gctx = GeoPotentialContext(elem0, s, rp, aE)

    val geoPot  = geoPotentialCoefs(elem0, context0, gctx, aE, wgs)
    (elem0, context0, geoPot, gctx, rp, perigeeHeight, isImpacting)
  }  

  def calcSecularFrequenciesAndCoefs[F : Field : NRoot : Order : Trig] (elem: SGPElems[F], ctx0: Context0[F], gcof : GeoPotentialCoefs[F], gctx : GeoPotentialContext[F], wgs: SGPConstants[F])
      : (SecularFrequencies[F], DragSecularCoefs[F]) = {
  
    import gcof._,gctx._,ctx0._
    import wgs._
    import elem.{e => e0,n => n0,a => a0,ω => ω0, M => M0,_}
    
    val gsto : F = predict4s.tle.gstime(epoch + 2433281.5) 
    
    val po   : F   = a0*`β0²`
    val posq : F   = po*po
    val pinvsq : F  = 1 / posq
    val temp1  : F  = 3 * J2 * pinvsq * n0 / 2
    val temp2 : F   = temp1 * J2 * pinvsq / 2
    val temp3  : F  = -0.46875 * J4 * pinvsq * pinvsq * n0
    val xhdot1 : F  = - temp1*θ 
     
    val secularFrequencies = 
      if (n0 >= 0.as[F] || `β0²` >= 0.as[F])
        SecularFrequencies(// derivative of M 
          n0 + 0.5.as[F] * temp1 * β0 * con41 + 0.0625.as[F] * temp2 * β0 * (13 - 78 * `θ²` + 137 * `θ⁴`),
        // derivative of the perigee argument
        - temp1 * con42 /2 + temp2*(7 - 114*`θ²` + 395*`θ⁴`)/16 + temp3*(3 - 36*`θ²` + 49*`θ⁴`),
        // derivative of the raan
        xhdot1 + (temp2 * (4 - 19*`θ²`)/2 + 2*temp3 * (3 - 7*`θ²`))*θ
        )
      else 
        SecularFrequencies(0.as[F],0.as[F],0.as[F])  
     
    // sgp4fix for divide by zero with I = 180 deg, // FIXME: not valid for deep space
    val xlcof  : F  =  
      if (abs(θ+1) > 1.5e-12.as[F]) 
        - `J3/J2` * sinio * (3 + 5*θ) / (1 + θ) / 4
      else
        - `J3/J2` * sinio * (3 + 5*θ) / 1.5e-12 / 4
    
    // other derived coeficients and variables that are used related to drag corrections
    val dragSecularCoefs = DragSecularCoefs(    
      if (e0 > 0.0001.as[F]) - 2*q0ms_ξ__to4 * bStar / e0η / 3 else 0.as[F],
      bStar*C3*cos(ω0),
      7 * `β0²` * xhdot1 * C1 / 2,
      xlcof,
       - `J3/J2` * sinio / 2,
      (1+η*cos(M0))**3)
      
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

case class SecularFrequencies[F](Mdot: F, ωdot: F, Ωdot: F)

case class DragSecularCoefs[F](Mcof: F, ωcof: F, Ωcof: F, xlcof: F, aycof: F, delM0: F)

case class LaneCoefs[F](t2cof: F, t3cof: F, t4cof: F, t5cof: F)