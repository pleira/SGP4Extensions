package predict4s
package sgp

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.coord._
import org.scalactic.Or
import org.scalactic.Good
import org.scalactic.Bad

case class SecularFrequencies[@sp(Double) F](Mdot: F, ωdot: F, Ωdot: F)

case class DragSecularCoefs[@sp(Double) F](Mcof: F, ωcof: F, Ωcof: F)

case class LaneCoefs[@sp(Double) F](T2: F, T3: F, T4: F, T5: F)

class BrouwerLaneSecularCorrections[@sp(Double) F : Field : NRoot : Order : Trig]( 
    val elem0Ctx: SGPElemsCtx[F],    
    val geoPotCtx: GeoPotentialCtx[F],
    val laneCoefs : LaneCoefs[F],
    val secularFreqs : SecularFrequencies[F], 
    val dragCoefs : DragSecularCoefs[F]
    )  {

  type Minutes = F // type to remember dealing with minutes from epoch

  // valid interval for eccentricity calculations
  def eValidInterval = Interval.open(0.as[F],1.as[F])
     
  def secularCorrections(t: Minutes): SGPSecularResult[F] = {
    
    import secularFreqs._  // {ωdot,Ωdot,mdot=>Mdot,Ωcof}
    import dragCoefs._  
    import elem0Ctx.{elem,iCtx,eCtx,wgs} 
    import elem._,wgs.{KE,`2/3`,`2pi`}
 
    // Brouwer’s gravitational corrections are applied first
    // Note that his theory relies on Delaunays variables, 
    // ωdot is gdot, Mdot is ℓdot, and  Ωdot is hdot.
    val ωdf  : F = ω + ωdot*t
    val Ωdf  : F = Ω + Ωdot*t
    val Mdf  : F = M + Mdot*t    
    
    // Next, the secular corrections due to the atmospheric drag are incorporated
    // which also take long period terms from drag;
    // in particular δh, δL, δe, δℓ 
   
    val (δL, δe, δℓ, ωm, xMp, xΩm) : (F,F,F,F,F,F) = dragSecularCorrections(t, ωdf, Mdf, Ωdf)
    val Ωm = xΩm  // it seems Scala has some trouble with certain variable names
    val Mp = xMp

    // Compute the secular elements (not exactly secular as they mix long-period terms from drag)
    val am : F  = ((KE/n) fpow `2/3`) * δL * δL // a * tempa**2  
    val nm : F  = KE / (am pow 1.5)
    val em_ : F = e - δe
    
    // fix tolerance for error recognition
    // sgp4fix am is fixed from the previous nm check
    if (!Interval.open(0.as[F],1.as[F]).contains(em_))
      {
        // sgp4fix to return if there is an error in eccentricity
        return Bad(s"Secular eccentricity $em_ outside valid range")
      }

    // sgp4fix fix tolerance to avoid a divide by zero
    val em = if (em_ < 1.0e-6.as[F]) 1.0e-6.as[F] else em_ 
    
    val Mm_ = Mp + n*δℓ
     
    // modulus so that the angles are in the range 0,2pi
    val Ω_ = Ωm  % `2pi`
    val ω_ = ωm  % `2pi`
    
    // Lyddane's variables and back 
    val ℓm = Mm_ + ωm + Ωm
    val lm = ℓm  % `2pi`
    val Mm = (lm - ω_ - Ω_) % `2pi`
    val elems = SGPElems(nm, em, I, ω_, Ω_, Mm, am, bStar, epoch)
    val elemCtx = (elems,iCtx,wgs)
    Good(elemCtx)
  }
  
  /*
   * The secular corrections due to atmospheric drag are calculated
   * by Brouwer/Lane in Delauneys variables. All the necessary constants and coefficients
   * have been evaluated at the initialization stage.
   * δL,δe,δℓ,δh	
   */
  private def dragSecularCorrections(t: Minutes, ωdf: F, Mdf: F, Ωdf: F): (F,F,F,F,F,F) = {

    import laneCoefs._
    import dragCoefs._ // {ωcof,delM0,sinM0,Mcof}    
    import geoPotCtx.{_1=>gcoefs,_2=>gctx}
    import gctx.η,gcoefs._ 
    import elem0Ctx.{elem,iCtx,eCtx,wgs,isImpacting} 
    import elem._,wgs.{KE,`2/3`,`2pi`}
    
    val `t²` = t*t    
    val Ωm  = Ωdf + Ωcof*`t²` 
 
    // It should be noted that when epoch perigee height is less than
    // 220 kilometers, the equations for a and Lane's are truncated after the C1 term, 
    // and the terms involving C5 , δω, and δM are dropped.    
    if (isImpacting) 
      return (1 - C1*t, bStar*C4*t, T2*`t²`, ωdf, Mdf, Ωm)
    
    val `t³` = `t²`*t
    val `t⁴` = `t²`*`t²`
    val delM0 = (1+η*cos(M))**3
    val δω  = ωcof*t
    val δM  = Mcof*( (1+η*cos(Mdf))**3 - delM0)
    val Mpm_  = Mdf + δω + δM
    val ωm_   = ωdf - δω - δM
    
    val δL = 1 - C1*t - D2*`t²` - D3*`t³` - D4*`t⁴`  // (L´´/L0) 
    val δe = bStar*(C4*t + C5*(sin(Mpm_) - sin(M)))  // sin(M) === sin(M0)
    val δℓ = T2*`t²` + T3*`t³` + `t⁴` * (T4 + t*T5)  // (ℓ´´ - ℓj´´)/ n0

    (δL, δe, δℓ, ωm_, Mpm_, Ωm)
  }
  
}

object BrouwerLaneSecularCorrections extends GeoPotentialAndAtmosphere2ndOrderModel { 
  
  /**
   * Factory method to produce all inputs needed to create the propagator for SecularCorrections.
   * Here we are starting with a SGP Elements directly obtained from a TLE. 
   */
  def apply[@sp(Double) F : Field : NRoot : Order : Trig](elem0Ctx: SGPElemsCtx[F]) = {
    val geoPotCtx = geoPotentialCoefs(elem0Ctx)
    val geoPotCoefs = geoPotCtx._1
    val laneCoefs = calcLaneCoefs(geoPotCoefs)
    val (secularFrequencies, hdot) = calcSecularFrequencies(elem0Ctx)
    val dragSecularCoefs = calcSecularDragCoefs(hdot, elem0Ctx, geoPotCtx)
    new BrouwerLaneSecularCorrections(elem0Ctx,geoPotCtx, laneCoefs, secularFrequencies, dragSecularCoefs)
  }
  
  def build[@sp(Double) F : Field : NRoot : Order : Trig](tle: TLE, wgs: SGPConstants[F]) :  BrouwerLaneSecularCorrections[F] Or ErrorMessage = for {
    elem0AndCtx <- SGPElemsConversions.sgpElemsAndContext(tle, wgs)
  } yield BrouwerLaneSecularCorrections(elem0AndCtx)
  
  private def calcSecularFrequencies[@sp(Double) F : Field : NRoot : Order : Trig](elem0Ctx: SGPElemsCtx[F])
      : (SecularFrequencies[F], F) = {
    import elem0Ctx.{elem,iCtx,eCtx,wgs} 
    import wgs._,iCtx._,eCtx._
    import elem.{e => e0,n => n0,a => a0,ω => ω0, M => M0,bStar}

    val p = a0 * `β0²` // a0 * (1 - `e²`) // semilatus rectum , which also is G²/μ, with G as the Delauney's action, the total angular momentum
    val `p²` = p*p
    val `p⁴` = `p²`*`p²`
    
    val ϵ2 = - J2/`p²`/ 4
    val ϵ4 = - 15 * J4 / `p⁴`/ 32
    val n0ϵ4 = n0*ϵ4 
    val `3n0ϵ2` = 3*ϵ2*n0
    val hdot =  2*`3n0ϵ2`*c 
    val secularFrequencies = 
      if (n0 >= 0.as[F] || `β0²` >= 0.as[F])
        SecularFrequencies(// derivative of M 
          n0 - `3n0ϵ2` * β0 * (`3c²-1` - ϵ2 * (13 - 78 * `c²` + 137 * `c⁴`) / 4),
        // derivative of the perigee argument
        `3n0ϵ2` * (1 - 5*`c²` + ϵ2*(7 - 114*`c²` + 395*`c⁴`)/4) + n0ϵ4*(3 - 36*`c²` + 49*`c⁴`),
        // derivative of the raan
        //(2* `3n0ϵ2` * (1 + (ϵ2*(4 - 19*`c²`)) + 2*temp3 * (3 - 7*`c²`)))*c
        hdot + ϵ2*hdot* (4 - 19*`c²`) + 2*n0ϵ4*c* (3 - 7*`c²`)
        )
      else 
        SecularFrequencies(0.as[F],0.as[F],0.as[F])
        
    (secularFrequencies, hdot)
  }
  
  private def calcSecularDragCoefs[@sp(Double) F : Field : NRoot : Order : Trig](hdot: F, elem0Ctx: SGPElemsCtx[F], gctx : GeoPotentialCtx[F])
      : DragSecularCoefs[F] = {
    
    import gctx.{_1=>gcoef,_2=>geoctx}
    import gcoef.{C1,C3},geoctx.{e0η, `ξ⁴(q0-s)⁴`}
    import elem0Ctx.{elem,eCtx,wgs},eCtx.`β0²`,wgs.`2/3` 
    import elem.{e => e0,n => n0,a => a0,ω => ω0, bStar}
    
    // other derived coeficients and variables that are used related to drag corrections
    DragSecularCoefs(    
      if (e0 > 0.0001.as[F]) -`2/3` * `ξ⁴(q0-s)⁴` * bStar / e0η else 0.as[F],
      bStar*C3*cos(ω0),
      7 * `β0²` * hdot * C1 / 2 )    
  }
  
  /**
   * The initialization process provides a series of coefficients needed
   * to apply drag secular corrections as computed from Lane’s theory.
   */
  private def calcLaneCoefs[@sp(Double) F : Field](gcoefs : GeoPotentialCoefs[F]) : LaneCoefs[F] = {
    import gcoefs._
    val `C1²` = C1*C1
    LaneCoefs(
         3*C1/2, 
         D2 + 2*`C1²`, 
         (3*D3 + C1*(12*D2 + 10 * `C1²`))/4,  
         (3*D4 + 12*C1*D3 + 6*D2*D2 + 15*`C1²`*(2*D2+`C1²`))/5)
  }
    
}