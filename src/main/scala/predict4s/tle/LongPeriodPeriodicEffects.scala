package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import predict4s.tle.TEME.SGPElems

// TODO: express the operations here as being part of an AST with a single Context as parameter, 
// returning a description, that is the new perturbed elements after long periodic effects and an updated Context 

trait SGP4LongPeriodicEffects {
  
  def calculateSGP4LongPeriodicEffects[F: Field: NRoot : Order: Trig](tif: SGP4TimeIndependentFunctions[F], el: TEME.SGPElems[F]) = {
       /* ----------------- compute extra mean quantities ------------- */
    // inputs are the mean elements
     val ep     = el.e 
     val xincp  = el.i 
     val argpp  = el.ω 
     val nodep  = el.Ω 
     val mp     = el.M 
     val am     = el.a
     //val sinip  = sinim 
     //val cosip  = cosim 
     
     val axnl = ep * cos(argpp)
     val temp = 1 / (am * (1 - ep * ep))
     val aynl = ep * sin(argpp) + temp * tif.ocf.aycof
     val xl   = mp + argpp + nodep + temp * tif.ocf.xlcof * axnl

     // Are these variables in relation with Delauney's? 
     (nodep, axnl, aynl, xl)
  }
  
  def calcHootsSGP4LongPeriodicEffects[F: Field: NRoot : Order: Trig](tif: SGP4TIF[F], el: TEME.SGPElems[F], ocf: HootsOtherCoefs[F]) = {
       /* ----------------- compute extra mean quantities ------------- */
    import el._
    val axnl = e * cos(ω)
    val temp = 1 / (a * (1 - e * e))
    val aynl = e * sin(ω) + temp * ocf.aycof
    val xl   = M + ω + Ω + temp * ocf.xlcof * axnl

    // Are these variables in relation with Delauney's? 
    (Ω, axnl, aynl, xl)
  }  
}

object SGP4LongPeriodicEffects extends SGP4LongPeriodicEffects