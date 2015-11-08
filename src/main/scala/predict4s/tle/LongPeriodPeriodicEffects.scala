package predict4s.tle

import spire.algebra._
import spire.math._
import spire.implicits._
import predict4s.tle.TEME.SGPElems

trait SGP4LongPeriodicEffects {
  
  def calculateSGP4LongPeriodicEffects[F: Field: NRoot : Order: Trig](tif: SGP4TimeIndependentFunctions[F], el: TEME.SGPElems[F], am: F) = {
       /* ----------------- compute extra mean quantities ------------- */
    import el._
    //val sinim = sin(i) //  sin(inclm);
    //val cosim = cos(i)

//     ep     = em;
//     xincp  = inclm;
//     argpp  = argpm;
//     nodep  = nodem;
//     mp     = mm;
//     sinip  = sinim;
//     cosip  = cosim;
    
    // p for periodics
    // inputs are the mean elements
     val ep     = e 
     val xincp  = i 
     val argpp  = ω 
     val nodep  = Ω 
     val mp     = M 
     //val sinip  = sinim 
     //val cosip  = cosim 
     
     val axnl = ep * cos(argpp)
     val temp = 1 / (am * (1 - ep * ep))
     val aynl = ep * sin(argpp) + temp * tif.ocf.aycof
     val xl   = mp + argpp + nodep + temp * tif.ocf.xlcof * axnl

     // Are these variables in relation with Delauney's? 
     (nodep, axnl, aynl, xl)
  }
  
  
  def calcHootsSGP4LongPeriodicEffects[F: Field: NRoot : Order: Trig](tif: SGP4TIF[F], el: TEME.SGPElems[F], ocf: HootsOtherCoefs[F], am: F) = {
       /* ----------------- compute extra mean quantities ------------- */
    import el._
    //val sinim = sin(i) //  sin(inclm);
    //val cosim = cos(i)

//     ep     = em;
//     xincp  = inclm;
//     argpp  = argpm;
//     nodep  = nodem;
//     mp     = mm;
//     sinip  = sinim;
//     cosip  = cosim;
    
    // p for periodics
    // inputs are the mean elements
     val ep     = e 
     val xincp  = i 
     val argpp  = ω 
     val nodep  = Ω 
     val mp     = M 
     //val sinip  = sinim 
     //val cosip  = cosim 
     
     val axnl = ep * cos(argpp)
     val temp = 1 / (am * (1 - ep * ep))
     val aynl = ep * sin(argpp) + temp * ocf.aycof
     val xl   = mp + argpp + nodep + temp * ocf.xlcof * axnl

     // Are these variables in relation with Delauney's? 
     (nodep, axnl, aynl, xl)
  }  
}

object SGP4LongPeriodicEffects extends SGP4LongPeriodicEffects