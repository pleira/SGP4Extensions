package predict4s.tle.lara

import spire.algebra._
import spire.math._
import spire.implicits._
import predict4s.tle.TEME.SGPElems
import spire.syntax.primitives._
import predict4s.tle._

/**
 *  Kepler's equation
 *  https://en.wikipedia.org/wiki/Kepler%27s_equation
 *  See the section Numerical approximation of inverse problem
 */
trait NewtonRaphsonKeplerSolver {

  /**
   * Solve the Kepler equation 
   * 		ℓ = E - e sinE
   * where E is the eccentric anomaly.
   * We are using Delauney's elements as variables.
   */
  def solve[F: Field: NRoot : Order: Trig](secularEffects: SecularEffects[F]): EccentricAnomalyState[F]= {
    
   
    import secularEffects.gpState.dps
    import dps.elem.{e,Ω,ω,M,a}, secularEffects.ocofs.twopi
     
    //---------------------------------------------------------------------------------------
    // CONFIRM
    //---------------------------------------------------------------------------------------
    
    // TBC : Delaunay Transformation
    val axnl = e * cos(ω)
    val temp = 1 / (a * (1 - e * e))
    
    // TBC: is here LPPE added? should aycof and xlcof go away here?
    import secularEffects.ocofs.{aycof,xlcof}
    val aynl = e * sin(ω) + temp * aycof
    val xl   = M + ω + Ω + temp * xlcof * axnl
     
    //---------------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------------

    
    
    
    /* --------------------- solve kepler's equation  M = E - e sin E     --------------- */
    // Nodep (or M) is the mean anomaly, E is the eccentric anomaly, and e is the eccentricity.
    var ktr : Int = 1
    val u    = Field[F].mod(xl - Ω, twopi)
    var eo1  = u
    var tem5 : F = 9999.9.as[F]     //   sgp4fix for kepler iteration
    var ecosE : F = 0.as[F]
    var esinE : F = 0.as[F]
    var coseo1 : F = 0.as[F]
    var sineo1 : F = 0.as[F]
     
    //   the following iteration needs better limits on corrections
    while (( abs(tem5) >= 1e-12.as[F]) && (ktr <= 10) )
     {
       sineo1 = sin(eo1)
       coseo1 = cos(eo1)
       ecosE = axnl * coseo1 + aynl * sineo1
       esinE = axnl * sineo1 - aynl * coseo1

       val fdot   = 1 - ecosE
       val f = (u + esinE - eo1)
       tem5   = f / fdot  // delta value
       if(abs(tem5) >= 0.95.as[F])
           tem5 = if (tem5 > 0.as[F]) 0.95.as[F]  else -0.95.as[F] 
       eo1    = eo1 + tem5
       ktr = ktr + 1
     }
     
     EccentricAnomalyState(eo1,coseo1,sineo1,ecosE,esinE,secularEffects)
   
  }


  /**
   * Solve the Kepler equation 
   * 		U = Ψ + S' cosΨ − C' sinΨ,
	 * where U = F' − h' to compute the anomaly Ψ = E' + g', 
   * where E is the eccentric anomaly. The Newton-Raphson iterations start from Ψ0 = U.
   * We are using Lydanne's elements as variables.
   */
//    def solve[F: Field: NRoot : Order: Trig](lppState: LongPeriodPeriodicState[F]): EccentricAnomalyState[F]= {
//      
//      /* --------------------- solve kepler's equation  M = E - e sin E     --------------- */
//     
//      // Nodep (or M) is the mean anomaly, E is the eccentric anomaly, and e is the eccentricity.
//     import lppState._
//     import secularState._
//     import elems.Ω, ocofs.twopi
//     
//     var ktr : Int = 1
//     val u    = Field[F].mod(xl - Ω, twopi)
//     var eo1  = u
//     var tem5 : F = 9999.9.as[F]     //   sgp4fix for kepler iteration
//     var ecosE : F = 0.as[F]
//     var esinE : F = 0.as[F]
//     var coseo1 : F = 0.as[F]
//     var sineo1 : F = 0.as[F]
//     
//     //   the following iteration needs better limits on corrections
//     while (( abs(tem5) >= 1e-12.as[F]) && (ktr <= 10) )
//       {
//         sineo1 = sin(eo1)
//         coseo1 = cos(eo1)
//         ecosE = axnl * coseo1 + aynl * sineo1
//         esinE = axnl * sineo1 - aynl * coseo1
//
//         val fdot   = 1 - ecosE
//         val f = (u + esinE - eo1)
//         tem5   = f / fdot  // delta value
//         if(abs(tem5) >= 0.95.as[F])
//             tem5 = if (tem5 > 0.as[F]) 0.95.as[F]  else -0.95.as[F] 
//         eo1    = eo1 + tem5
//         ktr = ktr + 1
//       }
//     
//     EccentricAnomalyState(eo1,coseo1,sineo1,ecosE,esinE,lppState)
//     
//    }

}

object NewtonRaphsonKeplerSolver extends NewtonRaphsonKeplerSolver

case class EccentricAnomalyState[F](eo1 : F, coseo1: F, sineo1: F, ecosE: F, esinE: F, secularState: SecularEffects[F])  
