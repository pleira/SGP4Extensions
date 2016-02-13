package predict4s

package object sgp {
 
   // type sp      = scala.specialized
    
  // for geopotential calculations
  type GeoPotentialCtx[F] = (GeoPotentialCoefs[F], GeoPotentialContext[F])
  
}