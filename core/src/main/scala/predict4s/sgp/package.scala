package predict4s

package object sgp {
  
  // for geopotential calculations
  type GeoPotentialCtx[F] = (GeoPotentialCoefs[F], GeoPotentialContext[F])
  
}