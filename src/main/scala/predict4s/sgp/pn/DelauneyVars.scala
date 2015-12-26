package predict4s.sgp.pn

import spire.algebra.Trig
import spire.algebra.Order
import spire.algebra.Field
import spire.algebra.NRoot
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import predict4s.coord.SGPElems

case class DelauneyVars[F](l: F, g: F, h: F, L: F, G: F, H: F)


object DelauneyVars {

  def sgpelem2Delauney[F : Field : NRoot : Order : Trig](elem : SGPElems[F]) = {
    import elem._
    val ℓ = M  // mean anomaly
    val g = ω  // argument of the periapsis
    val h = Ω  // RAAN
    val L = sqrt(a)  // Delaunay action, in reality sqrt(nu*a) but nu is taken out to be added later
    val G = L * sqrt(1 - e*e)
    val H = G * cos(I)
    DelauneyVars(ℓ,g,h,L,G,H)
  }    

  def sgpelem2DelauneyCtx[F : Field : NRoot : Order : Trig](elem : SGPElems[F]) = {
    import elem._
    val cosI = cos(I)
    val `e²` = e*e
//    val p = a*(1 - `e²`)
//    if (p < 0.as[F]) throw new Exception("p: " + p)
//    val `√p` = sqrt(p) 
    val β = sqrt(1 - `e²`)  
    val ℓ = M  // mean anomaly
    val g = ω  // argument of the periapsis
    val h = Ω  // RAAN
    val L = sqrt(a)  // Delaunay action, in reality sqrt(nu*a) but nu is taken out to be added later
    val G = L * β
    val H = G * cosI
    DelauneyVars(ℓ,g,h,L,G,H)
  }
  
  def secularCtx2Delauney[F : Field : NRoot : Order : Trig](M: F, ω: F, Ω : F, a : F, e : F, cosI: F) = {
    val ℓ = M  // mean anomaly
    val g = ω  // argument of the periapsis
    val h = Ω  // RAAN
    val L = sqrt(a)  // Delaunay action, in reality sqrt(nu*a) but nu is taken out to be multiplied later
    val G = L * sqrt(1 - e*e)
    val H = G * cosI
    DelauneyVars(ℓ,g,h,L,G,H)
  }    
}