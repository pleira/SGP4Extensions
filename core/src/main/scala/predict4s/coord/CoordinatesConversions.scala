package predict4s
package coord

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._

object CoordinatesConversions {

  /** 
  * The direct transformation from nonsingular to Cartesian variables is obtained
	* by means of the usual rotations applied to the projections of the position and
	* velocity vectors in the orbital frame. Thus R3 (−ν) ◦ R1 (−I) ◦ R3 (−θ) 
	* with I the orbital inclination where R1 , R3 , are the usual rotation matrices
	*/
  def polarNodal2Cartesian[F: Field: NRoot: Trig](pn: PolarNodalElems[F]) : CartesianElems[F] = {
    // After replacing ν = ψ − θ and sinθ = ξ/s, cosθ = χ/s, the
    // transformation from nonsingular to Cartesian variables can be obtained from the sequence
    // (s and c are abbreviations for the sine and cosine of the inclination)
    import pn._
    val c = N/Θ
    val s = sqrt(1 - c*c)
    val ψ = ν + θ
    val ξ = s * sin(θ)
    val χ = s * cos(θ)
    val cosψ = cos(ψ)
    val sinψ = sin(ψ)
    val q = ξ*χ / (1+c)
    val t = 1 + ξ*ξ / (1+c)
    val τ = 1 - χ*χ / (1+c)
     
    val ux = (t * cosψ + q * sinψ)
    val uy = (t * sinψ - q * cosψ)
    val uz = ξ
     
    CartesianElems(
         r * ux,
         r * uy,
         r * uz,
         R * ux - Θ / r * (q * cosψ + τ * sinψ),
         R * uy - Θ / r * (q * sinψ - τ * cosψ),
         R * uz + Θ / r * χ )
  }
  
  def polarNodal2SpecialPolarNodal[F: Field: Trig](pn: PolarNodalElems[F]) : SpecialPolarNodal[F] = {
    import pn._    
    SpecialPolarNodal(acos(N/Θ),θ,ν,r,R,r*Θ)
  }
  
  def specialPolarNodal2PolarNodal[F: Field: Trig](spn: SpecialPolarNodal[F]) = {
    import spn._
    PolarNodalElems(r,θ,Ω,R,Θ,Θ*cos(I))
  }
    
  /**
   *  Standard transformation from polar-nodal to Cartesian variables
   *  (r,0,0, rdot=R, rθdot = Θ/r, 0) -> (x,y,z, vx,vy,vz)
   *  where r is radial distance, R is radial velocity, and Ω is the node argument
   *  Mathematically involves matrix multiplication  R3(−h) R1(−I) R3(−θ)
   *  where R1 and R3 are the usual rotation matrices about the x and z axes, respectively
   */
  def spn2UnscaledCartesian[F: Field: Trig](spn: SpecialPolarNodal[F]): CartesianElems[F] = {
    import spn._
    val sinI  =  sin(I); val cosI  =  cos(I)
    val sinθ  =  sin(θ); val cosθ  =  cos(θ)
    val sinΩ  =  sin(Ω); val cosΩ  =  cos(Ω)
    val xmx   = -sinΩ * cosI
    val xmy   =  cosΩ * cosI
    val ux    =  xmx * sinθ + cosΩ * cosθ
    val uy    =  xmy * sinθ + sinΩ * cosθ
    val uz    =  sinI * sinθ
    val vvx   =  `Θ/r`*(xmx * cosθ - cosΩ * sinθ)
    val vvy   =  `Θ/r`*(xmy * cosθ - sinΩ * sinθ)
    val vvz   =  `Θ/r`*(sinI * cosθ)
    // unscaled velocity vector, not unit velocity
    val vx    =  R*ux + vvx
    val vy    =  R*uy + vvy
    val vz    =  R*uz + vvz    
    // return unit vectors position and unscaled velocity
    CartesianElems(ux,uy,uz,vx,vy,vz)
  }

  def pn2UnitCartesian[F: Field: Trig](spn: SpecialPolarNodal[F]) : CartesianElems[F] = {
    import spn._
    // val θ = atan2(ξ,χ)
    val sinθ  =  sin(θ); val cosθ  =  cos(θ) // to replace
    // val s = if (sin0 != 0) ξ/sinθ else χ/cosθ  // sinI
    // val `s2` = ξ2 + χ2
    // val `c2` = 1 - ξ2 - χ2
    // val s = sqrt(ξ2 + χ2)
    // val c = sqrt(1 - ξ2 - χ2)
    // val Ω = ψ - θ
    // val sinθ = ξ/s
    // val cosθ = χ/s
    // val sinΩ  = sinψ * cosθ - cosψ * sinθ
    // val cosΩ  = cosψ * cosθ + sinψ * sinθ
    val sinI  =  sin(I); val cosI  =  cos(I)
    val sinΩ  =  sin(Ω); val cosΩ  =  cos(Ω)
    val s = sinI ; val c = cosI; val χ = s * cosθ; val ξ = s * sinθ; 
    
    val xmx   = -sinΩ * c
    val xmy   =  cosΩ * c
    val ux    = -sinΩ * c * sinθ + cosΩ * cosθ
    val uy    =  cosΩ * c * sinθ + sinΩ * cosθ
    val uz    =  ξ // sinI * sinθ
    val vx    = -sinΩ * c * cosθ - cosΩ * sinθ  
    val vy    =  cosΩ * c * cosθ - sinΩ * sinθ
    val vz    =  χ // sinI * cosθ

    // return unit vectors position and velocity
    CartesianElems(ux,uy,uz,vx,vy,vz)
  }  
}
