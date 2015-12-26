package predict4s.coord

import spire.algebra.Trig
import spire.algebra.Field
import spire.algebra.NRoot
import spire.math._
import spire.implicits._
import spire.syntax.primitives._
import scala.Vector


case class SGPElems[F](
    n : F, // mean motion 
    e : F, // eccentricity
    I : F, // inclination
    ω : F, // argument Of perigee
    Ω : F, // right ascension ascending node
    M : F, // mean anomaly
    a : F, // semimajor axis (apogee)
    bStar : F, // atmospheric Drag Coeficient
    epoch : F) // epoch time in days from jan 0, 1950. 0 hr 
    
    
case class CartesianElems[F](x: F, y: F, z: F, vx: F, vy: F, vz: F) {
  def pos = Vector[F](x,y,z)
  def vel = Vector[F](vx,vy,vz)
  // Lara uses X,Y,Z for velocities in his formulas. Allow to follow his convention
  def X = vx
  def Y = vy
  def Z = vz
}

// Naming of the elements after "Efficient formulation of the periodic corrections in
// Brouwer’s gravity solution" by Martin Lara
case class PolarNodalElems[F: Field: NRoot: Trig](
    r : F, // the radial distance 
    θ : F, // the argument of latitude of the satellite measured from the ascending node
    ν : F, // the argument of the longitude of the ascending node 
    R : F, // radial velocity, dr/dt 
    Θ : F, // the total angular momentum
    N : F  // the polar component of the angular momentum
) {

  /** 
  * The direct transformation from nonsingular to Cartesian variables is obtained
	* by means of the usual rotations applied to the projections of the position and
	* velocity vectors in the orbital frame. Thus R3 (−ν) ◦ R1 (−I) ◦ R3 (−θ) 
	* with I the orbital inclination where R1 , R3 , are the usual rotation matrices
	*/
  def polarNodal2Cartesian() : CartesianElems[F] = {
     // After replacing ν = ψ − θ and sinθ = ξ/s, cosθ = χ/s, the
     // transformation from nonsingular to Cartesian variables can be obtained from the sequence
     // (s and c are abbreviations for the sine and cosine of the inclination)
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
  
  /*
   * The cosI is given instead of the I
   */
  def polarNodal2SpecialPolarNodal() : SpecialPolarNodal[F] = SpecialPolarNodal(N/Θ,θ,ν,r,R,r*Θ)
  

}

  
case class SpecialPolarNodal[F: Field: NRoot: Trig](
    I: F,  // the orbital inclination 
    θ: F, // TBC the argument of latitude
    Ω: F,  // the argument of the node
    r: F, // the radial distance
    R: F, // the radial velocity 
    `Θ/r` : F  // related to the total angular momentum
  ) {
  def su = θ; def su0 = su; def mrt = r; def mvt = R; def rdot0 = mvt; 
  // Note: Vallado's SGP4 uses rθdot = Θ/r instead of Θ, used by Lara
  def rvdot = `Θ/r`;
  def Θ = rvdot*r;
  def rθdot = Θ/r
  def +(o: SpecialPolarNodal[F]) = SpecialPolarNodal(I+o.I,θ+o.θ,Ω+o.Ω,r+o.r,R+o.R,`Θ/r`+o.`Θ/r`)

  def specialPolarNodal2PolarNodal() =  PolarNodalElems(r,θ,Ω,R,Θ,Θ*cos(I))
}
  
object CoordTransformation  {
  
  /**
   *  Standard transformation from polar-nodal to Cartesian variables
   *  (r,0,0, rdot=R, rθdot = Θ/r, 0) -> (x,y,z, vx,vy,vz)
   *  where r is radial distance, R is radial velocity, and Ω is the node argument
   *  Mathematically involves matrix multiplication  R3(−h) R1(−I) R3(−θ)
   *  where R1 and R3 are the usual rotation matrices about the x and z axes, respectively
   */
  def polarNodal2UnitCartesian[F: Field: Trig](I: F, R: F, Ω: F) = {
    val sinI  =  sin(I); val cosI  =  cos(I)
    val sinR  =  sin(R); val cosR  =  cos(R)
    val sinΩ  =  sin(Ω); val cosΩ  =  cos(Ω)
    val xmx   = -sinΩ * cosI
    val xmy   =  cosΩ * cosI
    val ux    =  xmx * sinR + cosΩ * cosR
    val uy    =  xmy * sinR + sinΩ * cosR
    val uz    =  sinI * sinR
    val vx    =  xmx * cosR - cosΩ * sinR
    val vy    =  xmy * cosR - sinΩ * sinR
    val vz    =  sinI * cosR

    // return unit vectors position and velocity
    CartesianElems(ux,uy,uz,vx,vy,vz)
  }

}


