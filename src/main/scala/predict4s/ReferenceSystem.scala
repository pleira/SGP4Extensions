package predict4s
import spire.algebra.{Trig, NRoot}
import spire.algebra.Field
import spire.math._
import spire.implicits._
import spire.syntax.primitives._

trait ReferenceSystem {

  case class ClassicalElems[F](
        a : F, // semimajor axis 
        e : F, // eccentricity
        I : F, // inclination
        ω : F, // argument of perigee
        Ω : F, // right ascension ascending node
        M : F) // mean anomaly
  
  case class OrbitalElements[F](val a : F, val e : F, val i : F, val ω : F, val Ω : F, val ν : F) {
    def semiMajorAxis = a
    def eccentricity = e
    def inclination = i
    def argumentOfPeriapsis = ω
    def rightAscension = Ω
    def trueAnomaly = ν
    override def toString = s"a: $a, e: $e, i: $i, raan: $Ω, ω: $ω, true anomaly: $trueAnomaly"
  }
                 
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
  case class PolarNodalElems[F: Field: Trig](
      r : F, // the radial distance 
      θ : F, // the argument of latitude 
      ν : F, // the argument of the ascending node 
      R : F, // radial velocity 
      Θ : F, // the total angular momentum
      N : F  // the polar component of the angular momentum
  ) {
  
   /** 
    * The direct transformation from nonsingular to Cartesian variables is obtained
		* by means of the usual rotations applied to the projections of the position and
		* velocity vectors in the orbital frame. Thus R3 (−ν) ◦ R1 (−I) ◦ R3 (−θ) 
		* with I the orbital inclination where R1 , R3 , are the usual rotation matrices
		*/
    def polarNodal2Cartesian(I : F) : CartesianElems[F] = {
       // After replacing ν = ψ − θ and sin θ = ξ/s, cos θ = χ/s, the
       // transformation from nonsingular to Cartesian variables can be obtained from the sequence
       // (s and c are abbreviations for the sine and cosine of the inclination)
       val s = sin(I)
       val c = cos(I)
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
  }
  
  /**
   *  Standard transformation from polar-nodal to Cartesian variables
   *  (r,0,0, rdot=R, rθdot = Θ/r, 0) -> (x,y,z, vx,vy,vz)
   *  where r is radial distance, R is radial velocity, and Ω is the node argument
   *  Mathematically involves matrix multiplication  R3(−h) R1(−I) R3(−θ)
   *  where R1 and R3 are the usual rotation matrices about the x and z axes, respectively
   */
  def polarNodal2UnitCartesian[F: Field: Trig](I: F, R: F, Ω: F) = {
  
      /* --------------------- orientation vectors ------------------- */
      val     sinR  =  sin(R);         val     cosR  =  cos(R)
      val     sinΩ  =  sin(Ω);         val     cosΩ  =  cos(Ω)
      val     sinI  =  sin(I);         val     cosI  =  cos(I)
      val     xmx   = -sinΩ * cosI
      val     xmy   =  cosΩ * cosI
      val     ux    =  xmx * sinR + cosΩ * cosR
      val     uy    =  xmy * sinR + sinΩ * cosR
      val     uz    =  sinI * sinR
      val     vx    =  xmx * cosR - cosΩ * sinR
      val     vy    =  xmy * cosR - sinΩ * sinR
      val     vz    =  sinI * cosR
  
      // return unit vectors position and velocity
      CartesianElems(ux,uy,uz,vx,vy,vz)
  }

}

// can we do without exceptions, just scala.util.Try?
case class Predict4sException(msg: String) extends Exception 

