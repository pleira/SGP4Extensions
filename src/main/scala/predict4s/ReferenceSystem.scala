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
        i : F, // inclination
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
      
  case class SGPElems[F](
        n : F, // mean motion 
        e : F, // eccentricity
        i : F, // inclination
        ω : F, // argument Of perigee
        Ω : F, // right ascension ascending node
        M : F, // mean anomaly
        bStar : F, // atmospheric Drag Coeficient
        epoch : F) // epoch time in days from jan 0, 1950. 0 hr 
  {
    def n0 = n  
    def e0 = e 
    def i0 = i 
    def ω0 = ω 
    def Ω0 = Ω
    def M0 = M 
  }
  
         
  case class CartesianElems[F](x: F, y: F, z: F, vx: F, vy: F, vz: F) {
    def pos = Vector[F](x,y,z)
    def vel = Vector[F](vx,vy,vz)
  }
   /**
    *  the Delaunay actions are L, G and H with units of angular momentum    
    */
  case class DelaunayElems[F](
        ℓ : F, L : F,  // mean anomaly and its conjugate momentum L = √µ a ,
        g : F, G : F,  // the argument of the perigee g = ω and its conjugate momentum G = L√(1 − e*e)
                       // (the total angular momentum), where e is the orbital eccentricity, 
        h : F, H : F   // the argument of the node h and its conjugate momentum H = G cosI (the polar component of the angular momentum).       
  )
  
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
		* velocity vectors in the orbital frame. Thus R_3 (−ν) ◦ R_1 (−I) ◦ R_3 (−θ) 
		* with I the orbital inclination where R_1 , R_3 , are the usual rotation matrices
		*/
    def polarNodal2Cartesian(i : F) : CartesianElems[F] = {
       // After replacing ν = ψ − θ and sin θ = ξ/s, cos θ = χ/s, the
       // transformation from nonsingular to Cartesian variables can be obtained from the sequence
       // (s and c are abbreviations for the sine and cosine of the inclination)
       val s = sin(i)
       val c = cos(i)
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
  
  // inclination, su and right ascension ascending node
  def coord2UnitCartesian[F: Field: Trig](incl: F, su: F, Ω: F) = {
  
      /* --------------------- orientation vectors ------------------- */
      val     sinsu =  sin(su)
      val     cossu =  cos(su)
      val     snode =  sin(Ω)
      val     cnode =  cos(Ω)
      val     sini  =  sin(incl)
      val     cosi  =  cos(incl)
      val     xmx   = -snode * cosi
      val     xmy   =  cnode * cosi
      val     ux    =  xmx * sinsu + cnode * cossu
      val     uy    =  xmy * sinsu + snode * cossu
      val     uz    =  sini * sinsu
      val     vx    =  xmx * cossu - cnode * sinsu
      val     vy    =  xmy * cossu - snode * sinsu
      val     vz    =  sini * cossu
  
      // return unit vectors position and velocity
      CartesianElems(ux,uy,uz,vx,vy,vz)
  }
  
    def classical2DelaunayElems[F: Field: Trig: NRoot]( oe : ClassicalElems[F], MU : F) : DelaunayElems[F] = {
//  def classical2DelaunayElems[F: Field: Trig: NRoot]( oe : ClassicalElems[F])(implicit wgs: WGSConstants[F]) : DelaunayElems[F] = {
    import oe._
    val L = (MU*a).sqrt; val G = L*(1 - e*e).sqrt; val H = G*cos(i)
    DelaunayElems(M, L, ω, G, Ω, H)
  }
}

// can we do without exceptions, just scala.util.Try?
case class Predict4sException(msg: String) extends Exception 

