package predict4s
package coord

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._

// abbreviated LNS
case class LaraNonSingular[@sp(Double) F: Field](ψ : F, ξ: F, χ: F, r: F, R: F, Θ: F) {
  def +(o: LaraNonSingular[F]) = LaraNonSingular(ψ + o.ψ,ξ+ o.ξ,χ+ o.χ,r+ o.r,R+ o.R,Θ+ o.Θ)
  def -(o: LaraNonSingular[F]) = LaraNonSingular(ψ - o.ψ,ξ- o.ξ,χ- o.χ,r- o.r,R- o.R,Θ- o.Θ)
  def `Θ/r` = Θ/r
}

object LNSConversions {
 	  
  val `2pi` = 2*pi

  /*
  def lns2spn[F: Field: Trig: NRoot: Order](lns: LaraNonSingular[F]) : SpecialPolarNodal[F] = {
    import lns._
    val `ξ²` = ξ*ξ
    val `χ²` = χ*χ    
    val θ = atan2(ξ,χ)
    //val c = sqrt(1 - `ξ²` - `χ²`)
    val sinθ = sin(θ)
    val s = if (abs(sinθ) > (1E-6).as[F]) ξ/sin(θ) else χ/cos(θ)// (`ξ²` + `χ²`)
    // TODO: probably, consider θ or another angle to have an indication of the inclination I
	  val ν = (ψ - θ)%`2pi`
    val Ω = ν 
    SpecialPolarNodal(asin(s),θ,Ω,r,R,Θ/r)
  }

  def lns2cpn[@sp(Double) F: Field: NRoot: Order: Trig](lns: LaraNonSingular[F]): CSpecialPolarNodal[F] = {
	  import lns._
    import lns._
    val `ξ²` = ξ*ξ
    val `χ²` = χ*χ    
    val θ = atan2(ξ,χ)
    val c = sqrt(1 - `ξ²` - `χ²`)
    //val s = sqrt(`ξ²` + `χ²`)
	  val ν = (ψ - θ)%`2pi`
    val Ω = ν 
    CSpecialPolarNodal(c,θ,Ω,r,R,Θ/r)
  }
  
  // ν = ψ − θ and sinθ = ξ/s, cosθ = χ/s and c = N/Θ, tanθ = ξ/χ
  // laraNonSingular to PolarNodal
  def lns2pn[@sp(Double) F: Field: NRoot: Order: Trig](lns: LaraNonSingular[F]): PolarNodalElems[F] = {
	  import lns._
	  import lns._
    import lns._
    val `ξ²` = ξ*ξ
    val `χ²` = χ*χ    
    val θ = atan2(ξ,χ)
    val c = sqrt(1 - `ξ²` - `χ²`)
	  val ν = (ψ - θ)%`2pi`
    val Ω = ν 

    PolarNodalElems(r,θ,Ω,R,Θ,Θ*c)
  }
  */

  // ν = ψ − θ and sinθ = ξ/s, cosθ = χ/s and c = N/Θ, tanθ = ξ/χ
  // laraNonSingular to PolarNodal
  def lns2spn[@sp(Double) F: Field: NRoot: Order: Trig](lns: LaraNonSingular[F], N: F): SpecialPolarNodal[F] = {
	  import lns._
	  // atan2 uniquely uses the principal value in the range (−π, π]
//	  val θ = atan2(ξ,χ)
	  val θ = atan2(ξ,χ)
    // convert ν to -2pi,2pi range
	  val ν = (ψ - θ)%`2pi`
    val Ω = ν
    
    SpecialPolarNodal(acos(N/Θ),θ,Ω,r,R,Θ/r)
  }
  
  // ν = ψ − θ and sinθ = ξ/s, cosθ = χ/s and c = N/Θ, tanθ = ξ/χ
  // laraNonSingular to PolarNodal
  def lns2pn[@sp(Double) F: Field: NRoot: Order: Trig](lns: LaraNonSingular[F], N: F): PolarNodalElems[F] = {
	  import lns._
	  // atan2 uniquely uses the principal value in the range (−π, π]
	  val θ = atan2(ξ,χ)
    // convert ν to -2pi,2pi range
	  val ν = (ψ - θ)%`2pi`
    val Ω = ν
    
    PolarNodalElems(r,θ,Ω,R,Θ,N)
  }

  // ν = ψ − θ and sinθ = ξ/s, cosθ = χ/s and c = N/Θ, tanθ = ξ/χ
  // laraNonSingular to CSpecialPolarNodal
  def lns2cpn[@sp(Double) F: Field: NRoot: Order: Trig](lns: LaraNonSingular[F], N: F): CSpecialPolarNodal[F] = {
	  import lns._
	  val θ = atan2(ξ,χ)
    // convert ν to -2pi,2pi range
	  val ν = (ψ - θ)%`2pi`
    val Ω = ν	  
    CSpecialPolarNodal(N/Θ,θ,Ω,r,R,Θ/r)
  }
  

  // specialPolarNodal2LaraNonSingular
  def spn2lns[@sp(Double) F: Field: NRoot: Trig](spn : SpecialPolarNodal[F], ictx: InclinationCtx[F]): LaraNonSingular[F] = {
    import spn._ , ictx.s
    val ψ = Ω + θ
    val ξ = s * sin(θ)
    val χ = s * cos(θ)
    LaraNonSingular(ψ, ξ, χ, r, R, Θ) 
  }
  
//  def polarNodal2LaraNonSingular[F: Field: NRoot: Trig](all : PolarNodalElems[F], SPNAuxVariables[F]) ) : LaraNonSingular[F] = {
//    import all._1._ ,all._2.s
//    val ψ = ν + θ
//    val ξ = s * sin(θ)
//    val χ = s * cos(θ)
//    LaraNonSingular(ψ, ξ, χ, r, R, Θ) 
//  }
  
  def cartesian2lns[@sp(Double) F: Field: NRoot: Trig](pv: CartesianElems[F]): LaraNonSingular[F] = {
    import pv._
    // (x,y,z) position, (X,Y,Z) velocity
    val r : F = (x**2 + y**2 + z**2).sqrt 
    val R : F = (x*X + y*Y + z*Z)/r
    val N : F = x*X - y*Y
    val Θ : F = ((y*Z - z*Y)**2 + (z*X - x*Z)**2 + N**2).sqrt
    val χ : F = (r*Z - z*R)/Θ
    val ξ : F =  z/r
    val c = N/Θ
    val `χ²` : F = χ**2
    val q = ξ * χ / (1 + c)
    val τ : F = 1 - `χ²` / (1 + c)
    // val `q²+τ²` = q*q + τ*τ 
//    val sinψ : F = (x*q + y*τ)/`q²+τ²`/r
//    val cosψ : F = (x*τ - y*q)/`q²+τ²`/r
    val tanψ : F = (x*q + y*τ)/(x*τ - y*q)
    val ψ : F = atan(tanψ)     
    LaraNonSingular(ψ, ξ, χ, r, R, Θ)
  }
  
  // laraNonSingular2Cartesian
  def lns2Cartesian[@sp(Double) F: Field: NRoot: Trig](lns: LaraNonSingular[F], N : F): CartesianElems[F] = {
    import lns._
    val `ξ²` = ξ*ξ
    val `χ²` = χ*χ
    val `R/r` : F = R/r
    val `Θ/r` : F = Θ/r
    val c = N/Θ
    val cosψ = cos(ψ)
    val sinψ = sin(ψ)
    val q = ξ * χ / (1 + c)
    val τ = 1 - `χ²` / (1 + c)
    val b = 1 - `ξ²` / (1 + c)

    val ux = (b * cosψ + q * sinψ)
    val uy = (b * sinψ - q * cosψ)
    val uz = ξ

    CartesianElems(
      r * ux,
      r * uy,
      r * uz,
      R * ux - `Θ/r` * (q * cosψ + τ * sinψ),
      R * uy - `Θ/r` * (q * sinψ - τ * cosψ),
      R * uz + `Θ/r` * χ)
    }
  
  def lns2CartesianCtx[@sp(Double) F: Field: NRoot: Trig](lns: LaraNonSingular[F], N: F): (CartesianElems[F], CartesianElems[F]) = {
    import lns._
    
    val `ξ²` = ξ*ξ
    val `χ²` = χ*χ
    val `R/r` : F = R/r
    val `Θ/r` : F = Θ/r
    val c = N/Θ
    val cosψ = cos(ψ)
    val sinψ = sin(ψ)
    val q = ξ * χ / (1 + c)
    val τ = 1 - `χ²` / (1 + c)
    val b = 1 - `ξ²` / (1 + c)

    val ux = (b * cosψ + q * sinψ)
    val uy = (b * sinψ - q * cosψ)
    val uz = ξ
    
    // FIXME
    val uvx = - (q * cosψ + τ * sinψ)
    val uvy = - (q * sinψ - τ * cosψ)
    val uvz = χ
    
    (CartesianElems(
      r * ux,
      r * uy,
      r * uz,
      R * ux - `Θ/r` * (q * cosψ + τ * sinψ),
      R * uy - `Θ/r` * (q * sinψ - τ * cosψ),
      R * uz + `Θ/r` * χ),
      CartesianElems(ux,uy,uz,uvx,uvy,uvz))
    }
  
  def lns2UnitPositionCartesian[@sp(Double) F: Field: NRoot: Trig](lns: LaraNonSingular[F], N: F): CartesianElems[F] = {
   import lns._

    val `ξ²` = ξ*ξ
    val `χ²` = χ*χ
    val c = N/Θ
    val cosψ = cos(ψ)
    val sinψ = sin(ψ)
    val q = ξ * χ / (1 + c)
    val τ = 1 - `χ²` / (1 + c)
    val b = 1 - `ξ²` / (1 + c)
    
    val ux = (b * cosψ + q * sinψ)
    val uy = (b * sinψ - q * cosψ)
    val uz = ξ

    CartesianElems(
      ux, uy, uz,
      R * ux - Θ * (q * cosψ + τ * sinψ),
      R * uy - Θ * (q * sinψ - τ * cosψ),
      R * uz + Θ * χ)
    }
 
  def lns2uPV[@sp(Double) F: Field: NRoot: Trig](lns: LaraNonSingular[F], N: F): CartesianElems[F] = {
    import lns._
    
    // new cosI  
    val c = N/Θ
    
    val `ξ²` = ξ*ξ
    val `χ²` = χ*χ
    val `R/r` : F = R/r
    val `Θ/r` : F = Θ/r
    val cosψ = cos(ψ)
    val sinψ = sin(ψ)
    val q = ξ * χ / (1 + c)
    val τ = 1 - `χ²` / (1 + c)
    val b = 1 - `ξ²` / (1 + c)

    val ux = (b * cosψ + q * sinψ)
    val uy = (b * sinψ - q * cosψ)
    val uz = ξ
    
    // FIXME as it is not really unit velocity vector
    val uvx = - (q * cosψ + τ * sinψ)
    val uvy = - (q * sinψ - τ * cosψ)
    val uvz = χ
    
    CartesianElems(ux,uy,uz, uvx, uvy, uvz)
  }

  def lns2UnitCartesian[F: Field: Trig: NRoot](lns: LaraNonSingular[F]) : CartesianElems[F] = {
    import lns._
    val `ξ²` = ξ*ξ
    val `χ²` = χ*χ

    val cosψ = cos(ψ)
    val sinψ = sin(ψ)
    val s = sqrt(`ξ²` + `χ²`)
    val c = sqrt(1 - `ξ²` - `χ²`)
    val sinθ = ξ/s
    val cosθ = χ/s
    // val Ω = ψ - θ
    val sinΩ  = sinψ * cosθ - cosψ * sinθ
    val cosΩ  = cosψ * cosθ + sinψ * sinθ
    
    val xmx   = -sinΩ * c
    val xmy   =  cosΩ * c
    val ux    = -sinΩ * c * sinθ + cosΩ * cosθ
    // val ux    = -sinψ * cosθ * c + cosψ * sinθ * c +  cosψ * cosθ * cosθ + sinψ * sinθ * cosθ
//    val ux    =  cosψ * (sinθ * c +  cosθ * cosθ) + sinψ * ( sinθ * cosθ - cosθ * c) 
//    val ux    =  cosψ * (ξ/s* c +  χ/s * χ/s + ξ/s * (1 - `ξ²` - `χ²`) + `χ²` * c/s/s)/(1+c) + sinψ * ( sinθ * cosθ - cosθ * c)(1+c)/(1+c)
   // val ux    =   cosψ * (ξ * s * c +  `χ²`)/s/s + sinψ * ( ξ * χ - χ * s * c) /s/s  
//    val ux    =  c * (cosψ - cos(Ω-θ))/2 + cos(Ω-θ)/2 + cosψ / 2
    val uy    =  cosΩ * c * sinθ + sinΩ * cosθ
    val uz    =  ξ // s * sinθ
    val vx    = -sinΩ * c * cosθ - cosΩ * sinθ
    val vy    =  cosΩ * c * cosθ - sinΩ * sinθ
    val vz    =  χ // s * cosθ

    // return unit vectors position and velocity
    CartesianElems(ux,uy,uz,vx,vy,vz)
  }
}