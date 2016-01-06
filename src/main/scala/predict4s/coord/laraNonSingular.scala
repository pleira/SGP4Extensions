package predict4s.coord

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
  
case class LaraNonSingular[F: Field](ψ : F, ξ: F, χ: F, r: F, R: F, Θ: F) {
  def +(o: LaraNonSingular[F]) = LaraNonSingular(ψ + o.ψ,ξ+ o.ξ,χ+ o.χ,r+ o.r,R+ o.R,Θ+ o.Θ)
}

object LaraConversions {
  
  // ν = ψ − θ and sinθ = ξ/s, cosθ = χ/s and c = N/Θ, tanθ = ξ/χ
  def laraNonSingular2PolarNodal[F: Field: NRoot: Trig](lnSingular: LaraNonSingular[F], N: F) : PolarNodalElems[F] = {
	  import lnSingular._
	  val θ : F = atan(ξ/χ)
	  val ν : F = ψ - θ
    PolarNodalElems(r,θ,ν,R,Θ,N)
  }
  
  // ν = ψ − θ and sinθ = ξ/s, cosθ = χ/s and c = N/Θ, tanθ = ξ/χ
  def laraNonSingular2SpecialPolarNodal[F: Field: NRoot: Trig](lnSingular: LaraNonSingular[F], I: F) : SpecialPolarNodal[F] = {
	  import lnSingular._
	  //val cosI = N/Θ
	  //if (cosI > 1.as[F]) throw new Exception("cosI")
	  // val I = acos(cosI)
	  val θ = atan2(ξ,χ)
	  val ν : F = ψ - θ
    SpecialPolarNodal(I,θ,ν,r,R,Θ/r)
  }

  def specialPolarNodal2LaraNonSingular[F: Field: NRoot: Trig](all : (SpecialPolarNodal[F], AuxVariables[F]) ) : LaraNonSingular[F] = {
    import all._1._ ,all._2.s
    val ψ = Ω + θ
    val ξ = s * sin(θ)
    val χ = s * cos(θ)
    LaraNonSingular(ψ, ξ, χ, r, R, Θ) 
  }
  
  def polarNodal2LaraNonSingular[F: Field: NRoot: Trig](all : (PolarNodalElems[F], AuxVariables[F]) ) : LaraNonSingular[F] = {
    import all._1._ ,all._2.s
    val ψ = ν + θ
    val ξ = s * sin(θ)
    val χ = s * cos(θ)
    LaraNonSingular(ψ, ξ, χ, r, R, Θ) 
  }
  
  def cartesian2LaraNonSingular[F: Field: NRoot: Trig](pv: CartesianElems[F]) : LaraNonSingular[F] = {
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
  
  def laraNonSingular2Cartesian[F: Field: NRoot: Trig](lnSingular: LaraNonSingular[F], N : F) : CartesianElems[F] = {
    import lnSingular._
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
  
  def laraNonSingular2UnitPositionCartesian[F: Field: NRoot: Trig](lnSingular: LaraNonSingular[F], N : F) : CartesianElems[F] = {
    import lnSingular._
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
  
}