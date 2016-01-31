package predict4s.coord

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.{ specialized => spec }
import spire.syntax.primitives._
  
case class LaraNonSingular[F: Field](ψ : F, ξ: F, χ: F, r: F, R: F, Θ: F) {
  def +(o: LaraNonSingular[F]) = LaraNonSingular(ψ + o.ψ,ξ+ o.ξ,χ+ o.χ,r+ o.r,R+ o.R,Θ+ o.Θ)
  def -(o: LaraNonSingular[F]) = LaraNonSingular(ψ - o.ψ,ξ- o.ξ,χ- o.χ,r- o.r,R- o.R,Θ- o.Θ)
  def `Θ/r` = Θ/r
}

object LaraConversions {
 	  
  val `2pi` = 2*pi
 	  
  // ν = ψ − θ and sinθ = ξ/s, cosθ = χ/s and c = N/Θ, tanθ = ξ/χ
  def laraNonSingular2PolarNodal[F: Field: NRoot: Order: Trig](lnSingular: LaraNonSingular[F], N: F) : PolarNodalElems[F] = {
	  import lnSingular._
	  // atan2 uniquely uses the principal value in the range (−π, π]
	  val θ = atan2(ξ,χ)
    // convert ν to -2pi,2pi range
	  val ν = (ψ - θ)%`2pi`
    val Ω = ν
    
    PolarNodalElems(r,θ,Ω,R,Θ,N)
  }
  
  // ν = ψ − θ and sinθ = ξ/s, cosθ = χ/s and c = N/Θ, tanθ = ξ/χ
  def laraNonSingular2SpecialPolarNodal[F: Field: NRoot: Order: Trig](lnSingular: LaraNonSingular[F], I: F) : SpecialPolarNodal[F] = {
	  import lnSingular._
	  val θ = atan2(ξ,χ)
    // convert ν to -2pi,2pi range
	  val ν = (ψ - θ)%`2pi`
    val Ω = ν // if (ν > 0.as[F]) ν else - ν
// 	  // convert to -pi,pi range
//    val Ω = 
//      if (ν > 0.as[F]) {
//        if (ν > pi) ν - `2pi`  else   ν
//      } else {
//         if (ν < -pi.as[F]) `2pi`+ν  else   ν
//      }
 	  
    SpecialPolarNodal(I,θ,Ω,r,R,Θ/r)
  }

  def specialPolarNodal2LaraNonSingular[F: Field: NRoot: Trig](spn : SpecialPolarNodal[F], ictx: InclinationCtx[F]) : LaraNonSingular[F] = {
    import spn._ , ictx.s
    val ψ = (Ω + θ)%`2pi`
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
  
  def laraNonSingular2CartesianCtx[F: Field: NRoot: Trig](laraCtx: SGPLaraCtx[F]) 
      : (CartesianElems[F], CartesianElems[F]) = {
    import laraCtx.{_1=>lnSingular,_2=>lppAndsecularCtx}
    import lnSingular._,lppAndsecularCtx.{_2=>secularCtx}
    import secularCtx._2.c
    
    val `ξ²` = ξ*ξ
    val `χ²` = χ*χ
    val `R/r` : F = R/r
    val `Θ/r` : F = Θ/r
    //val c = N/Θ
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
  
  def laraNonSingular2UnitPositionCartesian[F: Field: NRoot: Trig](laraCtx: SGPLaraCtx[F]) : CartesianElems[F] = {
    import laraCtx.{_1=>lnSingular,_2=>lppAndsecularCtx}
    import lnSingular._,lppAndsecularCtx.{_2=>secularCtx}
    import secularCtx._2.c

    val `ξ²` = ξ*ξ
    val `χ²` = χ*χ
    //val c = N/Θ
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
 
  def laraNonSingular2uPV[F: Field: NRoot: Trig](laraCtx: SGPLaraCtx[F]) 
      : CartesianElems[F] = {
    import laraCtx.{_1=>lnSingular,_2=>lppAndsecularCtx}
    import lnSingular._,lppAndsecularCtx.{_2=>secularCtx}
    import secularCtx._2.c
    
    val `ξ²` = ξ*ξ
    val `χ²` = χ*χ
    val `R/r` : F = R/r
    val `Θ/r` : F = Θ/r
    //val c = N/Θ
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
    
    CartesianElems(ux,uy,uz, uvx, uvy, uvz)
  }
  
}