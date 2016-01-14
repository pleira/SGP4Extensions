package predict4s.coord

import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._

// This initial context is produced when transforming a TLE into SGPElems
// so that certain variables are not calculated again.
case class Context0[F: Field: NRoot : Trig](
    a0: F, `e²`: F,s: F,c: F,`c²`: F, `3c²-1`: F,β0: F,`β0²`: F,`β0³`: F, private val wgs: SGPConstants[F]) {

  def cosI0      = c 
  def `cos²I0`   = `c²`
  def θ          = c
  def `θ²`       = `cos²I0`
  def `θ³`       = cosI0 * `cos²I0`
  def `θ⁴`       = `cos²I0` * `cos²I0`
  def θsq        = `cos²I0`
  def sinI0      = s
  // def sinio      = s

  val `β0⁴`      = `β0²`*`β0²`
  // def rteosq     = β0sq
  def omeosq     = `β0²`
    
  val `s²` = s*s
  val p : F = a0 * `β0²` // a0 * (1 - `e²`) // semilatus rectum , which also is G²/μ, with G as the Delauney's action, the total angular momentum
  val `p²` = p*p
  val `p⁴` = `p²`*`p²`
//  val `1/p²` = 1/`p²`
//  val `1/p⁴` = 1/`p⁴`
//  import wgs.{aE=>α,J2,`J2/J3`,`J3/J2`}
//  val `α/p` : F = α/p
//  val ϵ2 : F = -J2*(`α/p`**2) / 4
//  val ϵ3 : F = (`J3/J2`)*`α/p` / 2      // or (`C30/C20`)*`α/p` / 2   
// FIXME  val η : F = β0  // (1 - `e²`).sqrt           // eccentricity function G/L, with G as the Delauney's action, the total angular momentum , and L = √(μ a)
//  val x3thm1     = 3*`c²` - 1
  def con41 = `3c²-1` // depends on the inclination 
  val `1-5c²` = 1 - 5*`c²`
  val x1mth2 = 1 - `c²`
  val x7thm1 = 7*`c²` - 1   
  val x3thm1     = `3c²-1`   
  val `1-c²`   = 1 - `c²`
  val `7c²-1`  = 7*`c²` - 1   
}
