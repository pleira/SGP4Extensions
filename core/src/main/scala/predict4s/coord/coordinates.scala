package predict4s
package coord

import spire.algebra.Trig
import spire.algebra.Field
import spire.algebra.NRoot
import spire.math._
import spire.implicits._
import scala.Vector


case class SGPElems[@sp(Double) F](
    n : F, // mean motion
    e : F, // eccentricity
    I : F, // inclination
    ω : F, // argument Of perigee
    Ω : F, // right ascension ascending node
    M : F, // mean anomaly
    a : F, // semimajor axis (apogee)
    bStar : F, // atmospheric Drag Coeficient
    epoch : F) // epoch time in days from jan 0, 1950. 0 hr


case class CartesianElems[@sp(Double) F : Field](x: F, y: F, z: F, vx: F, vy: F, vz: F) {
  def pos = Vector[F](x,y,z)
  def vel = Vector[F](vx,vy,vz)
  // Lara uses X,Y,Z for velocities in his formulas. Allow to follow his convention
  def X = vx
  def Y = vy
  def Z = vz
  def xdot = vx; def ydot = vy ; def zdot = vz;
  // used to calculate differences in results or to add corrections
  def +(c: CartesianElems[F]) = CartesianElems(x+c.x,y+c.y,z+c.z,vx+c.vx,vy+c.vy,vz+c.vz)
  def -(c: CartesianElems[F]) = CartesianElems(x-c.x,y-c.y,z-c.z,vx-c.vx,vy-c.vy,vz-c.vz)
}

// Naming of the elements after "Efficient formulation of the periodic corrections in
// Brouwer’s gravity solution" by Martin Lara
case class PolarNodalElems[@sp(Double) F](
    r : F, // the radial distance
    θ : F, // the argument of latitude of the satellite measured from the ascending node
    ν : F, // the argument of the longitude of the ascending node
    R : F, // radial velocity, dr/dt
    Θ : F, // the total angular momentum
    N : F  // the polar component of the angular momentum
)

case class SpecialPolarNodal[@sp(Double) F: Field: Trig](
    I: F,  // the orbital inclination
    θ: F,  // the argument of latitude from the ascending node
    Ω: F,  // the argument of the node
    r: F,  // the radial distance
    R: F,  // the radial velocity
    `Θ/r` : F  // related to the total angular momentum
  ) {
  def su = θ; def su0 = su; def mrt = r; def mvt = R; def rdot0 = mvt;
  // Note: Vallado's SGP4 uses rθdot = Θ/r instead of Θ, used by Lara
  def rvdot = `Θ/r`;
  def Θ = `Θ/r`*r;
  def rθdot = Θ/r
  def +(o: SpecialPolarNodal[F]) = SpecialPolarNodal(I+o.I,θ+o.θ,Ω+o.Ω,r+o.r,R+o.R,`Θ/r`+o.`Θ/r`)
  def -(o: SpecialPolarNodal[F]) = SpecialPolarNodal(I-o.I,θ-o.θ,Ω-o.Ω,r-o.r,R-o.R,`Θ/r`-o.`Θ/r`)
  def toCPN = CSpecialPolarNodal(cos(I),θ,Ω,r,R,`Θ/r`)
}

// This class is just used in one test/comparison between Lara's SGP4 algorithm and the one in polar nodals
case class CSpecialPolarNodal[@sp(Double) F: Field: Trig](
    cosI: F, //  CosI, related to the orbital inclination
    θ: F,  // the argument of latitude from the ascending node
    Ω: F,  // the argument of the node
    r: F,  // the radial distance
    R: F,  // the radial velocity
    `Θ/r` : F  // related to the total angular momentum
  ) {
  def su = θ; def su0 = su; def mrt = r; def mvt = R; def rdot0 = mvt;
  // Note: Vallado's SGP4 uses rθdot = Θ/r instead of Θ, used by Lara
  def rvdot = `Θ/r`;
  def Θ = `Θ/r`*r;
  def rθdot = Θ/r
  def toSPN = SpecialPolarNodal(acos(cosI),θ,Ω,r,R,`Θ/r`)
}
