package predict4s.report

import java.io.InputStream
import java.io.FileInputStream
import scala.io.Source
import predict4s.sgp._
import predict4s.coord._
import predict4s.sgp._
import spire.implicits._
import spire.math._

trait LoadCppResults {
  
  def loadCppOutputData(sat: Int) : List[CartesianPNLppUnit] = {
    val path = s"/valladocpp/${sat}_wgs72.txt"
    val stream = getClass.getResourceAsStream(path)
    val cppOuts = Source.fromInputStream( stream ).getLines.filter { x => !(x.isEmpty() || x.startsWith("#")) }.toList map (l => CartesianPNLppUnit(l))
    cppOuts
  }

}

case class CartesianPNLppUnit (line: String) {
// line is    t: F, x: F, y: F, z: F, vx: F, vy: F, vz: F, I: F, θ: F, Ω: F, r: F, R: F, `Θ/r` : F, Il: F, θl: F, Ωl: F, rl: F, Rl: F, `Θl/rl` : F, ux: F, uy: F, uz: F
val l = line.trim.split("\\s+")
val t = l(0)
val x = l(1); val y = l(2); val z = l(3); val vx = l(4); val vy = l(5); val vz = l(6);
// these are without scaling, that is, in internal units
val I = l(7); val θ = l(8); val Ω = l(9); val r  = l(10); val R  = l(11); val `Θ/r` = l(12); 
val Il = l(15); val θl = l(16); val Ωl = l(17); val rl = l(18); val Rl = l(19); val `Θl/rl` = l(20);
val ux = l(24); val uy = l(25); val uz = l(26)
}
//    1             2                3                   4                5                6                7                8                9                0                1                2                3          4 5      6                7                8                9                0               1           2  3      4     5               6               7 
//    0.000000000   1442.101329117   6510.236254492      8.831458851     -3.475714837      0.997262768      6.835860345      1.085005548      0.001497722      1.352104124      1.045454739      0.029223183     0.977798633 - LP     1.084697696      0.001497918      1.352103081      1.044983279      0.029224321     0.977670463 -  22312  0     0.216270276     0.976332635     0.001324444 

trait DiffResults {

  def diffPosVel(opv : (CartesianPNLppUnit,CartesianElems[Double])) = {
    val o = opv._1
    val c = opv._2
    val dx = abs(o.x.toDouble - c.x)
    val dy = abs(o.y.toDouble - c.y)
    val dz = abs(o.z.toDouble - c.z)
    val dvx = abs(o.vx.toDouble - c.vx)
    val dvy = abs(o.vy.toDouble - c.vy)
    val dvz = abs(o.vz.toDouble - c.vz)
    val mxy = spire.math.max(dx,dy)
    val mxyz = spire.math.max(mxy, dz)
    val mvxy = spire.math.max(dvx, dvy)
    val mvxyz = spire.math.max(mvxy, dvz)
    (mxyz, mvxyz)
  }
    
  def diffPosVelCartesian(opv : (CartesianPNLppUnit,CartesianElems[Double])) : (String, CartesianElems[Double]) = {
    val o = opv._1
    val c = opv._2
    (o.t, CartesianElems(o.x.toDouble - c.x, o.y.toDouble - c.y, o.z.toDouble - c.z,
            o.vx.toDouble - c.vx, o.vy.toDouble - c.vy, o.vz.toDouble - c.vz))
  }
    
  def diffPN(opn : (CartesianPNLppUnit,SpecialPolarNodal[Double])) : (String, SpecialPolarNodal[Double]) = {
    val o = opn._1
    val s = opn._2
    (o.t, SpecialPolarNodal(o.I.toDouble - s.I, o.θ.toDouble - s.θ, o.Ω.toDouble - s.Ω,
            o.r.toDouble - s.r, o.R.toDouble - s.R, o.`Θ/r`.toDouble - s.`Θ/r`))
  }
    
  def diffPNLPP(opn : (CartesianPNLppUnit,SpecialPolarNodal[Double])) : (String, SpecialPolarNodal[Double]) = {
    val o = opn._1
    val s = opn._2
    (o.t, SpecialPolarNodal(o.Il.toDouble - s.I, o.θl.toDouble - s.θ, o.Ωl.toDouble - s.Ω,
            o.rl.toDouble - s.r, o.Rl.toDouble - s.R, o.`Θl/rl`.toDouble - s.`Θ/r`))
  }
  
  def calculateDiffList(os: List[CartesianPNLppUnit], pvs: IndexedSeq[CartesianElems[Double]]) : List[(String, CartesianElems[Double])] = {
    os zip pvs map diffPosVelCartesian
  }
    
  def calculatePnDiffList(os: List[CartesianPNLppUnit], pns: IndexedSeq[SpecialPolarNodal[Double]]) : List[(String, SpecialPolarNodal[Double])] = {
    os zip pns map diffPN
  }
    
  def calculatePnLppDiffList(os: List[CartesianPNLppUnit], pns: IndexedSeq[SpecialPolarNodal[Double]]) : List[(String, SpecialPolarNodal[Double])] = {
    os zip pns map diffPNLPP
  }
  
  def diffListPosVel(os: List[CartesianPNLppUnit], pvs: IndexedSeq[CartesianElems[Double]]) : (Double, Double) = {
            
    def max[A <% Ordered[A]](xs: Seq[A]): Option[A] = xs match {
      case s if s.isEmpty || !s.hasDefiniteSize => None
      case s if s.size == 1 => Some(s(0))
      case s if s(0) <= s(1) => max(s drop 1)
      case s => max((s drop 1).updated(0, s(0)))
    }
    val (ldp, ldv) =  os zip pvs map diffPosVel unzip

    (max(ldp).get, max(ldv).get)
  }    
  
}
