package predict4s.report

import java.io.InputStream
import java.io.FileInputStream
import scala.io.Source
import predict4s.sgp._
import predict4s.coord._
import predict4s.sgp._
import spire.implicits._
import spire.algebra.Field
import spire.math._

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


trait LoadCppResults {
  
  def loadCppOutputData(sat: Int) : List[CartesianPNLppUnit] = {
    val path = s"/valladocpp/${sat}_wgs72.txt"
    val stream = getClass.getResourceAsStream(path)
    val cppOuts = Source.fromInputStream( stream ).getLines.filter { x => !(x.isEmpty() || x.startsWith("#")) }.toList map (l => CartesianPNLppUnit(l))
    cppOuts
  }

}

class CompareAll(all: AllPropagators, tle: TestTLE) extends LoadCppResults with DiffResults {
  val algos = List("va", "vl", "pn", "la") // this is the order
  val SEP = "|"
  val sat = tle.tle.satelliteNumber
  val cppOuts = loadCppOutputData(sat)
  val pvs = all.finalCartesiansResults
  val pns = all.finalPNResults
  val pnls = all.lppPNResults
  val ups = all.unitCartesianPositions
  
  def maxs = pvs map { pvl => diffListPosVel(cppOuts, pvl) }
  def almaxs = algos zip maxs
  def cartesianMaxDiffList = maxs map {p => f"$SEP ${sat}%d $SEP ${p._1}%6.4e $SEP ${p._2}%6.4e $SEP" }
  def algosCartesianMaxDiffList = almaxs map {p => f"$SEP ${p._1}%3s $SEP ${sat}%05d  $SEP ${p._2._1}%6.4e $SEP${p._2._2}%6.4e $SEP" }
  val pvDiffs = pvs map { pvl => calculateDiffList(cppOuts, pvl) }
  val algosPvDiffs = algos zip pvDiffs
  // TODO: applicative ?
  val algosPvDiffsList = algosPvDiffs map {p =>
    val algo = p._1
    val list = p._2
    val ls = list map { tc =>
      val t : Int = (tc._1).toDouble.toInt
      val c = tc._2
      f"$SEP ${algo}%3s $SEP ${sat}%05d  $SEP ${t}%5d $SEP ${c.x}% 6.4e $SEP ${c.y}% 6.4e $SEP ${c.z}% 6.4e $SEP ${c.vx}% 6.4e $SEP ${c.vy}% 6.4e $SEP ${c.vz}% 6.4e $SEP"
    }
    ls
  }
  val pnDiffs = pns map { pnl => calculatePnDiffList(cppOuts, pnl) }
  val algosPnDiffs = algos zip pnDiffs
  // TODO: applicative ?
  val algosPnDiffsList = algosPnDiffs map {p =>
    val algo = p._1
    val list = p._2
    val ls = list map { tc =>
      val t : Int = (tc._1).toDouble.toInt
      val c = tc._2
      f"$SEP ${algo}%3s $SEP ${sat}%05d  $SEP ${t}%5d $SEP ${c.I}% 6.4e $SEP ${c.θ}% 6.4e $SEP ${c.Ω}% 6.4e $SEP ${c.r}% 6.4e $SEP ${c.R}% 6.4e $SEP ${c.`Θ/r`}% 6.4e $SEP"
    }
    ls
  }
  val pnLppDiffs = pnls map { pnl => calculatePnLppDiffList(cppOuts, pnl) }
  val algosPnLppDiffs = algos zip pnLppDiffs
  // TODO: applicative ?
  val algosPnLppDiffsList = algosPnLppDiffs map {p =>
    val algo = p._1
    val list = p._2
    val ls = list map { tc =>
      val t : Int = (tc._1).toDouble.toInt
      val c = tc._2
      f"$SEP ${algo}%3s $SEP ${sat}%05d  $SEP ${t}%5d $SEP ${c.I}% 6.4e $SEP ${c.θ}% 6.4e $SEP ${c.Ω}% 6.4e $SEP ${c.r}% 6.4e $SEP ${c.R}% 6.4e $SEP ${c.`Θ/r`}% 6.4e $SEP"
    }
    ls
  }

  def vlpnPnDiffs: List[SpecialPolarNodal[Double]] = (pnDiffs.tail.head zip pnDiffs.tail.tail.head) map { p => p._1._2 - p._2._2 }
  def vlpnPnDiffsList = vlpnPnDiffs map { c =>
          f"$SEP ${c.I}% 6.4e $SEP ${c.θ}% 6.4e $SEP ${c.Ω}% 6.4e $SEP ${c.r}% 6.4e $SEP ${c.R}% 6.4e $SEP ${c.`Θ/r`}% 6.4e $SEP"
  }
  
  def cartesianDiffReportHeader = 
    List(s"$SEP Algo $SEP TLE $SEP time (min) $SEP dx (km) $SEP dy (km) $SEP dz (km) $SEP dvx (km/s) $SEP dvy (km/s) $SEP dvz (km/s) $SEP",    
         s"$SEP ---- $SEP --- $SEP ---------- $SEP ------- $SEP ------- $SEP ------- $SEP ---------- $SEP ---------- $SEP ---------- $SEP")
  
  def pnDiffReportHeader = 
    List(s"$SEP Algo $SEP TLE $SEP time (min) $SEP dIncl (rad) $SEP dθ (rad) $SEP dΩ (rad) $SEP     dr     $SEP      dR    $SEP   d Θ/r    $SEP",    
         s"$SEP ---- $SEP --- $SEP ---------- $SEP ----------- $SEP -------- $SEP -------- $SEP ---------- $SEP ---------- $SEP ---------- $SEP")

  
  def vlpnFinalPNDiffReportHeader = 
    List(s"$SEP dIncl (rad) $SEP dθ (rad) $SEP dΩ (rad) $SEP     dr     $SEP      dR    $SEP   d Θ/r    $SEP",    
         s"$SEP ----------- $SEP -------- $SEP -------- $SEP ---------- $SEP ---------- $SEP ---------- $SEP")

         
 def unitCartesianDiffReportHeader = 
    List(s"$SEP Algo $SEP TLE $SEP time (min) $SEP  dux  $SEP   duy   $SEP   duz   $SEP",    
         s"$SEP ---- $SEP --- $SEP ---------- $SEP ----- $SEP ------- $SEP ------- $SEP")
         
  def cartesianMaxDiffReportHeader = 
    List(s"$SEP Algo $SEP TLE $SEP Max diff pos (km) $SEP Max diff vel (km/s) $SEP",    
         s"$SEP ---- $SEP --- $SEP ----------------- $SEP ------------------- $SEP")        
//    List(s"$SEP Algo $SEP TLE $SEP Max difference $SEP Max difference $SEP",    
//         s"$SEP type $SEP num $SEP position (km)  $SEP velocity (km/s)$SEP",  
//         s"$SEP ---- $SEP --- $SEP -------------- $SEP -------------- $SEP")         
         //s"$SEP ==== $SEP === $SEP ============== $SEP ============== $SEP")    

  def printDiffPosVel(p: (Double, Double)) = {
    val maxdp = p._1 
    val maxdv = p._2 
    println(s"TLE ${sat} : Max difference position (km): $maxdp , Max difference velocity (km/s): $maxdv")    
  }
  
  def cartesianMaxDiffReport() = {
    println("### Maximum Differences between final results in cartesian elements from Vallado to the algorithms\n")
    cartesianMaxDiffReportHeader map println
    algosCartesianMaxDiffList map println
    println
  }
  
  def cartesianDiffReport() = {
    println("### Differences between cartesian final results from Vallado to the algorithms\n")
    cartesianDiffReportHeader map println
    algosPvDiffsList map { ls => ls map { println } }
    println
  }
  
  def pnDiffReport() = {
    println("### Differences between final results from Vallado in polar nodals using internal units to the algorithms\n")
    pnDiffReportHeader map println
    algosPnDiffsList map { ls => ls map { println } }
    println
  }
  
  def pnLppDiffReport() = {
    println("### Differences between long period periodic corrections from Vallado in polar nodals using internal units to the algorithms\n")
    pnDiffReportHeader map println
    algosPnLppDiffsList map { ls => ls map { println } }
    println
  }
  
  def vlpnFinalPNDiffReport() = {
    println("### Differences between final polar nodals results from Vallado Long and Polar Nodals algorithms using internal units\n")
    vlpnFinalPNDiffReportHeader map println
    vlpnPnDiffsList map println 
    println
  }  
  
}

object ReportTestNearTLEs extends App {
  import better.files._
  import java.io.{File => JFile}
  def allComp(t: TestTLE) = new CompareAll(AllPropagators(t), t)
  val tles = List(TLE00005, TLE06251, TLE28057, TLE29238, TLE29141)
  val comps = tles map allComp

  //  comps map { _.cartesianDiffReport }
  val cdifffile = file".doc/hcartesiandiff.md"
  cdifffile.overwrite("")
  "### Differences between cartesian final results from Vallado to the algorithms\n" >>: cdifffile
  comps map { cls => 
    cls.cartesianDiffReportHeader map { _ >>: cdifffile }
    cls.algosPvDiffsList map { ls => ls map {_ >>: cdifffile }}
  }
  
//  comps map { _.pnDiffReport }
  val pndifffile = file".doc/hpndiff.md"
  pndifffile.overwrite("")
  "### Differences between final results from Vallado in polar nodals using internal units to the algorithms\n" >>: pndifffile
  comps map { cls => 
        "\n" >>: pndifffile
    cls.pnDiffReportHeader map { _ >>: pndifffile }
    cls.algosPnDiffsList map { ls => ls map {_ >>: pndifffile }}
  }
  
//  comps map { _.pnLppDiffReport }
  val pnlppdifffile = file".doc/hpnlppdiff.md"
  pndifffile.overwrite("")
  "### Differences between long period periodic corrections from Vallado in polar nodals using internal units to the algorithms\n" >>: pndifffile
  comps map { cls => 
    "\n" >>: pndifffile
    cls.pnDiffReportHeader map { _ >>: pnlppdifffile }
    cls.algosPnLppDiffsList map { ls => ls map {_ >>: pnlppdifffile }}
  }
  
  //val comp5 = comps(0)
  // comp5.algosPvDiffsList map { ls => ls map { println } }

  // GNUPLOT
  
  val file = file".doc/diffpn.out"
  file.overwrite("")
  comps map { _.algosPnDiffsList map { ls => ls map { _ >>: file } } }
  val pvfile = file".doc/diffcartesians.out"
  pvfile.overwrite("")
  comps map { _.algosPvDiffsList map { ls => ls map { _ >>: pvfile } } }
  
  // COMPARISON VL with PN
  // comps map {_.vlpnFinalPNDiffReport }
  val vlpndifffile = file".doc/hvlpndiff.md"
  vlpndifffile.overwrite("")
  "### Differences between final polar nodals results from Vallado Long and Polar Nodals algorithms using internal units\n" >>: vlpndifffile
  comps map { cls => 
    "\n" >>: vlpndifffile
    cls.vlpnFinalPNDiffReportHeader map { _ >>: vlpndifffile }
    cls.vlpnPnDiffsList map { _ >>: vlpndifffile }
  }
  //comps map {_.vlpnLppPNDiffReport }  
//  val vlpnlppdifffile = file".doc/hvlpnlppdiff.out"
//  vlpnlppdifffile.overwrite("")
//  "### Differences between long period periodic corrections from Vallado in polar nodals using internal units to the algorithms\n" >>: vlpnlppdifffile
//  comps map { cls => cls.pnDiffReportHeader map { _ >>: vlpnlppdifffile }}
//  comps map { cls => cls.algosPnLppDiffsList map { ls => ls map {_ >>: vlpnlppdifffile }}}


}

