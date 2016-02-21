package predict4s.report

import predict4s.sgp._
import predict4s.coord._
import predict4s.sgp._
import spire.implicits._
import spire.math._


class CompareAlgos(all: AllAlgos, tle: TestTLE) {
  val algos = all.algorithmNames // List("va", "vl", "pn", "la") in this order
  val SEP = "|"
  val sat = tle.tle.satelliteNumber
  val pvs = all.finalCartesiansResults
  val pns = all.finalPNResults
  
//  def cartesianMaxDiffList = maxs map {p => f"$SEP ${sat}%d $SEP ${p._1}%6.4e $SEP ${p._2}%6.4e $SEP" }
//  def algosCartesianMaxDiffList = almaxs map {p => f"$SEP ${p._1}%3s $SEP ${sat}%05d  $SEP ${p._2._1}%6.4e $SEP${p._2._2}%6.4e $SEP" }
  // differences with the vallado algorithm, pvs has (t, c)
  val pvDiffs = pvs.tail map { pvl =>  pvl zip pvs(0) map {p => p._1 - p._2 } }
  val algosPvDiffs = algos.tail zip pvDiffs
  val algosPvDiffsList = algosPvDiffs map {p =>
    val algo = p._1
    val list = p._2
    val ls = list.zipWithIndex.map {case(c,i) =>
      val t = i*all.step
      f"$SEP ${algo}%3s $SEP ${sat}%05d $SEP ${t}%5d $SEP ${c.x}% 6.4e $SEP ${c.y}% 6.4e $SEP ${c.z}% 6.4e $SEP ${c.vx}% 6.4e $SEP ${c.vy}% 6.4e $SEP ${c.vz}% 6.4e $SEP"
    }
    ls
  }
  val pnDiffs = pns.tail map { pnl => pnl zip pns(0) map {p => p._1 - p._2 } }
  val algosPnDiffs = algos.tail zip pnDiffs
  // TODO: applicative ?
  val algosPnDiffsList = algosPnDiffs map {p =>
    val algo = p._1
    val list = p._2
    val ls = list.zipWithIndex.map {case(c,i) =>
      val t = i*all.step    
      f"$SEP ${algo}%3s $SEP ${sat}%05d $SEP ${t}%5d  $SEP ${c.I}% 6.4e $SEP ${c.θ}% 6.4e $SEP ${c.Ω}% 6.4e $SEP ${c.r}% 6.4e $SEP ${c.R}% 6.4e $SEP ${c.`Θ/r`}% 6.4e $SEP"
    }
    ls
  }
  
  def cartesianDiffReportHeader = 
    List(s"$SEP Algo $SEP TLE $SEP time (min) $SEP dx (km) $SEP dy (km) $SEP dz (km) $SEP dvx (km/s) $SEP dvy (km/s) $SEP dvz (km/s) $SEP",    
         s"$SEP ---- $SEP --- $SEP ---------- $SEP ------- $SEP ------- $SEP ------- $SEP ---------- $SEP ---------- $SEP ---------- $SEP")
  
  def pnDiffReportHeader = 
    List(s"$SEP Algo $SEP TLE $SEP time (min) $SEP dIncl (rad) $SEP dθ (rad) $SEP dΩ (rad) $SEP     dr     $SEP      dR    $SEP   d Θ/r    $SEP",    
         s"$SEP ---- $SEP --- $SEP ---------- $SEP ----------- $SEP -------- $SEP -------- $SEP ---------- $SEP ---------- $SEP ---------- $SEP")

    
  def cartesianDiffReport() = {
    println("### Differences between cartesian final results from SGP4Vallado to the other algorithms\n")
    cartesianDiffReportHeader map println
    algosPvDiffsList map { ls => ls map { println } }
    println
  }
  
  def pnDiffReport() = {
    println("### Differences between final results from SGP4Vallado in polar nodals using internal units to the other algorithms\n")
    pnDiffReportHeader map println
    algosPnDiffsList map { ls => ls map { println } }
    println
  }
    
}

object ReportAlgorithmsWithTestNearTLEs extends App {
  import better.files._
  import java.io.{File => JFile}
  def allComp(t: TestTLE) = new CompareAlgos(AllAlgos(t), t)
  val tles = List(TLE00005) // , TLE06251, TLE28057, TLE29238, TLE29141)
  val comps = tles map allComp

  //  comps map { _.cartesianDiffReport }
//  val cdifffile = file".doc/hcartesiandiff.md"
//  cdifffile.overwrite("")
//  "### Differences between cartesian final results from SGP4Vallado to the algorithms\n" >>: cdifffile
//  comps map { cls => 
//    cls.cartesianDiffReportHeader map { _ >>: cdifffile }
//    cls.algosPvDiffsList map { ls => ls map {_ >>: cdifffile }}
//  }
//  
////  comps map { _.pnDiffReport }
//  val pndifffile = file".doc/hpndiff.md"
//  pndifffile.overwrite("")
//  "### Differences between final results from SGP4Vallado in polar nodals using internal units to the algorithms\n" >>: pndifffile
//  comps map { cls => 
//        "\n" >>: pndifffile
//    cls.pnDiffReportHeader map { _ >>: pndifffile }
//    cls.algosPnDiffsList map { ls => ls map {_ >>: pndifffile }}
//  }

  // GNUPLOT
  
  val file = file"gnuplot/algodiffpn.out"
  file.overwrite("")
  comps map { _.algosPnDiffsList map { ls => ls map { _ >>: file } } }
  val pvfile = file"gnuplot/algodiffcartesians.out"
  pvfile.overwrite("")
  comps map { _.algosPvDiffsList map { ls => ls map { _ >>: pvfile } } }
  
  // COMPARISON VL with PN
  // comps map {_.vlpnFinalPNDiffReport }
//  val vlpndifffile = file".doc/hvlpndiff.md"
//  vlpndifffile.overwrite("")
//  "### Differences between final polar nodals results from Vallado Long and Polar Nodals algorithms using internal units\n" >>: vlpndifffile
//  comps map { cls => 
//    "\n" >>: vlpndifffile
//    cls.vlpnFinalPNDiffReportHeader map { _ >>: vlpndifffile }
//    cls.vlpnPnDiffsList map { _ >>: vlpndifffile }
//  }
  //comps map {_.vlpnLppPNDiffReport }  
//  val vlpnlppdifffile = file".doc/hvlpnlppdiff.out"
//  vlpnlppdifffile.overwrite("")
//  "### Differences between long period periodic corrections from Vallado in polar nodals using internal units to the algorithms\n" >>: vlpnlppdifffile
//  comps map { cls => cls.pnDiffReportHeader map { _ >>: vlpnlppdifffile }}
//  comps map { cls => cls.algosPnLppDiffsList map { ls => ls map {_ >>: vlpnlppdifffile }}}


}

