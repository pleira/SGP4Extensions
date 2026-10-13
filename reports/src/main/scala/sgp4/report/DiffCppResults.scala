package sgp4.report

import sgp4.model.*

/** The differences of the final results of all the algorithms against the cpp reference results of Vallado's
  * implementation, in cartesian elements and in polar nodals.
  */
class CompareAll(all: AllPropagators, tle: TestTLE) extends LoadCppResults with DiffResults:
  val SEP = "|"
  val sat = tle.tle.satelliteNumber
  val cppOuts = loadCppOutputData(sat)
  val pvs = all.finalCartesiansResults
  val pns = all.finalPNResults

  /** The maximum cartesian differences of each algorithm against the cpp results.
    */
  def algosCartesianMaxDiffList = all.algorithmNames map { algo =>
    val (maxdp, maxdv) = diffListPosVel(cppOuts, pvs(algo))
    f"$SEP ${algo}%3s $SEP ${sat}%05d  $SEP ${maxdp}%6.4e $SEP${maxdv}%6.4e $SEP"
  }

  /** The cartesian differences of each algorithm against the cpp results, one entry per cpp result time.
    */
  val algosPvDiffsList = all.algorithmNames map { algo =>
    calculateDiffList(cppOuts, pvs(algo)) map { tc =>
      val t: Int = tc._1.toDouble.toInt
      val c = tc._2
      f"$SEP ${algo}%3s $SEP ${sat}%05d  $SEP ${t}%5d $SEP ${c.x}% 6.4e $SEP ${c.y}% 6.4e $SEP ${c.z}% 6.4e $SEP ${c.vx}% 6.4e $SEP ${c.vy}% 6.4e $SEP ${c.vz}% 6.4e $SEP"
    }
  }

  /** The polar nodals differences of each algorithm against the cpp results, one entry per cpp result time.
    */
  val algosPnDiffsList = all.algorithmNames map { algo =>
    calculatePnDiffList(cppOuts, pns(algo)) map { tc =>
      val t: Int = tc._1.toDouble.toInt
      val c = tc._2
      f"$SEP ${algo}%3s $SEP ${sat}%05d  $SEP ${t}%5d $SEP ${c.I}% 6.4e $SEP ${c.θ}% 6.4e $SEP ${c.Ω}% 6.4e $SEP ${c.r}% 6.4e $SEP ${c.R}% 6.4e $SEP ${c.`Θ/r`}% 6.4e $SEP"
    }
  }

  /** The polar nodals differences between the Vallado long and the polar nodals algorithms, with internal units.
    */
  val vlpnPnDiffsList = pns("vl").zip(pns("pn")).map { case (vl, pn) =>
    val c = vl - pn
    f"$SEP ${c.I}% 6.4e $SEP ${c.θ}% 6.4e $SEP ${c.Ω}% 6.4e $SEP ${c.r}% 6.4e $SEP ${c.R}% 6.4e $SEP ${c.`Θ/r`}% 6.4e $SEP"
  }

  def cartesianDiffReportHeader =
    List(
      s"$SEP Algo $SEP TLE $SEP time (min) $SEP dx (km) $SEP dy (km) $SEP dz (km) $SEP dvx (km/s) $SEP dvy (km/s) $SEP dvz (km/s) $SEP",
      s"$SEP ---- $SEP --- $SEP ---------- $SEP ------- $SEP ------- $SEP ------- $SEP ---------- $SEP ---------- $SEP ---------- $SEP"
    )

  def pnDiffReportHeader =
    List(
      s"$SEP Algo $SEP TLE $SEP time (min) $SEP dIncl (rad) $SEP dθ (rad) $SEP dΩ (rad) $SEP     dr     $SEP      dR    $SEP   d Θ/r    $SEP",
      s"$SEP ---- $SEP --- $SEP ---------- $SEP ----------- $SEP -------- $SEP -------- $SEP ---------- $SEP ---------- $SEP ---------- $SEP"
    )

  def vlpnFinalPNDiffReportHeader =
    List(
      s"$SEP dIncl (rad) $SEP dθ (rad) $SEP dΩ (rad) $SEP     dr     $SEP      dR    $SEP   d Θ/r    $SEP",
      s"$SEP ----------- $SEP -------- $SEP -------- $SEP ---------- $SEP ---------- $SEP ---------- $SEP"
    )

  def cartesianMaxDiffReportHeader =
    List(
      s"$SEP Algo $SEP TLE $SEP Max diff pos (km) $SEP Max diff vel (km/s) $SEP",
      s"$SEP ---- $SEP --- $SEP ----------------- $SEP ------------------- $SEP"
    )

  def cartesianMaxDiffReport() =
    println("### Maximum Differences between final results in cartesian elements from Vallado to the algorithms\n")
    cartesianMaxDiffReportHeader map println
    algosCartesianMaxDiffList map println
    println()

  def cartesianDiffReport() =
    println("### Differences between cartesian final results from Vallado to the algorithms\n")
    cartesianDiffReportHeader map println
    algosPvDiffsList map { ls => ls map { println } }
    println()

  def pnDiffReport() =
    println(
      "### Differences between final results from Vallado in polar nodals using internal units to the algorithms\n"
    )
    pnDiffReportHeader map println
    algosPnDiffsList map { ls => ls map { println } }
    println()

  def vlpnFinalPNDiffReport() =
    println(
      "### Differences between final polar nodals results from Vallado Long and Polar Nodals algorithms using internal units\n"
    )
    vlpnFinalPNDiffReportHeader map println
    vlpnPnDiffsList map println
    println()

/** Writes the differences against the cpp reference results of the test TLEs into markdown and gnuplot data files under
  * doc/reports.
  */
object ReportTestNearTLEs:
  import better.files._
  import better.files.Dsl._
  def allComp(t: TestTLE) = new CompareAll(AllPropagators(t), t)
  val tles = List(TLE00005) // , TLE06251, TLE28057, TLE29238, TLE29141)
  val comps = tles map allComp

  val cdifffile = file"doc/reports/hcartesiandiff.md"
  cdifffile.overwrite("")
  "### Differences between cartesian final results from Vallado to the algorithms\n" >>: cdifffile
  comps map { cls =>
    cls.cartesianDiffReportHeader map { _ >>: cdifffile }
    cls.algosPvDiffsList map { ls => ls map { _ >>: cdifffile } }
  }

  val pndifffile = file"doc/reports/hpndiff.md"
  pndifffile.overwrite("")
  "### Differences between final results from Vallado in polar nodals using internal units to the algorithms\n" >>: pndifffile
  comps map { cls =>
    "\n" >>: pndifffile
    cls.pnDiffReportHeader map { _ >>: pndifffile }
    cls.algosPnDiffsList map { ls => ls map { _ >>: pndifffile } }
  }

  // GNUPLOT
  val pnfile = file"doc/reports/diffpn.out"
  pnfile.overwrite("")
  comps map { _.algosPnDiffsList map { ls => ls map { _ >>: pnfile } } }
  val pvfile = file"doc/reports/diffcartesians.out"
  pvfile.overwrite("")
  comps map { _.algosPvDiffsList map { ls => ls map { _ >>: pvfile } } }

  // COMPARISON VL with PN
  val vlpndifffile = file"doc/reports/hvlpndiff.md"
  vlpndifffile.overwrite("")
  "### Differences between final polar nodals results from Vallado Long and Polar Nodals algorithms using internal units\n" >>: vlpndifffile
  comps map { cls =>
    "\n" >>: vlpndifffile
    cls.vlpnFinalPNDiffReportHeader map { _ >>: vlpndifffile }
    cls.vlpnPnDiffsList map { _ >>: vlpndifffile }
  }

  /** Runs the program: the statements above execute at object initialization, when the JVM launcher accesses this main
    * method.
    */
  def main(args: Array[String]): Unit = ()
