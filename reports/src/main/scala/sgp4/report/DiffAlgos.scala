package sgp4.report

import sgp4.model._

/** The differences of the final results of the Vallado long, polar nodals and Lara algorithms against Vallado's
  * original one, in cartesian elements and in polar nodals.
  */
class CompareAlgos(all: AllPropagators, tle: TestTLE) {
  val SEP = "|"
  val sat = tle.tle.satelliteNumber

  // Vallado's original algorithm is the baseline of all the comparisons
  private val others = all.algorithmNames.tail

  /** The cartesian differences of each algorithm against Vallado's, one entry per grid time.
    */
  val algosPvDiffsList = others map { algo =>
    all.finalCartesiansResults(algo).zip(all.finalCartesiansResults("va")).zipWithIndex.map { case ((c1, c0), i) =>
      val t = i * all.step
      val c = c1 - c0
      f"$SEP ${algo}%3s $SEP ${sat}%05d $SEP ${t}%5d $SEP ${c.x}% 6.4e $SEP ${c.y}% 6.4e $SEP ${c.z}% 6.4e $SEP ${c.vx}% 6.4e $SEP ${c.vy}% 6.4e $SEP ${c.vz}% 6.4e $SEP"
    }
  }

  /** The polar nodals differences of each algorithm against Vallado's, one entry per grid time.
    */
  val algosPnDiffsList = others map { algo =>
    all.finalPNResults(algo).zip(all.finalPNResults("va")).zipWithIndex.map { case ((c1, c0), i) =>
      val t = i * all.step
      val c = c1 - c0
      f"$SEP ${algo}%3s $SEP ${sat}%05d $SEP ${t}%5d  $SEP ${c.I}% 6.4e $SEP ${c.θ}% 6.4e $SEP ${c.Ω}% 6.4e $SEP ${c.r}% 6.4e $SEP ${c.R}% 6.4e $SEP ${c.`Θ/r`}% 6.4e $SEP"
    }
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

  def cartesianDiffReport() = {
    println("### Differences between cartesian final results from Vallado to the other algorithms\n")
    cartesianDiffReportHeader map println
    algosPvDiffsList map { ls => ls map { println } }
    println()
  }

  def pnDiffReport() = {
    println(
      "### Differences between final results from Vallado in polar nodals using internal units to the other algorithms\n"
    )
    pnDiffReportHeader map println
    algosPnDiffsList map { ls => ls map { println } }
    println()
  }
}

/** Writes the cartesian and polar nodals differences against Vallado's algorithm of the test TLEs into gnuplot data
  * files under doc/reports.
  */
object ReportAlgorithmsWithTestNearTLEs extends App {
  import better.files._
  import better.files.Dsl._
  def allComp(t: TestTLE) = new CompareAlgos(AllPropagators(t), t)
  val tles = List(TLE00005)
  val comps = tles map allComp

  val pnfile = file"doc/reports/algodiffpn.out"
  pnfile.overwrite("")
  comps map { _.algosPnDiffsList map { ls => ls map { _ >>: pnfile } } }
  val pvfile = file"doc/reports/algodiffcartesians.out"
  pvfile.overwrite("")
  comps map { _.algosPvDiffsList map { ls => ls map { _ >>: pvfile } } }
}
