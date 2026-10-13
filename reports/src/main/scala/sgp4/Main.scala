package sgp4

import sgp4.coord.SGPPropResult
import sgp4.earth_gravity.SGPConstants
import sgp4.io.TLE

/** Command line program of the SGP4 propagator: it propagates a two line element set given as arguments, or the TLEs of
  * the files given as arguments, with the propagation model selected by the -v, -vl, -p or -l option, defaulting to the
  * standard sgp4.model.SGP4 entry point, Vallado's original algorithm. The propagation times are given with the -t
  * option as minutes from the TLE epoch, comma separated; by default it propagates one day, from 0 to 24*60 min, every
  * 1 min, printing the propagated position and velocity to the standard output. Run without arguments or with --help it
  * prints a help message that shows the propagation model being used.
  */
object Main {

  import Cli.Parsed
  import Cli.PropagationModel

  def temeState(t: Double, pv: SGPPropResult[Double]): String = {
    val c = pv.get.pv
    f"TEME at t=${t}%7.1f min: pos = (${c.x}%9.3f, ${c.y}%9.3f, ${c.z}%9.3f) km, vel = (${c.vx}%7.4f, ${c.vy}%7.4f, ${c.vz}%7.4f) km/s"
  }

  def reportLine(t: Double, pv: SGPPropResult[Double]): String =
    if (pv.isGood)
      s"  ${temeState(t, pv)}"
    else
      f"  t = ${t}%7.1f min: propagation error: ${pv.swap.get}"

  /** Propagates one inline TLE, printing the full per time state report.
    */
  def propagateTLE(model: PropagationModel, wgs: SGPConstants[Double], tle: TLE, times: Seq[Double]): Unit = {
    val sgp4Or = model.fromTLE(tle, wgs)
    if (sgp4Or.isBad) Console.out.println(s"Error initializing SGP4: ${sgp4Or.swap.get}")
    else {
      Console.out.println(s"### TLE${tle.satelliteNumber}")
      Console.out.println(s"Propagation model used: ${model.name}")
      val sgp4 = sgp4Or.get
      times foreach { t => Console.out.println(reportLine(t, sgp4.propagate(t))) }
      Console.out.println("###")
    }
  }

  /** The outcome of propagating one satellite of a file for the compact summary report: the summary line and whether
    * the satellite could be propagated at all.
    */
  private sealed trait SatelliteOutcome {
    def line: String
  }
  private object SatelliteOutcome {
    final case class Propagated(line: String) extends SatelliteOutcome
    final case class Skipped(line: String) extends SatelliteOutcome
  }

  /** The compact, one line summary of propagating one satellite of a file: the number of times propagated, the TEME
    * state at the first one, and the first propagation error if any. Satellites that cannot be propagated at all are
    * reported as skipped.
    */
  private def satelliteOutcome(
      model: PropagationModel,
      wgs: SGPConstants[Double],
      tle: TLE,
      times: Seq[Double]
  ): SatelliteOutcome = {
    val sat = f"TLE ${tle.satelliteNumber}%05d"
    val sgp4Or = model.fromTLE(tle, wgs)
    if (sgp4Or.isBad) SatelliteOutcome.Skipped(s"  $sat: skipped: ${sgp4Or.swap.get}")
    else {
      val sgp4 = sgp4Or.get
      val results = times.map(t => (t, sgp4.propagate(t))).toList
      val good = results.filter(_._2.isGood)
      if (good.isEmpty)
        SatelliteOutcome.Skipped(f"  $sat: skipped: ${results.head._2.swap.get}")
      else {
        val firstError =
          if (good.size == results.size) ""
          else {
            val (t, pv) = results.collectFirst { case (t, pv) if pv.isBad => (t, pv) }.get
            f", first error at t=${t}%7.1f min: ${pv.swap.get}"
          }
        SatelliteOutcome.Propagated(
          f"  $sat: ${good.size}/${results.size} times propagated$firstError, ${temeState(good.head._1, good.head._2)}"
        )
      }
    }
  }

  /** Propagates all the TLEs of a file, printing one summary line per satellite and a count at the end. Satellites that
    * cannot be propagated are reported on their line and skipped.
    */
  def propagateFile(model: PropagationModel, wgs: SGPConstants[Double], path: String, times: Seq[Double]): Unit = {
    Console.out.println(s"### file $path, propagation model used: ${model.name}")
    val tles =
      try TLE.parseFile(path)
      catch {
        // a line pair in the file that is not a valid TLE
        case e: Exception =>
          Console.out.println(s"Error parsing the file: ${e.getMessage}")
          Nil
      }
    val outcomes = tles map (tle => satelliteOutcome(model, wgs, tle, times))
    outcomes foreach { outcome => Console.out.println(outcome.line) }
    val propagated = outcomes.count {
      case _: SatelliteOutcome.Propagated => true
      case _                              => false
    }
    Console.out.println(s"### ${tles.size} TLEs, $propagated propagated, ${tles.size - propagated} skipped")
  }

  def usageError(msg: String): Unit = {
    Console.out.println(msg)
    Console.out.println("Run with --help for usage information")
  }

  def run(config: Cli.Config): Unit =
    try {
      TLE.parseLines(config.tleLines) foreach (tle => propagateTLE(config.model, config.gravity.wgs, tle, config.times))
      config.files foreach (path => propagateFile(config.model, config.gravity.wgs, path, config.times))
    } catch {
      // user input at the program boundary: a malformed inline TLE line
      case e: Exception =>
        usageError(s"Error propagating the TLEs: ${e.getMessage}")
    }

  def main(args: Array[String]): Unit =
    Cli.parse(args) match {
      case Parsed.Help           => Console.out.println(Cli.helpMessage)
      case Parsed.Error(message) => usageError(message)
      case Parsed.Ok(config)     => run(config)
    }
}
