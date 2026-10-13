package sgp4

import org.scalactic.Or
import sgp4.coord.ErrorMessage
import sgp4.coord.SGPPropResult
import sgp4.earth_gravity.SGP72Constants
import sgp4.earth_gravity.SGPConstants
import sgp4.io.TLE
import sgp4.model.SGP4
import sgp4.model.SGP4Lara
import sgp4.model.SGP4PN
import sgp4.model.SGP4ValladoLong

/** Command line program of the SGP4 propagator: it propagates a two line element set given as arguments, or the TLEs of
  * the files given as arguments, with the propagation model selected by the -v, -vl, -p or -l option, defaulting to the
  * standard sgp4.model.SGP4 entry point, Vallado's original algorithm. The propagation times are given with the -t
  * option as minutes from the TLE epoch, comma separated; by default it propagates one day, from 0 to 24*60 min, every
  * 1 min, printing the propagated position and velocity to the standard output. Run without arguments or with --help it
  * prints a help message that shows the propagation model being used.
  */
object Main {

  import spire.std.any.DoubleAlgebra

  /** The SGP4 propagation models selectable from the command line.
    */
  private sealed trait PropagationModel {
    def option: String
    def name: String
    def fromTLE(tle: TLE, wgs: SGPConstants[Double]): SGP4[Double] Or ErrorMessage
  }

  private case object Vallado extends PropagationModel {
    val option: String = "-v"
    val name: String = "Vallado SGP4"
    def fromTLE(tle: TLE, wgs: SGPConstants[Double]): SGP4[Double] Or ErrorMessage = SGP4.fromTLE(tle, wgs)
  }

  private case object ValladoLong extends PropagationModel {
    val option: String = "-vl"
    val name: String = "Vallado-Lyddane LONG SGP4"
    def fromTLE(tle: TLE, wgs: SGPConstants[Double]): SGP4[Double] Or ErrorMessage =
      SGP4ValladoLong.fromTLE(tle, wgs)
  }

  private case object PolarNodals extends PropagationModel {
    val option: String = "-p"
    val name: String = "PN SGP4"
    def fromTLE(tle: TLE, wgs: SGPConstants[Double]): SGP4[Double] Or ErrorMessage = SGP4PN.fromTLE(tle, wgs)
  }

  private case object Lara extends PropagationModel {
    val option: String = "-l"
    val name: String = "Lara SGP4"
    def fromTLE(tle: TLE, wgs: SGPConstants[Double]): SGP4[Double] Or ErrorMessage = SGP4Lara.fromTLE(tle, wgs)
  }

  private val models: List[PropagationModel] = List(Vallado, ValladoLong, PolarNodals, Lara)

  def helpMessage: String =
    s"""|Usage: sbt "reports/runMain sgp4.Main [options] [TLE line 1] [TLE line 2] [TLE files ...]"
        |
        |Each argument is one of:
        |  an existing file name    a TLE file: any number of them, each with one or many
        |                           TLEs, in the 2 line or the 3 line (with name) format;
        |                           '#' comments and blank lines are ignored
        |  a line starting with "1 " or "2 "    an inline two line element set
        |
        |Options:
        |  -h, --help    print this help message and exit
        |  -v            Vallado SGP4, the standard entry point (default)
        |  -vl           Vallado-Lyddane LONG SGP4, with the extra long period terms
        |  -p            PN SGP4, long period corrections computed in polar nodals
        |  -l            Lara SGP4, first order corrections in Lara's non-singular variables
        |  -t <times>    propagation times in minutes from the TLE epoch, comma separated,
        |                e.g. -t 0,720,1440; negative times propagate back from the epoch.
        |                Default: from 0 to 1440 min, every 1 min
        |
        |Each inline TLE is propagated at the given times, printing the propagated
        |position (km) and velocity (km/s) in TEME coordinates. Each satellite of a file
        |gets one summary line instead, with the state at the first time and the count of
        |times propagated; satellites that cannot be propagated (deep space TLEs, with
        |period >= 225 min, are not supported) are reported and skipped.
        |
        |The propagation model used is the one selected with the options above: Vallado's
        |original algorithm by default (sgp4.model.SGP4Vallado).
        |Earth gravity model: WGS-72 low precision constants
        |""".stripMargin

  /** Each argument is a TLE line or a TLE file name, in that order of precedence.
    */
  private def isFile(arg: String): Boolean = new java.io.File(arg).isFile

  private def isTLELine(arg: String): Boolean = arg.startsWith("1 ") || arg.startsWith("2 ")

  /** The default propagation times: one day, from 0 to 24*60 min, by 1 min steps.
    */
  private val defaultTimes: Seq[Double] = for (i <- 0 to 24 * 60; t = i * 1.0) yield t

  /** Extracts the -t option and the value that follows it. Returns the value, the remaining arguments and an error if
    * -t is given more than once or is missing its value.
    */
  private def extractTimes(args: List[String]): (Option[String], List[String], Option[String]) = {
    val positions = args.zipWithIndex.collect { case ("-t", i) => i }
    if (positions.isEmpty) (None, args, None)
    else if (positions.length > 1) (None, args, Some("Error: at most one -t option"))
    else {
      val i = positions.head
      if (i + 1 >= args.length)
        (None, args, Some("Error: the -t option needs the propagation times as its value"))
      else (Some(args(i + 1)), args.patch(i, Nil, 2), None)
    }
  }

  /** Parses the value of the -t option: a comma separated list of minutes from the TLE epoch, the negative ones
    * propagating back from the epoch.
    */
  private def parseTimes(value: String): Option[Seq[Double]] = {
    val fields = value.split(",").map(_.trim)
    if (fields.exists(_.isEmpty)) None
    else
      scala.util
        .Try(fields.map(_.toDouble))
        .toOption
        .filter(times => times.forall(t => !t.isInfinite && !t.isNaN))
        .map(_.toIndexedSeq)
  }

  def temeState(t: Double, pv: SGPPropResult[Double]): String = {
    val (c, _, _) = pv.get
    f"TEME at t=${t}%7.1f min: pos = (${c.x}%9.3f, ${c.y}%9.3f, ${c.z}%9.3f) km, vel = (${c.vx}%7.4f, ${c.vy}%7.4f, ${c.vz}%7.4f) km/s"
  }

  def reportLine(t: Double, pv: SGPPropResult[Double]): String =
    if (pv.isGood)
      s"  ${temeState(t, pv)}"
    else
      f"  t = ${t}%7.1f min: propagation error: ${pv.swap.get}"

  /** Propagates one inline TLE, printing the full per time state report.
    */
  def propagateTLE(model: PropagationModel, tle: TLE, times: Seq[Double]): Unit = {
    val sgp4Or = model.fromTLE(tle, SGP72Constants.tleDoubleConstants)
    if (sgp4Or.isBad) Console.out.println(s"Error initializing SGP4: ${sgp4Or.swap.get}")
    else {
      Console.out.println(s"### TLE${tle.satelliteNumber}")
      Console.out.println(s"Propagation model used: ${model.name}")
      val sgp4 = sgp4Or.get
      times foreach { t => Console.out.println(reportLine(t, sgp4.propagate(t))) }
      Console.out.println("###")
    }
  }

  /** The compact, one line summary of propagating one satellite of a file: the number of times propagated, the TEME
    * state at the first one, and the first propagation error if any. Satellites that cannot be propagated at all are
    * reported as skipped.
    */
  def satelliteSummary(model: PropagationModel, tle: TLE, times: Seq[Double]): (String, Boolean) = {
    val sat = f"TLE ${tle.satelliteNumber}%05d"
    val sgp4Or = model.fromTLE(tle, SGP72Constants.tleDoubleConstants)
    if (sgp4Or.isBad) (s"  $sat: skipped: ${sgp4Or.swap.get}", false)
    else {
      val sgp4 = sgp4Or.get
      if (sgp4.sec.elem0Ctx.isDeepSpace)
        (s"  $sat: skipped: deep space TLE, period >= 225 min (SDP4 not implemented)", false)
      else {
        val results = times.map(t => (t, sgp4.propagate(t))).toList
        val good = results.filter(_._2.isGood)
        if (good.isEmpty)
          (f"  $sat: skipped: ${results.head._2.swap.get}", false)
        else {
          val firstError =
            if (good.size == results.size) ""
            else {
              val (t, pv) = results.collectFirst { case (t, pv) if pv.isBad => (t, pv) }.get
              f", first error at t=${t}%7.1f min: ${pv.swap.get}"
            }
          (
            f"  $sat: ${good.size}/${results.size} times propagated$firstError, ${temeState(good.head._1, good.head._2)}",
            true
          )
        }
      }
    }
  }

  /** Propagates all the TLEs of a file, printing one summary line per satellite and a count at the end. Satellites that
    * cannot be propagated are reported on their line and skipped.
    */
  def propagateFile(model: PropagationModel, path: String, times: Seq[Double]): Unit = {
    Console.out.println(s"### file $path, propagation model used: ${model.name}")
    val tles =
      try TLE.parseFile(path)
      catch {
        // a line pair in the file that is not a valid TLE
        case e: Exception =>
          Console.out.println(s"Error parsing the file: ${e.getMessage}")
          Nil
      }
    val summaries = tles map (tle => satelliteSummary(model, tle, times))
    summaries foreach { line => Console.out.println(line._1) }
    val propagated = summaries.count(_._2)
    Console.out.println(s"### ${tles.size} TLEs, $propagated propagated, ${tles.size - propagated} skipped")
  }

  def usageError(msg: String): Unit = {
    Console.out.println(msg)
    Console.out.println("Run with --help for usage information")
  }

  def main(args: Array[String]): Unit =
    if (args.exists(a => a == "-h" || a == "--help"))
      Console.out.println(helpMessage)
    else {
      val (timesValue, others, timesArgError) = extractTimes(args.toList)
      val flags = others filter (_ startsWith "-")
      val rest = others filterNot (_ startsWith "-")
      val modelFlags = models filter (m => flags contains m.option)
      val unknownFlags = flags filterNot (f => models.exists(_.option == f))
      val parsedTimes = timesValue.flatMap(parseTimes)
      val files = rest filter isFile
      val tleLines = rest filter isTLELine
      val badArgs = rest filterNot (a => isFile(a) || isTLELine(a))
      if (timesArgError.isDefined)
        usageError(timesArgError.get)
      else if (unknownFlags.nonEmpty)
        usageError(s"Error: unknown option ${unknownFlags.mkString(" ")}")
      else if (modelFlags.size > 1)
        usageError(s"Error: at most one propagation model option, got ${modelFlags.map(_.option).mkString(" ")}")
      else if (timesValue.isDefined && parsedTimes.isEmpty)
        usageError(s"Error: invalid propagation times '${timesValue.get}'")
      else if (badArgs.nonEmpty)
        usageError(
          s"Error: not a TLE file or TLE line: ${badArgs.mkString(" ")} (propagation times are given with the -t option)"
        )
      else if (files.isEmpty && tleLines.length < 2) {
        Console.out.println(helpMessage)
        if (args.nonEmpty)
          Console.out.println("Error: no TLE to propagate: give a two line element set or TLE file names")
      } else if (tleLines.nonEmpty && tleLines.length % 2 != 0)
        usageError("Error: the inline TLE lines must come in pairs, line 1 followed by line 2")
      else {
        val model = modelFlags.headOption.getOrElse(Vallado)
        val times = parsedTimes.getOrElse(defaultTimes)
        try {
          TLE.parseLines(tleLines.toList) foreach (tle => propagateTLE(model, tle, times))
          files foreach (path => propagateFile(model, path, times))
        } catch {
          // user input at the program boundary: a malformed inline TLE line
          case e: Exception =>
            usageError(s"Error propagating the TLEs: ${e.getMessage}")
        }
      }
    }
}
