package sgp4

import org.scalactic.Or
import sgp4.coord.SGP4Error
import sgp4.earth_gravity.SGP721Constants
import sgp4.earth_gravity.SGP72Constants
import sgp4.earth_gravity.SGP84Constants
import sgp4.earth_gravity.SGPConstants
import sgp4.io.TLE
import sgp4.model.SGP4
import sgp4.model.SGP4Lara
import sgp4.model.SGP4PN
import sgp4.model.SGP4ValladoLong

/** The parsing of the command line of the sgp4.Main program. The options select the propagation model and the
  * propagation times; the remaining arguments are the TLEs, as inline two line element sets and/or TLE file names. The
  * parsing is a pure function of the arguments, so that it is fully testable without running the program.
  */
object Cli:

  import spire.std.any.DoubleAlgebra

  /** The SGP4 propagation models selectable from the command line.
    */
  enum PropagationModel(val option: String, val name: String):

    /** Builds the propagator of the selected model from the TLE.
      */
    def fromTLE(tle: TLE, wgs: SGPConstants[Double]): SGP4[Double] Or SGP4Error = this match
      case Vallado     => SGP4.fromTLE(tle, wgs)
      case ValladoLong => SGP4ValladoLong.fromTLE(tle, wgs)
      case PolarNodals => SGP4PN.fromTLE(tle, wgs)
      case Lara        => SGP4Lara.fromTLE(tle, wgs)

    case Vallado extends PropagationModel("-v", "Vallado SGP4")
    case ValladoLong extends PropagationModel("-vl", "Vallado-Lyddane LONG SGP4")
    case PolarNodals extends PropagationModel("-p", "PN SGP4")
    case Lara extends PropagationModel("-l", "Lara SGP4")

  /** The selectable models, in help order.
    */
  val models: List[PropagationModel] = PropagationModel.values.toList

  /** The Earth gravity models selectable from the command line, named as in the python sgp4 library's earth_gravity
    * module.
    */
  enum GravityModel(val name: String):

    /** The constants of the selected gravity model.
      */
    def wgs: SGPConstants[Double] = this match
      case WGS72  => SGP72Constants.tleDoubleConstants
      case WGS721 => SGP721Constants.tleDoubleConstants
      case WGS84  => SGP84Constants.tleDoubleConstants

    case WGS72 extends GravityModel("wgs72")
    case WGS721 extends GravityModel("wgs72old")
    case WGS84 extends GravityModel("wgs84")

  /** The selectable gravity models, in help order.
    */
  val gravityModels: List[GravityModel] = GravityModel.values.toList

  /** The default propagation times: one day, from 0 to 24*60 min, by 1 min steps.
    */
  val defaultTimes: Seq[Double] = for i <- 0 to 24 * 60; t = i * 1.0 yield t

  /** A successfully parsed command line: the selected propagation model, the inline TLE lines, the TLE files and the
    * propagation times in minutes from the TLE epoch.
    */
  final case class Config(
      model: PropagationModel,
      gravity: GravityModel,
      tleLines: List[String],
      files: List[String],
      times: Seq[Double]
  )

  /** The result of parsing the command line: the configuration, or a request for the help message, or an error.
    */
  enum Parsed:
    case Help
    case Ok(config: Config)
    case Error(message: String)

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
        |  -g <model>    Earth gravity model: wgs72 (default), wgs72old or wgs84
        |
        |Each inline TLE is propagated at the given times, printing the propagated
        |position (km) and velocity (km/s) in TEME coordinates. Each satellite of a file
        |gets one summary line instead, with the state at the first time and the count of
        |times propagated; satellites that cannot be propagated (deep space TLEs, with
        |period >= 225 min, are not supported) are reported and skipped.
        |
        |The propagation model used is the one selected with the options above: Vallado's
        |original algorithm by default (sgp4.model.SGP4Vallado). The Earth gravity model
        |used is the one selected with the -g option, wgs72 by default.
        |""".stripMargin

  /** Each argument is a TLE line or a TLE file name, in that order of precedence.
    */
  private def isFile(arg: String): Boolean = new java.io.File(arg).isFile

  private def isTLELine(arg: String): Boolean = arg.startsWith("1 ") || arg.startsWith("2 ")

  /** Extracts an option and the value that follows it. Returns the value, the remaining arguments and an error if the
    * option is given more than once or is missing its value.
    */
  private def extractValue(
      args: List[String],
      option: String,
      valueName: String
  ): (Option[String], List[String], Option[String]) =
    val positions = args.zipWithIndex.collect { case (`option`, i) => i }
    if positions.isEmpty then (None, args, None)
    else if positions.length > 1 then (None, args, Some(s"Error: at most one $option option"))
    else
      val i = positions.head
      if i + 1 >= args.length then (None, args, Some(s"Error: the $option option needs $valueName as its value"))
      else (Some(args(i + 1)), args.patch(i, Nil, 2), None)

  /** Parses the value of the -t option: a comma separated list of minutes from the TLE epoch, the negative ones
    * propagating back from the epoch.
    */
  private def parseTimes(value: String): Option[Seq[Double]] =
    val fields = value.split(",").map(_.trim)
    if fields.exists(_.isEmpty) then None
    else
      scala.util
        .Try(fields.map(_.toDouble))
        .toOption
        .filter(times => times.forall(t => !t.isInfinite && !t.isNaN))
        .map(_.toIndexedSeq)

  /** Parses the command line into a configuration for the program, a request for the help message, or an error.
    */
  def parse(args: Array[String]): Parsed =
    if args.exists(a => a == "-h" || a == "--help") then Parsed.Help
    else
      val (timesValue, others0, timesArgError) = extractValue(args.toList, "-t", "the propagation times")
      val (gravityValue, others, gravityArgError) = extractValue(others0, "-g", "the gravity model name")
      val flags = others filter (_.startsWith("-"))
      val rest = others filterNot (_.startsWith("-"))
      val modelFlags = models filter (m => flags contains m.option)
      val unknownFlags = flags filterNot (f => models.exists(_.option == f))
      val parsedTimes = timesValue.flatMap(parseTimes)
      val gravityModel = gravityModels.find(_.name == gravityValue.getOrElse("wgs72"))
      val files = rest filter isFile
      val tleLines = rest filter isTLELine
      val badArgs = rest filterNot (a => isFile(a) || isTLELine(a))
      if timesArgError.isDefined then Parsed.Error(timesArgError.get)
      else if gravityArgError.isDefined then Parsed.Error(gravityArgError.get)
      else if unknownFlags.nonEmpty then Parsed.Error(s"Error: unknown option ${unknownFlags.mkString(" ")}")
      else if modelFlags.size > 1 then
        Parsed.Error(s"Error: at most one propagation model option, got ${modelFlags.map(_.option).mkString(" ")}")
      else if timesValue.isDefined && parsedTimes.isEmpty then
        Parsed.Error(s"Error: invalid propagation times '${timesValue.get}'")
      else if gravityValue.isDefined && gravityModel.isEmpty then
        Parsed.Error(
          s"Error: unknown gravity model '${gravityValue.get}' (available: ${gravityModels.map(_.name).mkString(", ")})"
        )
      else if badArgs.nonEmpty then
        Parsed.Error(
          s"Error: not a TLE file or TLE line: ${badArgs.mkString(" ")} (propagation times are given with the -t option)"
        )
      else if files.isEmpty && tleLines.length < 2 then
        if args.isEmpty then Parsed.Help
        else Parsed.Error("Error: no TLE to propagate: give a two line element set or TLE file names")
      else if tleLines.nonEmpty && tleLines.length % 2 != 0 then
        Parsed.Error("Error: the inline TLE lines must come in pairs, line 1 followed by line 2")
      else
        Parsed.Ok(
          Config(
            modelFlags.headOption.getOrElse(PropagationModel.Vallado),
            gravityModel.get,
            tleLines.toList,
            files.toList,
            parsedTimes.getOrElse(defaultTimes)
          )
        )
