package sgp4

import java.nio.file.Files
import org.scalatest.funsuite.AnyFunSuite

/** The parsing rules of the sgp4.Main command line, one scenario per test.
  */
class CliSpec extends AnyFunSuite {

  import Cli.Parsed

  private val tleLine1 = "1 00005U 58002B   00179.78495062  .00000023  00000-0  28098-4 0  4753"
  private val tleLine2 =
    "2 00005  34.2682 348.7242 1859667 331.7664  19.3264 10.82419157413667     0.00      4320.0        360.00"

  private def parseOk(args: String*): Cli.Config =
    Cli.parse(args.toArray) match {
      case Parsed.Ok(config)     => config
      case Parsed.Error(message) => fail(s"expected a parsed command line, got error: $message")
      case Parsed.Help           => fail("expected a parsed command line, got the help request")
    }

  private def parseError(args: String*): String =
    Cli.parse(args.toArray) match {
      case Parsed.Error(message) => message
      case Parsed.Ok(config)     => fail(s"expected an error, got the parsed command line $config")
      case Parsed.Help           => fail("expected an error, got the help request")
    }

  test("parse: no arguments requests the help message") {
    assert(Cli.parse(Array()) == Parsed.Help)
  }

  test("parse: -h and --help request the help message") {
    assert(Cli.parse(Array("-h")) == Parsed.Help)
    assert(Cli.parse(Array("--help")) == Parsed.Help)
  }

  test("parse: arguments but no TLE are an error pointing at the TLE inputs") {
    assert(parseError("-l").contains("no TLE to propagate"))
  }

  test("parse: an unknown option is an error") {
    assert(parseError("-x", tleLine1, tleLine2).contains("unknown option -x"))
  }

  test("parse: at most one propagation model option") {
    assert(parseError("-l", "-p", tleLine1, tleLine2).contains("at most one propagation model option"))
  }

  test("parse: the default propagation model is Vallado's") {
    assert(parseOk(tleLine1, tleLine2).model == Cli.Vallado)
  }

  test("parse: -l selects Lara's propagator") {
    assert(parseOk("-l", tleLine1, tleLine2).model == Cli.Lara)
  }

  test("parse: -t needs the propagation times as its value") {
    assert(parseError("-t").contains("needs the propagation times"))
  }

  test("parse: at most one -t option") {
    assert(parseError("-t", "0", "-t", "720", tleLine1, tleLine2).contains("at most one -t"))
  }

  test("parse: invalid propagation times are an error") {
    assert(parseError("-t", "abc", tleLine1, tleLine2).contains("invalid propagation times"))
    assert(parseError("-t", "0,,720", tleLine1, tleLine2).contains("invalid propagation times"))
    assert(parseError("-t", "NaN", tleLine1, tleLine2).contains("invalid propagation times"))
  }

  test("parse: propagation times are comma separated, negative ones back from the epoch") {
    assert(parseOk("-t", "0,720,-60", tleLine1, tleLine2).times == IndexedSeq(0.0, 720.0, -60.0))
  }

  test("parse: without -t the default times are one day, every minute") {
    val times = parseOk(tleLine1, tleLine2).times
    assert(times.size == 24 * 60 + 1)
    assert(times.head == 0.0)
    assert(times.last == 24 * 60.toDouble)
  }

  test("parse: an inline two line element set is recognized by its line prefixes") {
    val config = parseOk("-t", "0", tleLine1, tleLine2)
    assert(config.tleLines == List(tleLine1, tleLine2))
    assert(config.files.isEmpty)
  }

  test("parse: an existing file name is a TLE file input") {
    val file = Files.createTempFile("sgp4-cli-spec", ".txt").toFile
    try {
      val config = parseOk("-t", "0", file.getPath)
      assert(config.files == List(file.getPath))
      assert(config.tleLines.isEmpty)
    } finally file.delete()
  }

  test("parse: an odd number of inline TLE lines is an error") {
    val file = Files.createTempFile("sgp4-cli-spec", ".txt").toFile
    try assert(parseError("-t", "0", file.getPath, tleLine1).contains("must come in pairs"))
    finally file.delete()
  }

  test("parse: bare numbers are an error pointing at the -t option") {
    assert(parseError(tleLine1, tleLine2, "0", "720").contains("-t"))
  }

  test("parse: the default gravity model is wgs72") {
    assert(parseOk(tleLine1, tleLine2).gravity == Cli.WGS72)
  }

  test("parse: -g selects the gravity model") {
    assert(parseOk("-g", "wgs84", tleLine1, tleLine2).gravity == Cli.WGS84)
    assert(parseOk("-g", "wgs72old", tleLine1, tleLine2).gravity == Cli.WGS721)
  }

  test("parse: an unknown gravity model is an error listing the available ones") {
    assert(parseError("-g", "wgs42", tleLine1, tleLine2).contains("unknown gravity model 'wgs42'"))
    assert(parseError("-g", "wgs42", tleLine1, tleLine2).contains("wgs72, wgs72old, wgs84"))
  }

  test("parse: -g needs the gravity model name as its value") {
    assert(parseError("-g").contains("needs the gravity model name"))
  }

  test("parse: at most one -g option") {
    assert(parseError("-g", "wgs72", "-g", "wgs84", tleLine1, tleLine2).contains("at most one -g"))
  }
}
