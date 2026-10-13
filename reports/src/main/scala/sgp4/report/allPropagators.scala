package sgp4.report
import sgp4.coord._
import sgp4.earth_gravity._
import sgp4.io.TLE
import sgp4.model._
import spire.implicits._

/** Propagates a TLE with the four SGP4 algorithms over a time grid. All the results are kept keyed by the algorithm
  * short name: "va" (Vallado), "vl" (Vallado long), "pn" (polar nodals) and "la" (Lara).
  */
class AllPropagators(val tle: TLE, val wgs: SGPConstants[Double], val start: Int, val end: Int, val step: Int) {

  /** The short names of the algorithms, in the canonical order.
    */
  val algorithmNames = List("va", "vl", "pn", "la")

  /** The propagators of the algorithms, keyed by their short names. All reuse the secular corrections of the Vallado
    * one.
    */
  val sgp4s: Map[String, SGP4[Double]] = {
    val vasgp4 = SGP4Vallado.fromTLE(tle, wgs).get
    List(
      "va" -> vasgp4,
      "vl" -> SGP4ValladoLong(vasgp4.sec),
      "pn" -> SGP4PN(vasgp4.sec),
      "la" -> SGP4Lara(vasgp4.sec)
    ).toMap
  }

  /** The propagation results of each algorithm over the grid, one entry per time in minutes from the TLE epoch.
    */
  val results: Map[String, IndexedSeq[SGPPropCtx[Double]]] =
    sgp4s.map { case (algo, sgp4) =>
      algo -> (for (t <- start to end by step) yield sgp4.propagate(t).get)
    }

  /** The final TEME state vectors of each algorithm, position in km and velocity in km/s.
    */
  val finalCartesiansResults: Map[String, IndexedSeq[CartesianElems[Double]]] =
    results.map { case (algo, l) => algo -> l.map(_.pv) }

  /** The final results of each algorithm in Special Polar Nodals, with internal units.
    */
  val finalPNResults: Map[String, IndexedSeq[SpecialPolarNodal[Double]]] =
    results.map { case (algo, l) => algo -> l.map(_.spn) }

  /** The final results of each algorithm in cartesian elements with the internal unit system.
    */
  val unitCartesianPositions: Map[String, IndexedSeq[CartesianElems[Double]]] =
    results.map { case (algo, l) => algo -> l.map(_.unitPv) }
}

object AllPropagators {
  def apply(o: TestTLE) = new AllPropagators(o.tle, SGP72Constants.tleDoubleConstants, o.start, o.end, o.step)
  def createWithWGS84(o: TestTLE) =
    new AllPropagators(o.tle, SGP84Constants.tleDoubleConstants, o.start, o.end, o.step)
}
