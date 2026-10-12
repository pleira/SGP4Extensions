package sgp4
package model

import org.scalactic.Or
import sgp4.coord.ErrorMessage
import sgp4.propagation._
import sgp4.earth_gravity.SGPConstants
import sgp4.io.TLE
import spire.algebra._

/** Common structure of the SGP4 propagator companion objects: construction from an already built secular corrections
  * model, or from a TLE via BrouwerLaneSecularCorrections.fromTLE, which can fail.
  */
trait SGP4Companion[Alg[_]] {

  def apply[F: Field: NRoot: Order: Trig: TruncatedDivision](sec: BrouwerLaneSecularCorrections[F]): Alg[F]

  def fromTLE[F: Field: NRoot: Order: Trig: TruncatedDivision](
      tle: TLE,
      wgs: SGPConstants[F]
  ): Alg[F] Or ErrorMessage =
    BrouwerLaneSecularCorrections.fromTLE(tle, wgs).map(apply(_))
}
