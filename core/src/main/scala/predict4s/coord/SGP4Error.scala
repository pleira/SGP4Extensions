package predict4s
package coord

/** The failure type used across the SGP4 propagation pipeline, replacing the former plain-string errors. Each case is
  * matchable and carries the values involved; `message` keeps the human-readable rendering used in reports.
  */
sealed trait SGP4Error extends Product with Serializable {
  def message: String
  override def toString: String = message
}

object SGP4Error {

  final case class NegativeEccentricity[F](e: F) extends SGP4Error {
    def message: String = s"Problem with eccentricity $e"
  }

  final case class NegativeSemilatusRectum[F](p: F) extends SGP4Error {
    def message: String = s"lyddane2SpecialPolarNodal: Problem with semilatus rectum p: $p"
  }

  final case class SecularEccentricityOutOfRange[F](em: F) extends SGP4Error {
    def message: String = s"Secular eccentricity $em outside valid range"
  }

  final case class KeplerNoConvergence[F](incr: F, E: F) extends SGP4Error {
    def message: String = s"No convergence while solving Kepler equation: incr=$incr, E=$E"
  }

  final case class LyddaneKeplerNoConvergence[F](incr: F, U: F) extends SGP4Error {
    def message: String = s"No convergence while solving Kepler equation: incr=$incr, U=$U"
  }
}
