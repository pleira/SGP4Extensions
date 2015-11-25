package predict4s.tle.vallado

import spire.algebra._
import spire.math._
import spire.implicits._
import predict4s.tle._


// case class LongPeriodPeriodicState[F](axnl: F, aynl: F, xl: F, secularState: SecularState[F])


trait LongPeriodPeriodicEffects {
   
  /**
   * Brouwer long-period gravitational corrections are reformulated in Lyddane’s. 
   * At the precision of SGP4, there are only corrections for F and S. 
   * Brouwer’s theory finds trouble when evaluating the short-period
   * corrections for the lower eccentricity orbits. The trouble is artificial, and is
   * related to the singularity of Delaunay variables for circular orbits. Hence,
   * SGP4 implements a different set of elements based on Lyddane’s approach
   * which completely avoids that trouble. The Lyddane elements used are:
   *   F = ℓ + g + h, C = e*cosω, S = e*sinω, a, I, h
   */
  def calculate[F: Field: NRoot : Order: Trig](secularState: SecularState[F]): LongPeriodPeriodicState[F] = {
       /* ----------------- compute extra mean quantities ------------- */
    import secularState._
    import ocofs._,elems._
    val axnl = e * cos(ω)
    val temp = 1 / (a * (1 - e * e))
    val aynl = e * sin(ω) + temp * aycof
    val xl   = M + ω + Ω + temp * xlcof * axnl

    LongPeriodPeriodicState(axnl, aynl, xl, secularState)
  }  
}

object LongPeriodPeriodicEffects extends LongPeriodPeriodicEffects