package predict4s.tle

/** 
 * The SGP-4 theory is applied for all orbits with periods of T <= 225 min. 
 * It performs a propagation in time of doubly averaged elements according to their
 * secular rates of change due to the zonal harmonics J2 and J4 of the Earth potential,
 * and due to drag perturbations in an atmosphere with a power-law altitude profile of air density. 
 * The propagated, doubly averaged elements at epoch are subsequently
 * converted into singly averaged elements, by overlaying long-periodic
 * perturbations due to J3, before a final conversion step to osculating elements by superimposition
 * of first-order, short-period perturbation amplitudes due to J2. 
 * (from Space Debris, by H. Klinkrad, pag 216).
 */
trait SGP4[F] {
  def propagate(t: F) : OrbitalState[F]
}

case class OrbitalState[F](t: F, posVel: TEME.CartesianElems[F])
case class SGP4Context[F](t: F, elem: TEME.SGPElems[F], posVel: TEME.CartesianElems[F], tif : SGP4TimeIndependentFunctions[F], wgs: SGPConstants[F])
