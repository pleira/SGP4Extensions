package predict4s.tle

case class OrbitalState[F](t: F, posVel: TEME.CartesianElems[F])

case class GeoPotentialCoefs[F](C1: F, C2: F, C3: F, C4: F, C5: F, D2: F, D3: F, D4: F)

//case class EccentricAnomalyState[F](eo1 : F, coseo1: F, sineo1: F, ecosE: F, esinE: F, lppState: LongPeriodPeriodicState[F])  
//
//case class SGP4State[F](orbitalState: OrbitalState[F], uPV: TEME.CartesianElems[F], elem: TEME.SGPElems[F], 
//    sppState : ShortPeriodPeriodicState[F], wgs: SGPConstants[F])

// case class LongPeriodPeriodicState[F](axnl: F, aynl: F, xl: F, secularState: SecularState[F])

//case class ShortPeriodPeriodicState[F](
//    elem: TEME.SGPElems[F], 
//    I: F,     // inclination 
//    R: F,     // Radial velocity    
//    Ω: F,     // argument of the node
//    mrt: F, 
//    mvt: F, 
//    rvdot: F) 
//    eaState: EccentricAnomalyState[F])
    

    
trait LaneDragCoef[F] {
  def t2cof  : F ;def t3cof : F;  def t4cof  : F ; def t5cof : F
}

trait PotentialCoeficients[F] {
  def C1: F;  def C2: F;  def C3: F;  def C4: F;  def C5: F
  def D2: F;  def D3: F;  def D4: F
}

// Lara implementation
// case class EccentricAnomalyStateL[F](eo1 : F, coseo1: F, sineo1: F, ecosE: F, esinE: F)  



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


//trait SGP4[F] {
//  def propagate(t: F) : OrbitalState[F]
//  def state0 : SGP4Context[F]
//}