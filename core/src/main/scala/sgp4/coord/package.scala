package sgp4

import org.scalactic.Or

package object coord {
  type sp = scala.specialized

  // for secular corrections
  type SGPSecularResult[F] = SGPSecularCtx[F] Or SGP4Error

  // intermediate results in polar nodals SPN
  type SGPSPNResult[F] = SpecialPolarNodal[F] Or SGP4Error

  // final correction results in Lara Non Singular (LNS)
  type SGPLNSResult[F] = LaraNonSingular[F] Or SGP4Error

  // final results in cartesians
  type SGPPropResult[F] = SGPPropCtx[F] Or SGP4Error

  // For Long Period Periodic corrections in polar nodals SPN
  type LPPSPNResult[F] = LPPSPNCtx[F] Or SGP4Error

  // For Long Period Periodic corrections in cosI polar nodals CPN
  type LPPCPNResult[F] = CSpecialPolarNodal[F] Or SGP4Error

  // for Kepler Equations
  type AnomalyResult[F] = AnomalyState[F] Or SGP4Error

  // SGPElems
  type SGPElemsResult[F] = SGPElemsCtx[F] Or SGP4Error
}
