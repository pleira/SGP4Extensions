package predict4s

import org.scalactic.Or

package object coord {

  type sp = scala.specialized

  type ErrorMessage =  String

  // for secular corrections
  type SGPSecularCtx[F] = (SGPElems[F], InclinationCtx[F], SGPConstants[F])
  type SGPSecularResult[F] = SGPSecularCtx[F] Or ErrorMessage

  // for SPN secular corrections
  type SPNSecularCtx[F] = (SpecialPolarNodal[F], SGPSecularCtx[F])
  type SPNSecularResult[F] = SPNSecularCtx[F] Or ErrorMessage

  // intermediate results in polar nodals SPN
  type SGPSPNResult[F] = SpecialPolarNodal[F] Or ErrorMessage

  // final correction results in polar nodals SPN
  type SGPCorrPropCtx[F] = ((SpecialPolarNodal[F], SpecialPolarNodal[F]), SGPSecularCtx[F])
  type SGPCorrPropResult[F] = SGPCorrPropCtx[F] Or ErrorMessage

  // final correction results in Lara Non Singular (LNS)
  type SGPLNSResult[F] = LaraNonSingular[F] Or ErrorMessage

  // final results in cartesians
  type SGPPropCtx[F] = (CartesianElems[F], CartesianElems[F], SpecialPolarNodal[F])
  type SGPPropResult[F] = SGPPropCtx[F] Or ErrorMessage

  // For Long Period Periodic corrections in polar nodals SPN
  type LPPSPNCtx[F] = (SpecialPolarNodal[F], LongPeriodContext[F], SGPSecularCtx[F])
  type LPPSPNResult[F] = LPPSPNCtx[F] Or ErrorMessage

  // For Long Period Periodic corrections in cosI polar nodals CPN
  type LPPCPNResult[F] = CSpecialPolarNodal[F] Or ErrorMessage

  // Intermediate results in Lyddane's
  type LyddaneResult[F] = (LyddaneElems[F], LyddaneCtx[F])

  // for Kepler Equations
  type AnomalyResult[F] = AnomalyState[F] Or ErrorMessage

  // SGPElems
  type SGPElemsResult[F] = SGPElemsCtx[F] Or ErrorMessage
}
