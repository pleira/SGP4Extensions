package predict4s
import org.scalactic.Or
import org.scalactic.Good
import org.scalactic.Bad

package object coord {
  
  type ErrorMessage =  String
  
  // for secular corrections
  type SGPSecularCtx[F] = (SGPElems[F], InclinationCtx[F], SGPConstants[F])
  type SGPSecularResult[F] = SGPSecularCtx[F] Or ErrorMessage
 
  // for SPN secular corrections
  type SPNSecularCtx[F] = (SpecialPolarNodal[F], SGPSecularCtx[F])
  type SPNSecularResult[F] = SPNSecularCtx[F] Or ErrorMessage
  
  // intermediate results in polar nodals SPN
  type SGPSPNCtx[F] = (SpecialPolarNodal[F], SpecialPolarNodal[F])
  type SGPSPNResult[F] = SGPSPNCtx[F] Or ErrorMessage

  // final correction results in polar nodals SPN
  type SGPCorrPropCtx[F] = (SGPSPNCtx[F], SGPSecularCtx[F]) 
  type SGPCorrPropResult[F] = SGPCorrPropCtx[F] Or ErrorMessage

  // final correction results in Lara Non Singular
  type SGPLaraCtx[F] = (LaraNonSingular[F], (LaraNonSingular[F], SGPSecularCtx[F]), F) 
  type SGPLaraResult[F] = SGPLaraCtx[F] Or ErrorMessage
  
  // final results in cartesians
  type SGPPropCtx[F] = (CartesianElems[F], CartesianElems[F], SGPCorrPropCtx[F]) 
  type SGPPropResult[F] = SGPPropCtx[F] Or ErrorMessage
  
  // For Long Period Periodic corrections in polar nodals SPN
  type LPPSPNCtx[F] = (SpecialPolarNodal[F], LongPeriodContext[F], SGPSecularCtx[F])
  type LPPSPNResult[F] = LPPSPNCtx[F] Or ErrorMessage

  // For Long Period Periodic corrections in cosI polar nodals CPN
  type LPPCPNCtx[F] = (CSpecialPolarNodal[F], LongPeriodContext[F], SGPSecularCtx[F])
  type LPPCPNResult[F] = LPPCPNCtx[F] Or ErrorMessage
  
  // Intermediate results in Lyddane's
  type LyddaneCtx[F] = (F,F,F) // (ϵ3,η,esinω)
  type LyddaneResult[F] = (LyddaneElems[F], LyddaneCtx[F])
  
  // for Kepler Equations
  type AnomalyResult[F] = AnomalyState[F] Or ErrorMessage   
  
  // SGPElems
  type SGPElemsResult[F] = SGPElemsCtx[F] Or ErrorMessage
}