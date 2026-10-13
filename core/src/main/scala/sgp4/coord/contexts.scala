package sgp4.coord

import sgp4.earth_gravity.SGPConstants

/** The context types that thread the state through the SGP4 propagation pipeline. They are case classes instead of
  * tuples, so that every step of the pipeline accesses the fields by name.
  */

/** The secular elements at the propagation time, with their inclination context and gravity model. The result of the
  * secular corrections and the input of the periodic ones.
  */
case class SGPSecularCtx[@sp(Double) F](elem: SGPElems[F], iCtx: InclinationCtx[F], wgs: SGPConstants[F])

/** The secular elements transformed to Special Polar Nodals, an intermediate step towards the long period corrections.
  */
case class SPNSecularCtx[@sp(Double) F](spn: SpecialPolarNodal[F], secularCtx: SGPSecularCtx[F])

/** The long period periodic correction results in Special Polar Nodals, with the long period context and the secular
  * context they are computed from.
  */
case class LPPSPNCtx[@sp(Double) F](
    spn: SpecialPolarNodal[F],
    lppCtx: LongPeriodContext[F],
    secularCtx: SGPSecularCtx[F]
)

/** The final propagation results: the TEME state vectors, the same vectors in the internal unit system, and the final
  * periodic correction results in Special Polar Nodals.
  */
case class SGPPropCtx[@sp(Double) F](pv: CartesianElems[F], unitPv: CartesianElems[F], spn: SpecialPolarNodal[F])

/** The Lyddane elements with their context, an intermediate result of the long period corrections in Lyddane's
  * variables.
  */
case class LyddaneResult[@sp(Double) F](elems: LyddaneElems[F], ctx: LyddaneCtx[F])
