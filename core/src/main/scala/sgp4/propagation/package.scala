package sgp4

import spire.algebra.{Order, Ring, Signed}

package object propagation:

  /** Derives a Signed instance from Order and Ring. This replicates the derivation that spire 0.11 provided in
    * spire.algebra.Signed's companion object and that newer spire versions removed. It keeps the generic SGP4 code free
    * of explicit Signed bounds, as in the original code.
    */
  implicit def orderedRingIsSigned[A: Order: Ring]: Signed[A] = new Signed.forAdditiveCommutativeGroup[A]:
    val order: Order[A] = Order[A]
    def zero: A = Ring[A].zero
    def plus(x: A, y: A): A = Ring[A].plus(x, y)
    def negate(x: A): A = Ring[A].negate(x)
