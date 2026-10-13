package sgp4
package coord

import spire.math.{Rational, Real}
import spire.math.ConvertableTo

/** Creates the numeric type F of the propagation from the Double and Int literal values of the algorithm. spire's `.as`
  * syntax and the generic ConvertableTo derivations of its Scala 2 artifact are not available in spire's Scala 3
  * artifact, so the propagation provides this small conversion type class, with instances for the numeric types it runs
  * on and a low priority derivation from spire's ConvertableTo.
  */
trait FromDouble[@sp(Double) F] {
  def apply(x: Double): F
  def apply(x: Int): F = apply(x.toDouble)
}

object FromDouble extends FromDoubleFromConvertableTo {

  def apply[F](x: Double)(implicit fd: FromDouble[F]): F = fd(x)

  def apply[F](x: Int)(implicit fd: FromDouble[F]): F = fd(x)

  /** the identity conversion */
  implicit lazy val doubleFromDouble: FromDouble[Double] = new FromDouble[Double] {
    def apply(x: Double): Double = x
  }

  /** exact conversion: the binary rational value of the Double */
  implicit lazy val rationalFromDouble: FromDouble[Rational] = new FromDouble[Rational] {
    def apply(x: Double): Rational = Rational(x)
  }

  /** exact conversion: the binary rational value of the Double */
  implicit lazy val realFromDouble: FromDouble[Real] = new FromDouble[Real] {
    def apply(x: Double): Real = Real(Rational(x))
  }
}

/** The low priority derivation from spire's ConvertableTo, which on Scala 2.13 is available for every F with the type
  * classes of the propagation; on Scala 3 it covers the numeric types with a ConvertableTo instance.
  */
private[coord] trait FromDoubleFromConvertableTo {
  implicit def fromConvertableTo[F](implicit c: ConvertableTo[F]): FromDouble[F] = new FromDouble[F] {
    def apply(x: Double): F = c.fromDouble(x)
  }
}
