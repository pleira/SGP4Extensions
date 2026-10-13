package sgp4
package coord

/** The conversions of Double and Int values into the numeric type F of the propagation. They go through the FromDouble
  * type class of the package, see there: spire's own `.as` conversion is not available in its Scala 3 artifact.
  */
private[coord] object convert {

  def asF[F: FromDouble](x: Double): F = FromDouble[F](x)

  def asF[F: FromDouble](x: Int): F = FromDouble[F](x)
}
