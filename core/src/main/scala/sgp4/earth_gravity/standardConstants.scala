package sgp4.earth_gravity
import sgp4.coord.FromDouble
import sgp4.coord.sp

import spire.algebra.*
import spire.math.{pi}
import spire.math.Real
import spire.implicits.*

trait WGSConstants[@sp(Double) F]:

  /** sqrt(Grativational Constant * Earths's Mass) */
  def MU: F

  /** Equatorial radius of the Earth in km */
  def aE: F
  // def EARTH_RADIUS = aE
  /** sqrt (Grativational Constant * Earths's Mass)  in units (Earths Radii)** 1.5 / minute */
  def KE: F

  /** J2 spherical harmonic value */
  def J2: F

  /** J3 spherical harmonic value */
  def J3: F

  /** J3 spherical harmonic value */
  def J4: F
  def CK2: F = K2 // = FromDouble[F](5.413080e-4) // 1/2 J2aE
  def CK4: F = K4 // = -3*J4* aE⁴ / 8
  def K2: F
  def K4: F
  def A30: F
  def A3OVK2: F // def K2: F   // units of (Earth radii) ]

abstract class WGS[@sp(Double) F: Field: FromDouble]() extends WGSConstants[F]:
  def K2 = J2 * aE / 2 // We use Vallado's, in reality J2 * aE² / 2
  def K4 = -3 * J4 * aE * aE * aE * aE / 8
  def A3OVK2 = -J3 / K2
  def A30 = -J3 * aE * aE * aE
  override def CK2 = K2
  override def CK4 = K4

// old WGS72 constants
class WGS721Constants[F: Field: FromDouble]() extends WGS[F]:
  val MU = FromDouble[F](398600.8)
  val aE = FromDouble[F](6378.135)
  val KE = FromDouble[F](0.0743669161) // units 1/min = 60/sqrt(aE³/μ)
  val J2 = FromDouble[F](0.001082616)
  val J3 = -FromDouble[F](0.00000253881)
  val J4 = -FromDouble[F](0.00000165597)
  val vkmpersec = aE * KE / 60
  val `J2/J3`: F = J2 / J3
  val `J3/J2`: F = J3 / J2
  override val K2 = super.K2
  override val K4 = super.K4

class WGS72Constants[F: Field: FromDouble]() extends WGS[F]:
  val MU = FromDouble[F](398600.79964)
  val aE = FromDouble[F](6378.135)
  val KE = FromDouble[F](0.07436691613317) //   60 / (aE³/ MU).sqrt  // /min
  val J2 = FromDouble[F](0.001082616)
  val J3 = -FromDouble[F](0.00000253881)
  val J4 = -FromDouble[F](0.00000165597)
  override val K2 = super.K2
  override val K4 = super.K4
  val `J2/J3`: F = J2 / J3
  val `J3/J2`: F = J3 / J2
  val α: F = aE // Earth's equatorial radius

class WGS84Constants[F: Field: FromDouble]() extends WGS[F]:
  val MU = FromDouble[F](398600.5)
  val aE = FromDouble[F](6378.137)
  val KE = FromDouble[F](0.07436685316871) //   60 / (aE³/ MU).sqrt   //  /min
  val J2 = FromDouble[F](0.00108262998905)
  val J3 = -FromDouble[F](0.00000253215306)
  val J4 = -FromDouble[F](0.00000161098761)
  override val K2 = super.K2
  override val K4 = super.K4
  val `J2/J3`: F = J2 / J3
  val `J3/J2`: F = J3 / J2
  val α: F = aE // Earth's equatorial radius

object WGS721Constants:
  given tleDoubleConstants: WGS721Constants[Double] = new WGS721Constants[Double]()
  given tleRealConstants: WGS721Constants[Real] = new WGS721Constants[Real]()

object WGS72Constants:
  given tleDoubleConstants: WGS72Constants[Double] = new WGS72Constants[Double]()
  given tleRealConstants: WGS72Constants[Real] = new WGS72Constants[Real]()

object WGS84Constants:
  given tleDoubleConstants: WGS84Constants[Double] = new WGS84Constants[Double]()
  given tleRealConstants: WGS84Constants[Real] = new WGS84Constants[Real]()

trait SGPConstants[@sp(Double) F]:
  def MU: F
  def aE: F
  def J2: F
  def J3: F
  def J4: F
  def KE: F
  def vkmpersec: F
  def α: F
  def `J2/J3`: F
  def `J3/J2`: F
  def μ: F
  def `2pi` = 2 * pi
  def `2/3`: F
  def `1/3`: F
//  def CK2: F = wgs.K2 // = FromDouble[F](5.413080e-4) // 1/2 J2aE
//  def CK4: F = wgs.K4  // = -3*J4* aE⁴ / 8
//  def K2: F =  wgs.K2
//  def K4: F = wgs.K4
//  def A30 : F = wgs.A30
//  def A3OVK2 : F  = wgs.A3OVK2  // def K2: F   // units of (Earth radii) ]

class SGP721Constants[@sp(Double) F: Field: FromDouble](wgs: WGS721Constants[F]) extends SGPConstants[F]:
  override def MU: F = wgs.MU
  override def aE: F = wgs.aE
  override def KE: F = wgs.KE
  override def J2: F = wgs.J2
  override def J3: F = wgs.J3
  override def J4: F = wgs.J4
  val vkmpersec = aE * KE / 60
  override val `J2/J3`: F = J2 / J3
  override val `J3/J2`: F = J3 / J2
  override val α: F = aE // Earth's equatorial radius
  override val μ: F = wgs.MU
  override val `2/3` = FromDouble[F](2) / FromDouble[F](3)
  override val `1/3` = FromDouble[F](1) / FromDouble[F](3)

class SGP72Constants[@sp(Double) F: Field: FromDouble](wgs: WGS72Constants[F]) extends SGPConstants[F]:
  override def MU: F = wgs.MU
  override def aE: F = wgs.aE
  override def KE: F = wgs.KE
  override def J2: F = wgs.J2
  override def J3: F = wgs.J3
  override def J4: F = wgs.J4
  val vkmpersec = aE * KE / 60
  override val `J2/J3`: F = J2 / J3
  override val `J3/J2`: F = J3 / J2
  override val α: F = aE // Earth's equatorial radius
  override val μ: F = wgs.MU
  override val `2/3` = FromDouble[F](2) / FromDouble[F](3)
  override val `1/3` = FromDouble[F](1) / FromDouble[F](3)

class SGP84Constants[@sp(Double) F: Field: FromDouble](wgs: WGS84Constants[F]) extends SGPConstants[F]:
  override def MU: F = wgs.MU
  override def aE: F = wgs.aE
  override def KE: F = wgs.KE
  override def J2: F = wgs.J2
  override def J3: F = wgs.J3
  override def J4: F = wgs.J4
  val vkmpersec = aE * KE / 60
  override val `J2/J3`: F = J2 / J3
  override val `J3/J2`: F = J3 / J2
  override val α: F = aE // Earth's equatorial radius
  override val μ: F = wgs.MU
  override val `2/3` = FromDouble[F](2) / FromDouble[F](3)
  override val `1/3` = FromDouble[F](1) / FromDouble[F](3)

object SGP721Constants:
  given tleDoubleConstants: SGP721Constants[Double] =
    new SGP721Constants[Double](WGS721Constants.tleDoubleConstants)
  given tleRealConstants: SGP721Constants[Real] =
    new SGP721Constants[Real](WGS721Constants.tleRealConstants)

object SGP72Constants:
  given tleDoubleConstants: SGP72Constants[Double] =
    new SGP72Constants[Double](WGS72Constants.tleDoubleConstants)
  given tleRealConstants: SGP72Constants[Real] = new SGP72Constants[Real](WGS72Constants.tleRealConstants)

object SGP84Constants:
  given tleDoubleConstants: SGP84Constants[Double] =
    new SGP84Constants[Double](WGS84Constants.tleDoubleConstants)
  given tleRealConstants: SGP84Constants[Real] = new SGP84Constants[Real](WGS84Constants.tleRealConstants)
// Variable name Definition Value as given in Hoots document
//CK2 = 5.413080E-4 // 1/2 J2aE
//CK4 = .62098875E-6 //  J4
//E6A = 10E-6  // 6 1.0 E-6
//QOMS2T = 1.88027916E-9
//S = 1.01222928
//TOTHRD =  .66666667 // 2/3
//XJ3 = - 0.253881E-5 // J3
//XKE = 0.743669161E-1
//XKMPER = 6378.135
//XMNPDA = 1440.0
//AE = 1.0 // distance units/Earth radii
//DE2RA = 0.174532925E-1 // radians/degree
//PI = 3.14159265 // π
//PIO2 = 1.57079633 // π/2
//`2pi` =  6.2831853 // 2π
//X3PIO2 = 4.71238898  // 3π/2

//object TLEConstants {
//  val TLEConstantsD = new TLEConstants[Double]
//  val TLEConstantsR = new TLEConstants[Real]
//  val TLEConstantsBD = new TLEConstants[BigDecimal]
//
//  import reflect.runtime.universe._
//  def getTLEConstants[T](using tag: TypeTag[T]) : TLEConstants[T] =
//    typeOf[T] match {
//      case t if (t <:< typeOf[Double])      => TLEConstantsD
//      case t if (t <:< typeOf[Real])        => TLEConstantsR
//      case t if (t <:< typeOf[BigDecimal])  => TLEConstantsBD
//      // case t if (t <:< typeOf[Field[_]])  => TLEConstants[t]()
//      case _ => throw new IllegalArgumentException()
//  }
//}

/*
          // -- wgs-72 low precision str#3 constants --
           case wgs72old:
           mu     = 398600.79964;        // in km3 / s2
           radiusearthkm = 6378.135;     // km
           xke    = 0.0743669161;
           tumin  = 1.0 / xke;
           j2     =   0.001082616;
           j3     =  -0.00000253881;
           j4     =  -0.00000165597;
           j3oj2  =  j3 / j2;
         break;
           // ------------ wgs-72 constants ------------
           case wgs72:
           mu     = 398600.8;            // in km3 / s2
           radiusearthkm = 6378.135;     // km
           xke    = 60.0 / sqrt(radiusearthkm*radiusearthkm*radiusearthkm/mu);
           tumin  = 1.0 / xke;
           j2     =   0.001082616;
           j3     =  -0.00000253881;
           j4     =  -0.00000165597;
           j3oj2  =  j3 / j2;
          case wgs84:
           // ------------ wgs-84 constants ------------
           mu     = 398600.5;            // in km3 / s2
           radiusearthkm = 6378.137;     // km
           xke    = 60.0 / sqrt(radiusearthkm*radiusearthkm*radiusearthkm/mu);
           tumin  = 1.0 / xke;
           j2     =   0.00108262998905;
           j3     =  -0.00000253215306;
           j4     =  -0.00000161098761;
           j3oj2  =  j3 / j2;
 */
