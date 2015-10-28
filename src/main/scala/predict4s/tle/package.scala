package predict4s
import spire.algebra._
import spire.math._
import spire.implicits._
import spire.syntax.primitives._

package object tle {
    
  // true equator, mean equinox (TEME)
  
  object TEME extends ReferenceSystem {
     
    def sgpElems[F: Field: Trig](tle: TLE) :  SGPElems[F] = { 
      val e0 = tle.eccentricity.toDouble.as[F]
      val i0 = tle.inclination.toDouble.toRadians.as[F]
      val pa = tle.argumentOfPeriapsis.toDouble.toRadians.as[F]
      val raan = tle.rightAscension.toDouble.toRadians.as[F]
      val meanAnomaly =  tle.meanAnomaly.toDouble.toRadians.as[F]
      def meanMotion = tle.meanMotion.toDouble.as[F]
      val year = tle.year
      val bStar = tle.atmosphericDragCoeficient.toDouble.as[F]
    
      // in julian days
      val epoch : F = {
        val mdhms = days2mdhms(tle.year, tle.epoch.toDouble)
        (jday(tle.year, mdhms._1, mdhms._2, mdhms._3, mdhms._4, mdhms._5) - 2433281.5).as[F]
      }
      val ω0 = pa
      val Ω0 = raan
      val M0 = meanAnomaly    
      val radpm0 = revPerDay2RadPerMin(meanMotion)
  
      SGPElems[F](radpm0,e0,i0,pa,raan,M0,bStar,epoch)
    }
    
    def revPerDay2RadPerMin[F: Field](rpd: F) : F = 2 * pi * rpd / 1440 

  }
     
  // use old way of finding gst
  // count integer number of days from 0 jan 1970
  def oldGst(epoch: Double) : Double = {
     val twopi = 2*pi
     val    ts70  = epoch - 7305.0
     val    ds70 = floor(ts70 + 1.0e-8)
     val    tfrac = ts70 - ds70
         // find greenwich location at epoch
     val    c1    = 1.72027916940703639e-2
     val    thgr70= 1.7321343856509374
     val    fk5r  = 5.07551419432269442e-15
     val    c1p2p = c1 + twopi
     var    gsto  = EuclideanRing[Double].mod( thgr70 + c1*ds70 + c1p2p*tfrac + ts70*ts70*fk5r, twopi)
     if ( gsto < 0.0 ) gsto + twopi else gsto
   }
   

/**
*                           function gstime
*
*  this function finds the greenwich sidereal time.
*
*  author        : david vallado                  719-573-2600    1 mar 2001
*
*  inputs          description                    range / units
*    jdut1       - julian date in ut1             days from 4713 bc
*
*  outputs       :
*    gstime      - greenwich sidereal time        0 to 2pi rad
*
*  locals        :
*    temp        - temporary variable for doubles   rad
*    tut1        - julian centuries from the
*                  jan 1, 2000 12 h epoch (ut1)
*
*  coupling      :
*    none
*
*  references    :
*    vallado       2004, 191, eq 3-45
*/

def  gstime[F : Field: Order](jdut1: F) : F = {
     val twopi = (2 * pi).as[F]
     val deg2rad = pi / 180

     val tut1 = (jdut1 - 2451545) / 36525
     var temp = -6.2e-6* tut1 * tut1 * tut1 + 0.093104 * tut1 * tut1 +
             (876600.0*3600 + 8640184.812866) * tut1 + 67310.54841  // sec
     temp = EuclideanRing[F].mod(temp * deg2rad / 240, twopi) //360/86400 = 1/240, to deg, to rad

     // ------------------------ check quadrants ---------------------
     if (temp < 0.as[F])  temp += twopi

     temp
   }  // end gstime
  

/**
*                           procedure jday
*
*  this procedure finds the julian date given the year, month, day, and time.
*    the julian date is defined by each elapsed day since noon, jan 1, 4713 bc.
*
*  algorithm     : calculate the answer in one step for efficiency
*
*  author        : david vallado                  719-573-2600    1 mar 2001
*
*  inputs          description                    range / units
*    year        - year                           1900 .. 2100
*    mon         - month                          1 .. 12
*    day         - day                            1 .. 28,29,30,31
*    hr          - universal time hour            0 .. 23
*    min         - universal time min             0 .. 59
*    sec         - universal time sec             0.0 .. 59.999
*
*  outputs       :
*    jd          - julian date                    days from 4713 bc
*
*  locals        :
*    none.
*
*  coupling      :
*    none.
*
*  references    :
*    vallado       2007, 189, alg 14, ex 3-14
*
*/

def jday( year : Int,  mon : Int,  day : Int,  hr : Int,  minute : Int, sec : Double) : Double =
     367 * year -
          floor((7 * (year + floor((mon + 9) / 12.0))) * 0.25) +
          floor( 275 * mon / 9.0 ) +
          day + 1721013.5 +
          ((sec / 60.0 + minute) / 60.0 + hr) / 24.0  // ut in days
          // - 0.5*sgn(100.0*year + mon - 190002.5) + 0.5
     // end jday

/**
*                           procedure days2mdhms
*
*  this procedure converts the day of the year, days, to the equivalent month
*    day, hour, minute and second.
*
*  algorithm     : set up array for the number of days per month
*                  find leap year - use 1900 because 2000 is a leap year
*                  loop through a temp value while the value is < the days
*                  perform int conversions to the correct day and month
*                  convert remainder into h m s using type conversions
*
*  author        : david vallado                  719-573-2600    1 mar 2001
*
*  inputs          description                    range / units
*    year        - year                           1900 .. 2100
*    days        - julian day of the year         0.0  .. 366.0
*
*  outputs       :
*    mon         - month                          1 .. 12
*    day         - day                            1 .. 28,29,30,31
*    hr          - hour                           0 .. 23
*    min         - minute                         0 .. 59
*    sec         - second                         0.0 .. 59.999
*
*  locals        :
*    dayofyr     - day of year
*    temp        - temporary extended values
*    inttemp     - temporary int value
*    i           - index
*    lmonth[12]  - int array containing the number of days per month
*
*  coupling      :
*    none.
*/

def  days2mdhms(year: Int, days : Double) = {
     
  val lmonth : Vector[Int] = 
       if ( (year % 4) == 0 )
         Vector(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
       else 
         Vector(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  val   dayofyr : Int = floor(days).toInt
     
     /* ----------------- find month and day of month ---------------- */

 var i = 1
 var inttemp = 0
 while ((dayofyr > inttemp + lmonth(i-1)) && (i < 12))
 {
   inttemp = inttemp + lmonth(i-1)
   i = i + 1
 }
  val   mon = i
  val   day = dayofyr - inttemp

     /* ----------------- find hours minutes and seconds ------------- */
  var   temp = (days - dayofyr) * 24.0
  val   hr   = floor(temp).toInt
  temp = (temp - hr) * 60
  val   minute  = floor(temp).toInt
  val   sec  = (temp - minute) * 60
  (mon, day, hr, minute, sec)
  }  // end days2mdhms

/**
*                           procedure invjday
*
*  this procedure finds the year, month, day, hour, minute and second
*  given the julian date. tu can be ut1, tdt, tdb, etc.
*
*  algorithm     : set up starting values
*                  find leap year - use 1900 because 2000 is a leap year
*                  find the elapsed days through the year in a loop
*                  call routine to find each individual value
*
*  author        : david vallado                  719-573-2600    1 mar 2001
*
*  inputs          description                    range / units
*    jd          - julian date                    days from 4713 bc
*
*  outputs       :
*    year        - year                           1900 .. 2100
*    mon         - month                          1 .. 12
*    day         - day                            1 .. 28,29,30,31
*    hr          - hour                           0 .. 23
*    min         - minute                         0 .. 59
*    sec         - second                         0.0 .. 59.999
*
*  locals        :
*    days        - day of year plus fractional
*                  portion of a day               days
*    tu          - julian centuries from 0 h
*                  jan 0, 1900
*    temp        - temporary double values
*    leapyrs     - number of leap years from 1900
*
*  coupling      :
*    days2mdhms  - finds month, day, hour, minute and second given days and year
*
*  references    :
*    vallado       2007, 208, alg 22, ex 3-13
*/

  def  invjday(jd: Double) : (Int, Int, Int, Int, Int, Double) = {


     /* --------------- find year and days of the year --------------- */
     val temp    = jd - 2415019.5
     val tu      = temp / 365.25
     var year : Int     = 1900 + floor(tu).toInt
     var leapyrs : Int  =  floor((year - 1901) * 0.25).toInt

     // optional nudge by 8.64x10-7 sec to get even outputs
     var days    = temp - ((year - 1900) * 365.0 + leapyrs) + 0.00000000001

     /* ------------ check for case of beginning of a year ----------- */
     if (days < 1.0)  {
         year    = year - 1
         leapyrs = floor((year - 1901) * 0.25).toInt
         days    = temp - ((year - 1900) * 365 + leapyrs)
     }

     /* ----------------- find remaing data  ------------------------- */
     val (mon, day, hr, minute, sec0) = days2mdhms(year, days)
     val sec = sec0 - 0.00000086400
      (year, mon, day, hr, minute, sec)
   }  // end invjday
  
}