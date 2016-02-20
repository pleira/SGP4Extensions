package predict4s
package coord


/**
 * TLE represents the non converted data given bz a Two Line Element.
 * No double mumeric conversions done here, so double values are returned as Strings.
 * See Dr. T.S. Kelso comments on TLE at 
 * http://www.celestrak.com/columns/v04n03/
 */
case class TLE(
  lineNumber: Int
  ,satelliteNumber: Int
  ,classification: Char
  ,launchYear: Int
  ,launchNumber: Int
  ,launchPiece: String
  ,ephemerisType: Int
  ,elementNumber: Int
  ,epochyear: Int
  ,year: Int
  ,epoch: String      // day of the year and fractional portion of the day
  ,meanMotion: String
  ,meanMotionFirstDerivative: String
  ,meanMotionSecondDerivative: String
  ,atmosphericDragCoeficient: String
  ,eccentricity : String
  ,inclination : String
  ,argumentOfPeriapsis : String
  ,rightAscension : String
  ,meanAnomaly : String
  ,revolutionNumberAtEpoch: Int
)

object TLE {

  def apply(line1 : String, line2: String ) : TLE = {
    val lineNumber = parseInt(line1, 1, 1)
    val satelliteNumber = parseInt(line1, 2, 5)
    val classification  = line1.charAt(7)
    val launchYear      = parseYear(line1, 9)
    val launchNumber    = parseInt(line1, 11, 3)
    val launchPiece     = line1.substring(14, 17).trim
    val ephemerisType   = parseInt(line1, 62, 1)
    val elementNumber   = parseInt(line1, 64, 4)
    val epochyear       = parseInt(line1, 18, 2)
    val year            = epochyear + (if (epochyear < 57)  2000 else 1900)
    
    val epoch           = parse(line1, 20, 12)  // days of the year
    // rev/day, 2 * rev/day^2 and 6 * rev/day^3 
    val meanMotion                 = parse(line2, 52, 11)
//        drag                       = line1.parseDouble(33, 10),
    val meanMotionFirstDerivative  = parse(line1, 33, 10) 
//        nddot6                     = ((line1.substring(44, 45) + '.' +
//                                     line1.substring(45, 50) + 'e' +
//                                     line1.substring(50, 52)).replace(' ', '0')).toDouble,
    val meanMotionSecondDerivative = (line1.substring(44, 45) + '.' +
                                 line1.substring(45, 50) + 'e' +
                                 line1.substring(50, 52)).replace(' ', '0')
    val atmosphericDragCoeficient = (line1.substring(53, 54) + '.' +
              line1.substring(54, 59) + 'e' +
              line1.substring(59, 61)).replace(' ', '0')   
    val eccentricity = ("." + line2.substring(26, 33).replace(' ', '0'))
    val inclination = parse(line2, 8, 8)
    val argumentOfPeriapsis = parse(line2, 34, 8)
    val rightAscension  = line2.substring(17, 25).replace(' ', '0')
    val meanAnomaly = parse(line2, 43, 8)
    val revolutionNumberAtEpoch = parseInt(line2, 63, 5)
    TLE(lineNumber,satelliteNumber,classification,launchYear,launchNumber,
          launchPiece, ephemerisType, elementNumber, epochyear, year, epoch, meanMotion,
          meanMotionFirstDerivative, meanMotionSecondDerivative, atmosphericDragCoeficient,
          eccentricity,inclination, argumentOfPeriapsis,rightAscension, meanAnomaly,
          revolutionNumberAtEpoch)
  }
  
  def parseInt(line : String, start: Int, length: Int): Int = {
    line.substring(start, start + length).replace(' ', '0').toInt
  }
  
  def parse(line : String, start: Int, length: Int) : String = 
    line.substring(start, start + length)
  
  
  def parseYear(line : String, start: Int) : Int = {
    val year = 2000 + parseInt(line, start, 2)
    if (year > 2056) (year - 100) else year
  }

  
  def parseFile(path: String): List[TLE] = {
    parseLines(io.Source.fromFile(path).getLines.toList)
  }
  
  def parseResource(path: String): List[TLE] = {
    parseLines(io.Source.fromInputStream(getClass.getResourceAsStream(path)).getLines.toList)
  }

  def parseLines(list: List[String]) : List[TLE] = {
    var queue = new scala.collection.mutable.Queue[TLE]() // scala.collection.mutable.LinkedList[TLE]();
    val iter = list.iterator
    while (iter.hasNext) {
      val line1 = iter.next()
      if (!line1.startsWith("#")) {
        val line2 = iter.next()
        if (!line2.startsWith("#")) {
          queue += apply(line1, line2)           
        }
      }
    }
    queue.toList  
  }
  
  
  import java.util.regex.Pattern;
  
  val LINE_1_PATTERN : Pattern =
      Pattern.compile("1 [ 0-9]{5}U [ 0-9]{5}[ A-Z]{3} [ 0-9]{5}[.][ 0-9]{8} [ +-][.][ 0-9]{8} " +
                      "[ +-][ 0-9]{5}[+-][ 0-9] [ +-][ 0-9]{5}[+-][ 0-9] [ 0-9] [ 0-9]{4}[ 0-9]")

  val LINE_2_PATTERN : Pattern =
      Pattern.compile("2 [ 0-9]{5} [ 0-9]{3}[.][ 0-9]{4} [ 0-9]{3}[.][ 0-9]{4} [ 0-9]{7} " +
                      "[ 0-9]{3}[.][ 0-9]{4} [ 0-9]{3}[.][ 0-9]{4} [ 0-9]{2}[.][ 0-9]{13}[ 0-9]")

  def isFormatOK(line1: String, line2: String) : Boolean = {
      if (!LINE_1_PATTERN.matcher(line1).matches() ||
          !LINE_2_PATTERN.matcher(line2).matches()) 
         return false

      val satNum  = parseInt(line1, 2, 5)
      val satNum2 = parseInt(line2, 2, 5)
      if (satNum != satNum2) return false

      // check sums
      val checksum1 = checksum(line1)
      if (line1.substring(68).toInt != (checksum1 % 10)) 
        return false
      
      val checksum2 = checksum(line2)
      if (line2.substring(68).toInt != (checksum2 % 10)) 
        return false
      
      true
  }

  def checksum(line: String): Int = {
      var sum: Int = 0
      for (j <- 0 until 68) {
       val c = line(j)
       if (Character.isDigit(c)) {
            sum += Character.digit(c, 10)
          } else if (c == '-') {
            sum += 1
          }
      }
      sum % 10
  }

}