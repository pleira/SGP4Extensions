package predict4s.coord

/**
 * TLE represents the non converted data given bz a Two Line Element.
 * No double mumeric conversions done here, so double values are returned as Strings.
 * See Dr. T.S. Kelso comments on TLE at 
 * http://www.celestrak.com/columns/v04n03/
 */
trait TLE {
  def lineNumber: Int
  def satelliteNumber: Int
  def classification: Char
  def launchYear: Int
  def launchNumber: Int
  def launchPiece: String
  def ephemerisType: Int
  def elementNumber: Int
  def epochyear: Int
  def year: Int
  def epoch: String      // day of the year and fractional portion of the day
  def meanMotion: String
  def meanMotionFirstDerivative: String
  def meanMotionSecondDerivative: String
  def revolutionNumberAtEpoch: Int
  def atmosphericDragCoeficient: String
  def eccentricity : String
  def inclination : String
  def argumentOfPeriapsis : String
  def rightAscension : String
  def meanAnomaly : String
}

object TLE {

  def apply(line1 : String, line2: String ) : TLE = new TLE {
    def lineNumber = parseInt(line1, 1, 1)
      def satelliteNumber = parseInt(line1, 2, 5)
      def classification  = line1.charAt(7)
      def launchYear      = parseYear(line1, 9)
      def launchNumber    = parseInt(line1, 11, 3)
      def launchPiece     = line1.substring(14, 17).trim
      def ephemerisType   = parseInt(line1, 62, 1)
      def elementNumber   = parseInt(line1, 64, 4)
      def epochyear       = parseInt(line1, 18, 2)
      def year            = epochyear + (if (epochyear < 57)  2000 else 1900)
      
      def epoch           = parse(line1, 20, 12)  // days of the year
      // rev/day, 2 * rev/day^2 and 6 * rev/day^3 
      def meanMotion                 = parse(line2, 52, 11)
//        drag                       = line1.parseDouble(33, 10),
      def meanMotionFirstDerivative  = parse(line1, 33, 10) 
//        nddot6                     = ((line1.substring(44, 45) + '.' +
//                                     line1.substring(45, 50) + 'e' +
//                                     line1.substring(50, 52)).replace(' ', '0')).toDouble,
      def meanMotionSecondDerivative = (line1.substring(44, 45) + '.' +
                                   line1.substring(45, 50) + 'e' +
                                   line1.substring(50, 52)).replace(' ', '0')
      def eccentricity = ("." + line2.substring(26, 33).replace(' ', '0'))
      def inclination = parse(line2, 8, 8)
      def argumentOfPeriapsis = parse(line2, 34, 8)
      def rightAscension  = line2.substring(17, 25).replace(' ', '0')
      def meanAnomaly = parse(line2, 43, 8)

      def revolutionNumberAtEpoch = parseInt(line2, 63, 5)
      def atmosphericDragCoeficient = (line1.substring(53, 54) + '.' +
                line1.substring(54, 59) + 'e' +
                line1.substring(59, 61)).replace(' ', '0')                     
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