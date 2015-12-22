package predict4s.tle

/* Near earth examples from Vallado's */
trait NearTLEs extends TLE00005 with TLE06251 with TLE28057 {
  def tles = List(tle00005, tle06251, tle28057)
  def lines = List(lines00005,lines06521,lines28057)
}

trait TLE00005 {
  val lines00005 = List(
  "#                       # TEME example",
  "1 00005U 58002B   00179.78495062  .00000023  00000-0  28098-4 0  4753",
  "2 00005  34.2682 348.7242 1859667 331.7664  19.3264 10.82419157413667     0.00      4320.0        360.00")
  val tle00005 = TLE.parseLines(lines00005).head
}

trait TLE06251 {
  val lines06521 = List(
  "#   DELTA 1 DEB         # near earth normal drag equation",
  "#                       # perigee = 377.26km, so moderate drag case",
  "1 06251U 62025E   06176.82412014  .00008885  00000-0  12808-3 0  3985",
  "2 06251  58.0579  54.0425 0030035 139.1568 221.1854 15.56387291  6774      0.0      2880.0        120.00")
  val tle06251 = TLE.parseLines(lines06521).head
}

trait TLE28057 {
  val lines28057= List(
  "#                       # drop certain normal drag terms",
  "1 28057U 03049A   06177.78615833  .00000060  00000-0  35940-4 0  1836",
  "2 28057  98.4283 247.6961 0000884  88.1964 271.9322 14.35478080140550      0.0      2880.0        120.00")
  val tle28057 = TLE.parseLines(lines28057).head
}
     