package predict4s
package sgp

import predict4s.coord.TLE;


trait TestNearTLEs {
  def tles : List[TestTLE] = List(TLE00005, TLE06251, TLE22312, TLE28057, TLE28350, TLE28872, TLE29141, TLE29238, TLE88888)
}

trait TestTLE {
  def lines: List[String]
  def tle: TLE
  def start: Int
  def end: Int
  def step: Int
}

trait TLE00005 {
  val lines00005 = List(
  "#                       # TEME example",
  "1 00005U 58002B   00179.78495062  .00000023  00000-0  28098-4 0  4753",
  "2 00005  34.2682 348.7242 1859667 331.7664  19.3264 10.82419157413667     0.00      4320.0        360.00")
  val tle00005 = TLE.parseLines(lines00005).head
  val start00005 = 0
  val end00005 = 4320
  val step00005 = 36
}

object TLE00005 extends TestTLE with TLE00005 {
  def lines: List[String] = lines00005
  def tle: TLE = tle00005
  def start: Int = start00005
  def end: Int = end00005
  def step: Int =  step00005
}

trait TLE06251 {
  val lines06251 = List(
  "#   DELTA 1 DEB         # near earth normal drag equation",
  "#                       # perigee = 377.26km, so moderate drag case",
  "1 06251U 62025E   06176.82412014  .00008885  00000-0  12808-3 0  3985",
  "2 06251  58.0579  54.0425 0030035 139.1568 221.1854 15.56387291  6774      0.0      2880.0        120.00")
  val tle06251 = TLE.parseLines(lines06251).head
  val start06251 = 0
  val end06251 = 2880
  val step06251 = 12
}

object TLE06251 extends TestTLE with TLE06251 {
  def lines: List[String] = lines06251
  def tle: TLE = tle06251
  def start: Int = start06251
  def end: Int = end06251
  def step: Int =  step06251
}

trait TLE28057 {
  val lines28057= List(
  "#                       # drop certain normal drag terms",
  "1 28057U 03049A   06177.78615833  .00000060  00000-0  35940-4 0  1836",
  "2 28057  98.4283 247.6961 0000884  88.1964 271.9322 14.35478080140550      0.0      2880.0        120.00")
  val tle28057 = TLE.parseLines(lines28057).head
  val start28057 = 0
  val end28057 = 2880
  val step28057 = 12
}

object TLE28057 extends TestTLE with TLE28057 {
  def lines: List[String] = lines28057
  def tle: TLE = tle28057
  def start: Int = start28057
  def end: Int = end28057
  def step: Int =  step28057
}
   
trait TLE22312 {
  val lines22312= List(
  "#   SL-6 R/B(2)         # last tle given, decayed 2006-04-04, day 94",
  "1 22312U 93002D   06094.46235912  .99999999  81888-5  49949-3 0  3953",
  "2 22312  62.1486  77.4698 0308723 267.9229  88.7392 15.95744531 98783  54.2028672   1440.0         20.00")
  val tle22312 = TLE.parseLines(lines22312).head
  val start22312 = 54.2028672
  val end22312 = 1440
  val step22312 = 20
}

object TLE22312 extends TestTLE with TLE22312 {
  def lines: List[String] = lines22312
  def tle: TLE = tle22312
  def start: Int = start22312.toInt
  def startD: Double = start22312
  def end: Int = end22312
  def step: Int =  step22312
}

trait TLE28350 {
  val lines28350= List(
  "#   COSMOS 2405         # Near Earth, perigee = 127.20 (< 156) s4 mod",
  "1 28350U 04020A   06167.21788666  .16154492  76267-5  18678-3 0  8894",
  "2 28350  64.9977 345.6130 0024870 260.7578  99.9590 16.47856722116490      0.0      2880.0        120.00")
  val tle28350 = TLE.parseLines(lines28350).head
  val start28350 = 0
  val end28350 = 2880
  val step28350 = 12
}

object TLE28350 extends TestTLE with TLE28350 {
  def lines: List[String] = lines28350
  def tle: TLE = tle28350
  def start: Int = start28350
  def end: Int = end28350
  def step: Int =  step28350
}

trait TLE28872 {
  val lines28872= List(
  "#   MINOTAUR R/B        # Sub-orbital case - Decayed 2005-11-29",
  "#                       #(perigee = -51km), lost in 50 minutes",
  "1 28872U 05037B   05333.02012661  .25992681  00000-0  24476-3 0  1534",
  "2 28872  96.4736 157.9986 0303955 244.0492 110.6523 16.46015938 10708      0.0        50.0          5.00")
  val tle28872 = TLE.parseLines(lines28872).head
  val start28872 = 0
  val end28872 = 50
  val step28872 = 5
}

object TLE28872 extends TestTLE with TLE28872 {
  def lines: List[String] = lines28872
  def tle: TLE = tle28872
  def start: Int = start28872
  def end: Int = end28872
  def step: Int =  step28872
}

trait TLE29141 {
  val lines29141= List(
  "#   SL-14 DEB           # Last stage of decay - lost in under 420 min",
  "1 29141U 85108AA  06170.26783845  .99999999  00000-0  13519-0 0   718",
  "2 29141  82.4288 273.4882 0015848 277.2124  83.9133 15.93343074  6828      0.0       440.0         20.00")
  val tle29141 = TLE.parseLines(lines29141).head
  val start29141 = 0
  val end29141 = 440
  val step29141 = 20
}

object TLE29141 extends TestTLE with TLE29141 {
  def lines: List[String] = lines29141
  def tle: TLE = tle29141
  def start: Int = start29141
  def end: Int = end29141
  def step: Int =  step29141
}

trait TLE29238 {
  val lines29238= List(
  "#   SL-12 DEB           # Near Earth, perigee = 212.24 < 220",
  "#                       # simplified drag eq",
  "1 29238U 06022G   06177.28732010  .00766286  10823-4  13334-2 0   101",
  "2 29238  51.5595 213.7903 0202579  95.2503 267.9010 15.73823839  1061      0.0      1440.0        120.00")
  val tle29238 = TLE.parseLines(lines29238).head
  val start29238 = 0
  val end29238 = 1440
  val step29238 = 12
}

object TLE29238 extends TestTLE with TLE29238 {
  def lines: List[String] = lines29238
  def tle: TLE = tle29238
  def start: Int = start29238
  def end: Int = end29238
  def step: Int =  step29238
}

trait TLE88888 {
  val lines88888= List(
  "#                       # drop certain normal drag terms",
  "1 88888U 03049A   06177.78615833  .00000060  00000-0  35940-4 0  1836",
  "2 88888  98.4283 247.6961 0000884  88.1964 271.9322 14.35478080140550      0.0      2880.0        120.00")
  val tle88888 = TLE.parseLines(lines88888).head
  val start88888 = 0
  val end88888 = 2880
  val step88888 = 12
}

object TLE88888 extends TestTLE with TLE88888 {
  def lines: List[String] = lines88888
  def tle: TLE = tle88888
  def start: Int = start88888
  def end: Int = end88888
  def step: Int =  step88888
}
