package predict4s
package conjunction

import predict4s.coord.TLE;

/* These two satellites collided on 10 February 2009, at 16:56 GMT */

trait TLE24946 {
  val lines24946 = List(
"#      IRIDIUM 33: last TLE before collision",             
"1 24946U 97051C   15313.53802329  .00000228  00000-0  75380-4 0  9991",
"2 24946  86.3898 171.8504 0007925   6.3243  13.7073 14.33431024950141")

  val tle24946 = TLE.parseLines(lines24946).head
}

trait TLE22675 {
  val lines22675 = List(
"# COSMOS 2251 : last TLE before collision",             
"1 22675U 93036A   15312.87914574  .00000046  00000-0  26387-4 0  9990",
"2 22675  74.0398 206.0906 0023974 108.6227 321.7254 14.32449027170085")

  val tle22675 = TLE.parseLines(lines22675).head
}
