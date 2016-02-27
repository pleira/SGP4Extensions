package predict4s
package conjunction


import spire.math._
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.sgp.ref.SGP4Vallado
import predict4s.coord.SGPElemsConversions
import predict4s.coord.SpecialPolarNodal

// the idea is to find out algorithms that give interesting propagation times for conjunction analysis
// In this case, the problem is about relating the classical elements of two satellites in different orbits
// with one another. That requires a relation for the transformation of the elements in one orbit to the other,
// in particular, the mean anomaly.
object ConjunctionInterpreterExperiment1 extends App with TLE22675 with TLE24946 {

  val wgs = SGP72Constants.tleDoubleConstants

  import spire.std.any.DoubleAlgebra
  val tles = List(tle22675,tle24946)
  val sgp4s = for {
    tle <- tles
    elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs).get
    model = BrouwerLaneSecularCorrections(elem0AndCtx)
  } yield SGP4Vallado[Double](model)

  // we have initialized the propagators for the 2 TLEs.
  val sgp22675 = sgp4s.head
  val sgp24946 = sgp4s.tail.head

  // before propagation, aligning their epoch times, which are different in each TLE
  val jd22675 = tle22675.epoch.toDouble
  val jd24946 = tle24946.epoch.toDouble
  val jddiff = abs(jd24946 - jd22675)
  // in minutes
  val diffMinutes = jddiff * 24 * 60

  // now, propagate secular
  var t = 0.0;
  import scala.collection.mutable.ListBuffer
  val lb = new ListBuffer[Double]
  while (t < 2*1440) {
    val (s0t,_,_) = sgp24946.secularCorrections(t).get
    val (s1t,_,_) = sgp22675.secularCorrections(t + diffMinutes).get
    val m1ω = (s1t.M - s1t.ω)
    val m0ω = (s0t.M - s0t.ω)
    val dm = (m1ω - m0ω) % (2*pi)
    val dΩ = s1t.Ω - s0t.Ω
    val dt =
      if (dm > pi/4) 10
      else if (dm > pi/8) 3
      else {
        // IGNORE: just some trials
        // if both s0t.M or s1t.M are near the inclination of the other orbit)
        if (Interval(s1t.I-0.1,s1t.I+0.1).contains(m0ω) &&
            Interval(s0t.I-0.1,s0t.I+0.1).contains(m1ω)) {
          lb += t
          1
        } else if (Interval(s1t.I-0.2,s1t.I+0.2).contains(m0ω) &&
            Interval(s0t.I-0.2,s0t.I+0.2).contains(m1ω)) {
          2
        } else if (Interval(s1t.I-0.3,s1t.I+0.3).contains(m0ω) &&
            Interval(s0t.I-0.3,s0t.I+0.3).contains(m1ω)) {
          3
        } else
          10
      }
    t += dt

  }

  // We have some collection of interesting times
  Console.println(lb)

  // TODO analyse conjunctions in those times, going for more detailed propagations

}
