package predict4s.report

import predict4s.sgp._
import predict4s.coord._
import spire.implicits._
import spire.math._
import predict4s.sgp.ref.SGP4Vallado


class Propagate(val ttle: TestTLE) extends LoadCppResults {
  
  val cppOuts = loadCppOutputData(ttle.tle.satelliteNumber)
  
  val vasgp4 = SGP4Vallado.build(ttle.tle, SGP72Constants.tleDoubleConstants).get
  
  def pvts =
    for {
      t <- ttle.start to ttle.end by ttle.step 
      pv = vasgp4.propagate(t.toDouble) 
    } yield (pv, t.toDouble)

  // Check that results contain ErrorMessages from the propagation
  def errors = pvts.filter(pvt => pvt._1.isBad)
  // here should be flatMap
  def goods = pvts.filter{pvt => pvt._1.isGood } // .map(_.get)
  def propagate(t: Double) = vasgp4.propagate(t)
  def goodProp = goods map {p => { val r = p._1.get; (r._1, p._2) } }
  
  val pvDiffs = cppOuts zip goodProp map diffPosVelCartesian

  def diffPosVelCartesian(opv : (CartesianPNLppUnit,(CartesianElems[Double], Double))) : (Double, CartesianElems[Double]) = {
    val o = opv._1
    val p = opv._2
    val c = p._1
    val t = p._2
    if (t == o.t.toDouble) 
    (t, CartesianElems(o.x.toDouble - c.x, o.y.toDouble - c.y, o.z.toDouble - c.z,
            o.vx.toDouble - c.vx, o.vy.toDouble - c.vy, o.vz.toDouble - c.vz))
    else 
    (o.t.toDouble, CartesianElems(t,-9,-9,-9,-9,-9))
  }
}

class Propagate22312D extends Propagate(TLE22312) {
  val seq = for {
      t <- TLE22312.startD to TLE22312.end by TLE22312.step.toDouble 
      pv = vasgp4.propagate(t) 
    } yield (pv, t)
  val sgpOutsD = (vasgp4.propagate(0),0.0) +: seq 
  // Check that results contain ErrorMessages from the propagation
  override def errors = sgpOutsD.filter(pvt => pvt._1.isBad)
  override def goods = sgpOutsD.filter(pvt => pvt._1.isGood)
}

object CheckBadTLE22312 extends App {
  val p22312 = new Propagate22312D
  
  Console.out.println(s"Propagations size is ${p22312.sgpOutsD.size}")  
  val glen = p22312.goods.size
  Console.out.println(s"Good's size is ${glen}")
  Console.out.println(s"Last good is at epoch time ${p22312.goods(glen-1)._2}")
  Console.out.println(s"Error's size is ${p22312.errors.size}")
  Console.out.println(s"First error is ${p22312.errors.head._1} at epoch time ${p22312.errors.head._2}")
  // last from vasgp4
  // CartesianElems(-1276.5553218406549,4553.268984595287,4406.197873778597,-3.715146421392648,-5.3201769144326105,4.418210777045478)
  // error is Secular eccentricity -2.611834722557327E-5 outside valid range at epoch time 474.2028672
  Console.out.println(s"Comparison with Vallado's cpp resutls")
  val cglen = p22312.cppOuts.size
  Console.out.println(s"Cpp good's size is ${cglen}")
  Console.out.println(s"Last cpp good is at epoch time ${p22312.cppOuts(cglen-1).t}")
  
}