package predict4s.report

import predict4s.sgp._
import predict4s.coord._
import spire.implicits._
import spire.math._
import predict4s.sgp.ref.SGP4Vallado

object PropagateTLE28xxx extends App {
  
  def report(propag: Propagate) : Unit = {
    Console.out.println(s"### TLE${propag.ttle.tle.satelliteNumber}")
    Console.out.println(s"Propagations size is ${propag.pvts.size}")  
    val glen = propag.goods.size
    Console.out.println(s"Good's size is ${glen}")
    Console.out.println(s"Last good is at epoch time ${propag.goods(glen-1)._2}")
    val elen = propag.errors.size
    Console.out.println(s"Error's size is ${elen}")
    if (elen > 0) {
      Console.out.println(s"First error is ${propag.errors.head._1} at epoch time ${propag.errors.head._2}")
    }
    Console.out.println(s"Comparison with Vallado's cpp resutls")
    val cglen = propag.cppOuts.size
    Console.out.println(s"Cpp good's size is ${cglen}")
    Console.out.println(s"Last cpp good is at epoch time ${propag.cppOuts(cglen-1).t}")
    Console.out.println("time, pv differences : ")
    propag.pvDiffs map { println }
    Console.out.println("###")
  }

  report(new Propagate(TLE28350))
  report(new Propagate(TLE28872))

}


