package predict4s.report

import predict4s.sgp._
import predict4s.coord._
import spire.implicits._
import spire.math._
import predict4s.sgp.ref.SGP4Vallado


class Propagate(ttle: TestTLE) extends LoadCppResults {
  
  val cppOuts = loadCppOutputData(ttle.tle.satelliteNumber)
  
  val vasgp4 = SGP4Vallado.build(ttle.tle, SGP72Constants.tleDoubleConstants).get
  
  val sgpOuts =
    for {
      t <- ttle.start to ttle.end by ttle.step 
      r = vasgp4.propagate(t.toDouble) 
    } yield r

  // Check that results contain ErrorMessages from the propagation
  val errors = sgpOuts.filter(r => r.isBad) 

  def propagate(t: Double) = vasgp4.propagate(t)
//   def apply(tle: TLE) = new 
}

object CheckBadTLE22312 extends App {
  val p22312 = new Propagate(TLE22312)
  
  // here, it was starting by a double
  // val out0 = p22312.propagate(TLE22312.start22312)
  // Console.out.println(s"${out0}")
  
  // Check that results contain ErrorMessages from the propagation
  Console.out.println(s"Propagations size is ${p22312.sgpOuts.size}")
  
  Console.out.println(s"Error's size is ${p22312.errors.size}")
  
}