package predict4s.report

import predict4s.coord._
import predict4s.sgp.TestTLE
import predict4s.sgp.ref._
import spire.implicits._
import spire.algebra.Field
import spire.math._

class AllAlgos(val tle: TLE, val wgs: SGPConstants[Double], val start: Int, val end: Int, val step: Int)  {
  val times = new Range(start, end, step) // times in minutes  
  val vasgp4 = SGP4Vallado.build(tle, wgs).get
  val pnsgp4 = SGP4PN(vasgp4.sec)
  val vlsgp4 = SGP4ValladoLong(vasgp4.sec)
  val lasgp4 = SGP4Lara(vasgp4.sec)
  val sgp4s = List(vasgp4, vlsgp4, pnsgp4, lasgp4)
  val results = sgp4s map { sgp4 =>
    for (t <- start to end by step) yield sgp4.propagate(t).get
  }
  val varesult = results(0)
  val vlresult = results(1)
  val pnresult = results(2)
  val laresult = results(3) // for (t <- start to end by step) yield lasgp4.propagate(t).get

  val finalCartesiansResults = results map { l =>
    l map { tuple => tuple._1 } 
  }
  val vafc = finalCartesiansResults(0)
  val vlfc = finalCartesiansResults(1)
  val pnfc = finalCartesiansResults(2)
  val lafc = finalCartesiansResults(3) // laresult map { tuple => tuple._1 } 
  
  val finalPNResults = results map { l =>
    l map { tuple => tuple._3 } 
  }
  val vafpn = finalPNResults(0)
  val vlfpn = finalPNResults(1)
  val pnfpn = finalPNResults(2)
  val lafpn = finalPNResults(3) // laresult map { tuple => tuple._3._1._1 } 
  
//  val lppPNResults = results map { l =>
//    l map { tuple => tuple._3._1._2 } 
//  }
//  val lpvpn = lppPNResults(0)
//  val lpvlpn = lppPNResults(1)
//  val lppnpn = lppPNResults(2)
  //val lplpn = lppPNResults(3)
  
  val unitCartesianPositions = results map { l =>
    l map { tuple => tuple._2 } 
  }
    
//  def calcPropagation(sgp4: SGP4[Double], start: Int, end: Int, step: Int) = {
//    for (t <- start to end by step) yield sgp4.propagate(t)
//  }
  
  def algorithmNames = List("va", "vl", "pn", "la")
}

object AllAlgos {
  def apply(o: TestTLE) = new AllAlgos(o.tle, SGP72Constants.tleDoubleConstants, o.start, o.end, o.step)
  def createWithWGS84(o: TestTLE) = new AllAlgos(o.tle, SGP84Constants.tleDoubleConstants, o.start, o.end, o.step)
}

