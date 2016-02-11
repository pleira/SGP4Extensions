package predict4s.sgp.ref

import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm

/**
 * The purpose of this benchmark is to measure and compare the different algorithms
 */
import scala.math._
import predict4s.coord.SGP72Constants
import predict4s.sgp._
import predict4s.coord.SGPElemsConversions
import predict4s.coord.SpecialPolarNodal
import predict4s.collision.TLE22675
import predict4s.collision.TLE24946
import predict4s.coord.LaraConversions._

object Sgp4ImplBench extends App with TLE22675 with TLE24946 with TLE00005  with TLE06251 with TLE28057 {
 
  implicit val wgs = SGP72Constants.tleDoubleConstants

  val th = Thyme.warmed(verbose = print)
  
  import spire.std.any.DoubleAlgebra
  val tles = List(tle00005 ,tle06251,tle22675,tle24946,tle28057)
  for (tle <- tles) {
    val elem0AndCtx = SGPElemsConversions.sgpElemsAndContext(tle, wgs).get
    val model = BrouwerLaneSecularCorrections(elem0AndCtx)
    val vasgp4 = SGP4Vallado[Double](model)
    val pnsgp4 = SGP4PN[Double](model)
    val vlsgp4 = SGP4ValladoLong[Double](model)
    val lasgp4 = SGP4Lara[Double](model)
    val ts = Vector.range(0, 3000, 5)    
    val secs = ts map { vasgp4.secularCorrections(_).get } 
    val vapc = th.Warm(secs.map(s => vasgp4.periodicCorrections(s)))
    val pnpc = th.Warm(secs.map(s => pnsgp4.periodicCorrections(s)))
    
    th.pbenchOffWarm(s"Comparison Vallado/PN for TLE ${tle.satelliteNumber}")(vapc)(pnpc)  
    
    val lapc = th.Warm(secs.map(s => lasgp4.periodicCorrectionsSPN(s)))
    val vlpc = th.Warm(secs.map(s => vlsgp4.periodicCorrections(s)))
    
    th.pbenchOffWarm(s"Comparison Lara/Vallado Long for TLE ${tle.satelliteNumber}")(lapc)(vlpc)  
    
  }

} 
