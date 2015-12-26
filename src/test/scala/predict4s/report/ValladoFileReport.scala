package predict4s.report
import predict4s.sgp.HelperTypes
import predict4s.coord.TLE
import better.files._
import java.io.{File => JFile}
import predict4s.coord.SpecialPolarNodal
import predict4s.coord.SGPElems
import predict4s.sgp.SGP4
import predict4s.sgp.vallado.SGP4Vallado
import predict4s.sgp.EccentricAnomalyState

object ValladoFileReport {

  type F = Double
  // Hardcode the type at the moment
  type ShortPeriodCorrections = SpecialPolarNodal[F]
  type FinalState = SpecialPolarNodal[F]
  type ShortPeriodState = (SpecialPolarNodal[F], ShortPeriodCorrections) // final values, corrections ShortPeriodPolarNodalContext
  type LongPeriodState = (SpecialPolarNodal[F], F, F, F, F, F, F) // final values, context variables
  type EccentricAState = EccentricAnomalyState[F]
  
  def save(pnr: IndexedSeq[((FinalState, ShortPeriodState, LongPeriodState, EccentricAState), SGPElems[F])], tle: TLE, lines: List[String], times : IndexedSeq[Int]) = {
    val f = File("doc/reports/sgp4vallado_"+tle.satelliteNumber + "_" + tle.year + "_" + tle.epoch + ".md")
    implicit val oo = File.OpenOptions.append
    f << "# Vallado SGP4 algorithm\n"
    
    f << "\n\n#### Input TLE\n"
    lines foreach (f << "  " + _ + "\n")
        
    f << "\n\n#### propagation times in min " 
    f << times.toString() < "\n"  
    //f < "# " < tle.satelliteNumber.toString() < " " < tle.year.toString() < " " < tle.epoch < "\n"
    
    f << "\n\n#### Computed Final State in Special Polar Nodal coordinates\n"  
    f << "| the orbital inclination | the argument of latitude | the node  | the radial distance | the radial velocity | `Θ/r` "
    f << "| ----------------------- | -----------------------  | --------- | ------------------- | ------------------- | ----- "
    pnr foreach { p => 
      f << p._1._1.toString().replaceAll(",|SpecialPolarNodal\\(|\\)|\\(", "|")    
    }
    
    f << "\n\n#### Short period corrections in Special Polar Nodal coordinates\n"  
    f << "| the orbital inclination | the argument of latitude | the node | the radial distance | the radial velocity | `Θ/r` "
    f << "| ----------------------- | -----------------------  | --------- | ------------------- | ------------------- | ----- "
   pnr foreach { p => 
      f << p._1._2._2.toString().replaceAll(",|SpecialPolarNodal\\(|\\)|\\(", "|")          
    }
    
    f << "\n\n#### Long period state in Special Polar Nodal coordinates\n"  
    f << "| the orbital inclination | the argument of latitude | the node | the radial distance | the radial velocity | `Θ/r` "
    f << "| ----------------------- | -----------------------  | --------- | ------------------- | ------------------- | ----- "
 
    pnr foreach { p => 
      f << p._1._3._1.toString().replaceAll(",|SpecialPolarNodal\\(|\\)|\\(", "|")      
    }
    
    f << "\n\n#### Anomaly calculation \n"
    f << "| E | cosE | sinE | ecosE | esinE" 
    f << "| ----------------------- | -----------------------  | --------- | ------------------- | ------------------- | ----- "
    pnr foreach { p => 
      f << p._1._4.toString().replaceAll(",|EccentricAnomalyState\\(|\\)|\\(", "|")            
    }
    
    f << "\n\n#### Orbital elements with secular corrections \n" 
    f << "| mean motion | eccentricity | inclination | argument Of perigee | ascending node | mean anomaly | semimajor axis | atmospheric Drag | epoch time in days from jan 0 1950. 0 hr"
    f << "| ----------- | -----------  | --------- | ------------------- | ------------------- | --------- | ---------      | ---------        | --------------   "
    pnr foreach { p => 
      f << p._2.toString().replaceAll(",|SGPElems\\(|\\)|\\(", "|")  
    }

  }  
}
