package predict4s.sgp

import org.scalatest.FunSuite
import org.scalactic.TolerantNumerics
import org.scalactic.Equality

trait ValladoNearTLEsCheck[F] extends ValladoTLE00005Check[F] with ValladoTLE06251Check[F] with ValladoTLE28057Check[F] { self :  FunSuite => 

  val check00005  = List(checkSgp4_5_0 ,checkSgp4_5_360 , sgp4CheckSat5_720 , sgp4CheckSat5_1080 , sgp4CheckSat5_1440 , sgp4CheckSat5_1800 , sgp4CheckSat5_2160 ,
               sgp4CheckSat5_2520 , sgp4CheckSat5_2880 , sgp4CheckSat5_3240 , sgp4CheckSat5_3600 , sgp4CheckSat5_3960 , sgp4CheckSat5_4320)
  
  val times00005  = for (i <- 0 until check00005.size; j = i*360) yield j // times in minutes 

  val check06251  = List(sgp4CheckSat6251_0 ,sgp4CheckSat6251_120 ,sgp4CheckSat6251_240 ,sgp4CheckSat6251_360 ,sgp4CheckSat6251_480 ,sgp4CheckSat6251_600 ,
      sgp4CheckSat6251_720 ,sgp4CheckSat6251_840 ,sgp4CheckSat6251_960 ,sgp4CheckSat6251_1080 ,sgp4CheckSat6251_1200 ,sgp4CheckSat6251_1320 ,sgp4CheckSat6251_1440 ,
      sgp4CheckSat6251_1560 ,sgp4CheckSat6251_1680 ,sgp4CheckSat6251_1800 ,sgp4CheckSat6251_1920 ,sgp4CheckSat6251_2040 ,sgp4CheckSat6251_2160 ,sgp4CheckSat6251_2280 ,
      sgp4CheckSat6251_2400 ,sgp4CheckSat6251_2520 ,sgp4CheckSat6251_2640 ,sgp4CheckSat6251_2760 ,sgp4CheckSat6251_2880 )
  
  val times06251  = for (i <- 0 until check06251.size; j = i*120) yield j // times in minutes
  
  val check28057  = List(sgp4CheckSat28057_0 ,sgp4CheckSat28057_120 ,sgp4CheckSat28057_240 ,sgp4CheckSat28057_360 ,sgp4CheckSat28057_480 ,sgp4CheckSat28057_600 ,
      sgp4CheckSat28057_720 ,sgp4CheckSat28057_840 ,sgp4CheckSat28057_960 ,sgp4CheckSat28057_1080 ,sgp4CheckSat28057_1200 ,sgp4CheckSat28057_1320 ,sgp4CheckSat28057_1440 ,
      sgp4CheckSat28057_1560 ,sgp4CheckSat28057_1680 ,sgp4CheckSat28057_1800 ,sgp4CheckSat28057_1920 ,sgp4CheckSat28057_2040 ,sgp4CheckSat28057_2160 ,sgp4CheckSat28057_2280 ,
      sgp4CheckSat28057_2400 ,sgp4CheckSat28057_2520 )
      
  val times28057   = for (i <- 0 until check28057.size; j = i*120) yield j // times in minutes 
  
  def tlesTimes = List(times00005, times06251, times28057)
}

trait ValladoNearTLEsPVCheck[F] extends ValladoTLE00005PVCheck[F] with ValladoTLE06251PVCheck[F] with ValladoTLE28057PVCheck[F] { self :  FunSuite =>  

  val pvCheck00005  = List(pvCheckSgp4_5_0 ,pvCheckSgp4_5_360 , sgp4PVCheckSat5_720 , sgp4PVCheckSat5_1080 , sgp4PVCheckSat5_1440 , sgp4PVCheckSat5_1800 , sgp4PVCheckSat5_2160 ,
               sgp4PVCheckSat5_2520 , sgp4PVCheckSat5_2880 , sgp4PVCheckSat5_3240 , sgp4PVCheckSat5_3600 , sgp4PVCheckSat5_3960 , sgp4PVCheckSat5_4320 )
  
  val pvCheck06251  = List(sgp4PVCheckSat6251_0 ,sgp4PVCheckSat6251_120 ,sgp4PVCheckSat6251_240 ,sgp4PVCheckSat6251_360 ,sgp4PVCheckSat6251_480 ,sgp4PVCheckSat6251_600 ,
      sgp4PVCheckSat6251_720 ,sgp4PVCheckSat6251_840 ,sgp4PVCheckSat6251_960 ,sgp4PVCheckSat6251_1080 ,sgp4PVCheckSat6251_1200 ,sgp4PVCheckSat6251_1320 ,sgp4PVCheckSat6251_1440 ,
      sgp4PVCheckSat6251_1560 ,sgp4PVCheckSat6251_1680 ,sgp4PVCheckSat6251_1800 ,sgp4PVCheckSat6251_1920 ,sgp4PVCheckSat6251_2040 ,sgp4PVCheckSat6251_2160 ,sgp4PVCheckSat6251_2280 ,
      sgp4PVCheckSat6251_2400 ,sgp4PVCheckSat6251_2520 ,sgp4PVCheckSat6251_2640 ,sgp4PVCheckSat6251_2760 ,sgp4PVCheckSat6251_2880 )
  
  
  val pvCheck28057  = List(sgp4PVCheckSat28057_0 ,sgp4PVCheckSat28057_120 ,sgp4PVCheckSat28057_240 ,sgp4PVCheckSat28057_360 ,sgp4PVCheckSat28057_480 ,sgp4PVCheckSat28057_600 ,
      sgp4PVCheckSat28057_720 ,sgp4PVCheckSat28057_840 ,sgp4PVCheckSat28057_960 ,sgp4PVCheckSat28057_1080 ,sgp4PVCheckSat28057_1200 ,sgp4PVCheckSat28057_1320 ,sgp4PVCheckSat28057_1440 ,
      sgp4PVCheckSat28057_1560 ,sgp4PVCheckSat28057_1680 ,sgp4PVCheckSat28057_1800 ,sgp4PVCheckSat28057_1920 ,sgp4PVCheckSat28057_2040 ,sgp4PVCheckSat28057_2160 ,sgp4PVCheckSat28057_2280 ,
      sgp4PVCheckSat28057_2400 ,sgp4PVCheckSat28057_2520 )
      
  val pvNearChecks = List(pvCheck00005, pvCheck06251, pvCheck28057)
}

// FIXME : assert(  xlcof  ===    0.001289058 ); assert(  aycof  ===   0.000660216 ); assert(  delmo  ===  4.873084659 );
// 
trait  ValladoTLE00005Check[F] { self :  FunSuite => 
                                   // ------------------after initl  :---------------
val checkIntl5 = (ini : Initl[F]) => { implicit equ: Equality[F] =>
  import ini._
  assert(   satn  ===               5 );  assert(   ecco  ===    0.185966700 );  assert(  epoch  === 18441.784950620 );  assert(  inclo  ===     0.598092919 ); 
  //   in/out : 
  assert(     no  ===     0.047206302 ); 
  //   outputs : 
  assert(   ainv  ===     0.738607085 ); assert(     ao  ===     1.353899821 ); assert(  cosio  ===     0.826410932 ); 
  assert( cosio2  ===     0.682955029 ); 
/*  assert(  einx   ===     0.034583614 ); */ assert(  eccsq  ===     0.034583614 ); assert(  omeosq ===     0.965416386 ); assert(   posq  ===     1.708450473 ); assert(     rp  ===     1.102119539 ); assert(  rteosq ===     0.982556048 ); 
  assert(  sinio  ===     0.563067465 ); assert(   gsto  ===     3.469172342 ); 
} 
       // ------------------after sgp4init :-------------
val checkSgp4Init5 = (ini: Sgp4Init[F]) => { implicit equ: Equality[F] =>
  import ini._
assert(   satn  ===               5); assert(     yr  ===               0); assert(  bstar  ===     0.000028098); assert(   ecco  ===     0.185966700);  assert(  epoch  === 18441.784950620);  assert(  argpo  ===     5.790416027 ); 
assert(  inclo  ===     0.598092919); assert(     mo  ===     0.337309313 ); 
// in and out variables 
assert(     no  ===     0.047206302 ); 
//    outputs  :
 assert(  isimp  ===               0);  
assert(  con41  ===     1.048865088); assert(    cc1  ===     0.000000000); assert(    cc4  ===     0.000000526 ); 
assert(    cc5  ===     0.000016465); assert(     d2  ===     0.000000000); assert(     d3  ===     0.000000000); assert(     d4  ===     0.000000000);  
assert(    eta  ===     0.736909543); 
assert( argpdot ===     0.000054293); assert(  omgcof ===     0.000000000 ); 
assert( sinmao  ===     0.330949230); assert(  t2cof  ===     0.000000000); assert(  t3cof  ===     0.000000000 ); 
assert(  t4cof  ===     0.000000000); assert(  t5cof  ===     0.000000000); assert(   gsto  ===     3.469172342); assert( x1mth2  ===     0.317044971); assert( x7thm1  ===     3.780685205); 
assert(  xmcof  ===    -0.000000000); assert(   mdot  ===     0.047229443); assert(  nodecf ===    -0.000000000); assert(  nodedt ===    -0.000037171 ); 
assert(      t  ===     0.000000000); assert(  nodeo  ===     6.086385471 ); 
} 
//                                     ------------------after sgp4   :---------------
val checkSgp4_5_0 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  bstar  ===     0.000028098 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===     0.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  7022.465292664 ); assert(      y  === -1400.082967554 ); assert(      z  ===     0.039951554 ); assert(   xdot  ===     1.893841015 ); assert(   ydot  ===     6.405893759 ); assert(   zdot  ===     4.534807250 ); 
} 
val checkSgp4_5_360 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  bstar  ===     0.000028098 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===   360.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -7154.031202016 ); assert(      y  === -3783.176825037 ); assert(      z  === -3536.194122942 ); assert(   xdot  ===     4.741887409 ); assert(   ydot  ===    -4.151817765 ); assert(   zdot  ===    -2.093935425 ); 
} 
val checkSgp4_5_720 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  bstar  ===     0.000028098 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===   720.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -7134.593401193 ); assert(      y  ===  6531.686413336 ); assert(      z  ===  3260.271864826 ); assert(   xdot  ===    -4.113793027 ); assert(   ydot  ===    -2.911922039 ); assert(   zdot  ===    -2.557327851 ); 
} 
// 5
        // ------------------after sgp4init :-------------
val checkSgp4Init6251 = (ini: Sgp4Init[F]) => { implicit equ: Equality[F] =>
 import ini._
assert(   satn  ===            6251); assert(     yr  ===               6); assert(  bstar  ===     0.000128080); assert(   ecco  ===     0.003003500);  assert(  epoch  === 20630.824120140);  assert(  argpo  ===     2.428744337 ); 
assert(  inclo  ===     1.013301512); assert(     mo  ===     3.860413487 ); 
// in and out variables 
assert(     no  ===     0.067918037 ); 
//    outputs  :
 assert(  isimp  ===               0);  assert(  aycof  ===     0.000994993 ); 
assert(  con41  ===    -0.160280193); assert(    cc1  ===     0.000000003); assert(    cc4  ===     0.000005200 ); 
assert(    cc5  ===     0.000650194); assert(     d2  ===     0.000000000); assert(     d3  ===     0.000000000); assert(     d4  ===     0.000000000); 
assert(    eta  ===     0.063675056); 
assert( argpdot ===     0.000019407); assert(  omgcof ===    -0.000000052 ); 
assert( sinmao  ===    -0.658497710); assert(  t2cof  ===     0.000000004); assert(  t3cof  ===     0.000000000 ); 
assert(  t4cof  ===     0.000000000); assert(  t5cof  ===     0.000000000); assert(   gsto  ===     3.673754968); assert( x1mth2  ===     0.720093398); assert( x7thm1  ===     0.959346217); 
assert(  xmcof  ===    -0.000133147); assert(   mdot  ===     0.067910210); assert(  nodecf ===    -0.000000000); assert(  nodedt ===    -0.000051677 ); 
//   in and outputs from deep space satellites :
assert(      t  ===     0.000000000); assert(  nodeo  ===     0.943219561 ); 
} 
// 6251
                                   // ------------------after initl  :---------------
val checkIntl28057 = (ini : Initl[F]) => { implicit equ: Equality[F] =>
 import ini._
  assert(   satn  ===           28057 );  assert(   ecco  ===     0.000088400 );  assert(  epoch  === 20631.786158330 );  assert(  inclo  ===     1.717897912 ); 
 //   in/out : 
  assert(     no  ===     0.062672399 ); 
 //   outputs : 
    assert(   ainv  ===     0.892204404 ); assert(     ao  ===     1.120819394 ); assert(  cosio  ===    -0.146571640 ); 
  assert( cosio2  ===     0.021483246 ); 
/*  assert(  einx   ===     0.000000008 ); */ assert(  eccsq  ===     0.000000008 ); assert(  omeosq ===     0.999999992 ); assert(   posq  ===     1.256236095 ); assert(     rp  ===     1.120720314 ); assert(  rteosq ===     0.999999996 ); 
  assert(  sinio  ===     0.989200058 ); assert(   gsto  ===     3.451783622 ); 
} 
       // ------------------after sgp4init :-------------
val checkSgp4Init28057 = (ini: Sgp4Init[F]) => { implicit equ: Equality[F] =>
 import ini._
assert(   satn  ===           28057); assert(     yr  ===               6); assert(  bstar  ===     0.000035940); assert(   ecco  ===     0.000088400);  assert(  epoch  === 20631.786158330);  assert(  argpo  ===     1.539317568 ); 
assert(  inclo  ===     1.717897912); assert(     mo  ===     4.746112232 ); 
// in and out variables 
assert(     no  ===     0.062672399 ); 
//    outputs  :
 assert(  isimp  ===               0);  assert(  aycof  ===     0.001159872 ); 
assert(  con41  ===    -0.935550263); assert(    cc1  ===     0.000000000); assert(    cc4  ===    -0.000000044 ); 
assert(    cc5  ===     0.000030313); assert(     d2  ===     0.000000000); assert(     d3  ===     0.000000000); assert(     d4  ===     0.000000000); 
assert(    eta  ===     0.000912426); assert( argpdot ===    -0.000036077); assert(  omgcof ===     0.000000000 ); 
assert( sinmao  ===    -0.999431425); assert(  t2cof  ===     0.000000000); assert(  t3cof  ===     0.000000000 ); 
assert(  t4cof  ===     0.000000000); assert(  t5cof  ===     0.000000000); assert(   gsto  ===     3.451783622); assert( x1mth2  ===     0.978516754); assert( x7thm1  ===    -0.849617280); 
assert(  xmcof  ===     0.000000000); assert(   mdot  ===     0.062634526); assert(  nodecf ===     0.000000000); assert(  nodedt ===     0.000011840 ); 
//   in and outputs from deep space satellites :
assert(      t  ===     0.000000000); assert(  nodeo  ===     4.323112489 ); 
} 
// 28057
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat5_720 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); 
 assert(  bstar  ===     0.000028098 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===   720.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -7134.593401193 ); assert(      y  ===  6531.686413336 ); assert(      z  ===  3260.271864826 ); assert(   xdot  ===    -4.113793027 ); assert(   ydot  ===    -2.911922039 ); assert(   zdot  ===    -2.557327851 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat5_1080 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); 
 assert(  bstar  ===     0.000028098 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===  1080.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  5568.539011812 ); assert(      y  ===  4492.069925906 ); assert(      z  ===  3863.876419829 ); assert(   xdot  ===    -4.209106476 ); assert(   ydot  ===     5.159719888 ); assert(   zdot  ===     2.744852980 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat5_1440 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  bstar  ===     0.000028098 ); assert(  con41  ===     1.048865088 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===  1440.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  -938.559239429 ); assert(      y  === -6268.187488314 ); assert(      z  === -4294.029247512 ); assert(   xdot  ===     7.536105209 ); assert(   ydot  ===    -0.427127707 ); assert(   zdot  ===     0.989878080 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat5_1800 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); 
 assert(  bstar  ===     0.000028098 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===  1800.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -9680.561217281 ); assert(      y  ===  2802.477713539 ); assert(      z  ===   124.106880382 ); assert(   xdot  ===    -0.905874102 ); assert(   ydot  ===    -4.659467970 ); assert(   zdot  ===    -3.227347517 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat5_2160 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  bstar  ===     0.000028098 ); assert(  con41  ===     1.048865088 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===  2160.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===   190.197969879 ); assert(      y  ===  7746.966536135 ); assert(      z  ===  5110.006754119 ); assert(   xdot  ===    -6.112325142 ); assert(   ydot  ===     1.527008184 ); assert(   zdot  ===    -0.139152358 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat5_2520 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  bstar  ===     0.000028098 ); assert(  con41  ===     1.048865088 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===  2520.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  5579.556401157 ); assert(      y  === -3995.613967894 ); assert(      z  === -1518.821089660 ); assert(   xdot  ===     4.767927483 ); assert(   ydot  ===     5.123185301 ); assert(   zdot  ===     4.276837355 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat5_2880 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  bstar  ===     0.000028098 ); assert(  con41  ===     1.048865088 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===  2880.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -8650.730822189 ); assert(      y  === -1914.938115252 ); assert(      z  === -3007.036034428 ); assert(   xdot  ===     3.067165127 ); assert(   ydot  ===    -4.828384068 ); assert(   zdot  ===    -2.515322836 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat5_3240 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  bstar  ===     0.000028098 ); assert(  con41  ===     1.048865088 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===  3240.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -5429.792041645 ); assert(      y  ===  7574.364937924 ); assert(      z  ===  3747.393052359 ); assert(   xdot  ===    -4.999442110 ); assert(   ydot  ===    -1.800561422 ); assert(   zdot  ===    -2.229392830 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat5_3600 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  bstar  ===     0.000028098 ); assert(  con41  ===     1.048865088 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===  3600.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  6759.045837218 ); assert(      y  ===  2001.581982197 ); assert(      z  ===  2783.551925329 ); assert(   xdot  ===    -2.180993947 ); assert(   ydot  ===     6.402085603 ); assert(   zdot  ===     3.644723952 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat5_3960 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  bstar  ===     0.000028098 ); assert(  con41  ===     1.048865088 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===  3960.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -3791.445315589 ); assert(      y  === -5712.956178939 ); assert(      z  === -4533.486307144 ); assert(   xdot  ===     6.668817493 ); assert(   ydot  ===    -2.516382327 ); assert(   zdot  ===    -0.082384354 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat5_4320 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  bstar  ===     0.000028098 ); assert(  con41  ===     1.048865088 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===     0.000000526 ); assert(    cc5  ===     0.000016465 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 );  
 assert(   ecco  ===     0.185966700 ); assert(    eta  ===     0.736909543 ); assert(  argpo  ===     5.790416027 ); assert( argpdot ===     0.000054293 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===     0.330949230 ); 
 assert(      t  ===  4320.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.317044971 ); 
 assert(  x7thm1 ===     3.780685205 ); assert(  inclo  ===     0.598092919 ); assert(     mo  ===     0.337309313 ); assert(   mdot  ===     0.047229443 ); assert(    xno  ===     0.047206302 ); assert(  nodeo  ===     6.086385471 ); 
 assert( nodedt  ===    -0.000037171 ); assert(  xmcof  ===    -0.000000000 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -9060.473735694 ); assert(      y  ===  4658.709525023 ); assert(      z  ===   813.686731534 ); assert(   xdot  ===    -2.232832783 ); assert(   ydot  ===    -4.110453490 ); assert(   zdot  ===    -3.157345433 ); 
} 
}

// FIXME : assert(  xlcof  ===    0.001836762 );  assert(  aycof  ===     0.000994993 );  assert(  delmo  ===   0.863016906 );
trait ValladoTLE06251Check[F] { self :  FunSuite => 
  // implicit def doubleEqualityTLE06251 = TolerantNumerics.tolerantDoubleEquality(1E-9)
// ------------------after initl  :---------------
val checkIntl6251 = (ini : Initl[F]) => { implicit equ: Equality[F] =>
 import ini._
//   assert(   satn  ===            6251 ); assert(             yr  ===           ); assert(   ecco  ===     0.003003500 ); assert(  epoch  === 20630.824120140 ); assert(  inclo  ===     1.013301512 ); 
 //   in/out : 
  assert(     no  ===     0.067918037 ); 
 //   outputs : 
//   assert( method  ===               n ); assert(   ainv  ===     0.941319167 ); assert(     ao  ===     1.062338933 ); assert(  con41  ===    -0.160280193 ); assert(  con42  ===    -0.399533012 ); assert(  cosio  ===     0.529062002 ); 
  assert( cosio2  ===     0.279906602 ); 
//   assert(  einx   ===     0.000009021 ); assert(  eccsq  ===     0.000009021 ); assert(  omeosq ===     0.999990979 ); assert(   posq  ===     1.128543648 ); assert(     rp  ===     1.059148199 ); assert(  rteosq ===     0.999995489 ); 
  assert(  sinio  ===     0.848583171 ); assert(   gsto  ===     3.673754968 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_0 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); 
assert(  bstar  ===     0.000128080 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===     0.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  3988.310226994 ); assert(      y  ===  5498.966572352 ); assert(      z  ===     0.900558787 ); assert(   xdot  ===    -3.290032738 ); assert(   ydot  ===     2.357652820 ); assert(   zdot  ===     6.496623475 ); 
} 
// 6251
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_120 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); 
 assert(  bstar  ===     0.000128080 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===   120.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -3935.698000834 ); assert(      y  ===   409.109808365 ); assert(      z  ===  5471.335773274 ); assert(   xdot  ===    -3.374784183 ); assert(   ydot  ===    -6.635211043 ); assert(   zdot  ===    -1.942056221 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_240 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===   240.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -1675.127669149 ); assert(      y  === -5683.304323518 ); assert(      z  === -3286.215109367 ); assert(   xdot  ===     5.282496925 ); assert(   ydot  ===     1.508674259 ); assert(   zdot  ===    -5.354872978 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_360 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===   360.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  4993.626428356 ); assert(      y  ===  2890.549699000 ); assert(      z  === -3600.401456269 ); assert(   xdot  ===     0.347333429 ); assert(   ydot  ===     5.707031557 ); assert(   zdot  ===     5.070699638 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_480 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===   480.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -1115.079595139 ); assert(      y  ===  4015.116914910 ); assert(      z  ===  5326.997277178 ); assert(   xdot  ===    -5.524279443 ); assert(   ydot  ===    -4.765738774 ); assert(   zdot  ===     2.402255961 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_600 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===   600.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -4329.100081975 ); assert(      y  === -5176.702879352 ); assert(      z  ===   409.653138574 ); assert(   xdot  ===     2.858408303 ); assert(   ydot  ===    -2.933091792 ); assert(   zdot  ===    -6.509690397 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_720 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===   720.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  3692.600300280 ); assert(      y  ===  -976.242652553 ); assert(      z  === -5623.364474929 ); assert(   xdot  ===     3.897257243 ); assert(   ydot  ===     6.415554948 ); assert(   zdot  ===     1.429112190 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_840 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===   840.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  2301.835100373 ); assert(      y  ===  5723.923945532 ); assert(      z  ===  2814.615145803 ); assert(   xdot  ===    -5.110924966 ); assert(   ydot  ===    -0.764510559 ); assert(   zdot  ===     5.662120145 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_960 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===   960.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -4990.916379503 ); assert(      y  === -2303.425478800 ); assert(      z  ===  3920.863355985 ); assert(   xdot  ===    -0.993439372 ); assert(   ydot  ===    -5.967458360 ); assert(   zdot  ===    -4.759110856 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_1080 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  1080.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===   642.277699768 ); assert(      y  === -4332.898219009 ); assert(      z  === -5183.315239096 ); assert(   xdot  ===     5.720542579 ); assert(   ydot  ===     4.216573838 ); assert(   zdot  ===    -2.846576139 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_1200 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  1200.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  4719.783357520 ); assert(      y  ===  4798.069389959 ); assert(      z  ===  -943.588510624 ); assert(   xdot  ===    -2.294860662 ); assert(   ydot  ===     3.492499389 ); assert(   zdot  ===     6.408334723 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_1320 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  1320.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -3299.169936023 ); assert(      y  ===  1576.831683195 ); assert(      z  ===  5678.678406385 ); assert(   xdot  ===    -4.460347074 ); assert(   ydot  ===    -6.202025196 ); assert(   zdot  ===    -0.885874586 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_1440 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  1440.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -2777.146823355 ); assert(      y  === -5663.160317077 ); assert(      z  === -2462.548891232 ); assert(   xdot  ===     4.915493146 ); assert(   ydot  ===     0.123328992 ); assert(   zdot  ===    -5.896495091 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_1560 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  1560.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  4992.315738931 ); assert(      y  ===  1716.623567695 ); assert(      z  === -4287.860655812 ); assert(   xdot  ===     1.640717189 ); assert(   ydot  ===     6.071570434 ); assert(   zdot  ===     4.338797931 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_1680 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  1680.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===    -8.223847547 ); assert(      y  ===  4662.215216680 ); assert(      z  ===  4905.664118573 ); assert(   xdot  ===    -5.891011274 ); assert(   ydot  ===    -3.593173872 ); assert(   zdot  ===     3.365100460 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_1800 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  1800.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -4966.201379626 ); assert(      y  === -4379.591550374 ); assert(      z  ===  1349.333475022 ); assert(   xdot  ===     1.763172581 ); assert(   ydot  ===    -3.981456387 ); assert(   zdot  ===    -6.343279443 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_1920 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  1920.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  2954.493903314 ); assert(      y  === -2080.659846504 ); assert(      z  === -5754.750380565 ); assert(   xdot  ===     4.895893306 ); assert(   ydot  ===     5.858184322 ); assert(   zdot  ===     0.375474825 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_2040 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  2040.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  3363.287943208 ); assert(      y  ===  5559.558411795 ); assert(      z  ===  1956.055422663 ); assert(   xdot  ===    -4.587378863 ); assert(   ydot  ===     0.591943403 ); assert(   zdot  ===     6.107838605 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_2160 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  2160.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -4856.667800700 ); assert(      y  === -1107.034501919 ); assert(      z  ===  4557.212582406 ); assert(   xdot  ===    -2.304158557 ); assert(   ydot  ===    -6.186437070 ); assert(   zdot  ===    -3.956549542 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_2280 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  2280.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  -497.844800712 ); assert(      y  === -4863.460053117 ); assert(      z  === -4700.812112174 ); assert(   xdot  ===     5.960065407 ); assert(   ydot  ===     2.996683369 ); assert(   zdot  ===    -3.767123329 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_2400 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  2400.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  5241.619360959 ); assert(      y  ===  3910.759606834 ); assert(      z  === -1857.934739522 ); assert(   xdot  ===    -1.124834806 ); assert(   ydot  ===     4.406213160 ); assert(   zdot  ===     6.148161299 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_2520 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  2520.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -2451.380459530 ); assert(      y  ===  2610.604632610 ); assert(      z  ===  5729.790220688 ); assert(   xdot  ===    -5.366560525 ); assert(   ydot  ===    -5.500855666 ); assert(   zdot  ===     0.187958716 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_2640 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  2640.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -3791.875206380 ); assert(      y  === -5378.828513819 ); assert(      z  === -1575.827379301 ); assert(   xdot  ===     4.266273592 ); assert(   ydot  ===    -1.199162551 ); assert(   zdot  ===    -6.276154080 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_2760 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  2760.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  4730.539583565 ); assert(      y  ===   524.050064331 ); assert(      z  === -4857.293697253 ); assert(   xdot  ===     2.918056288 ); assert(   ydot  ===     6.135412849 ); assert(   zdot  ===     3.495115636 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat6251_2880 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.000994993 ); assert(  bstar  ===     0.000128080 ); assert(  con41  ===    -0.160280193 ); assert(    cc1  ===     0.000000003 ); 
 assert(    cc4  ===     0.000005200 ); assert(    cc5  ===     0.000650194 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.003003500 ); assert(    eta  ===     0.063675056 ); assert(  argpo  ===     2.428744337 ); assert( argpdot ===     0.000019407 ); assert(  omgcof ===    -0.000000052 ); assert(  sinmao ===    -0.658497710 ); 
 assert(      t  ===  2880.000000000 ); assert(  t2cof  ===     0.000000004 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.720093398 ); 
 assert(  x7thm1 ===     0.959346217 ); assert(  inclo  ===     1.013301512 ); assert(     mo  ===     3.860413487 ); assert(   mdot  ===     0.067910210 ); assert(    xno  ===     0.067918037 ); assert(  nodeo  ===     0.943219561 ); 
 assert( nodedt  ===    -0.000051677 ); assert(  xmcof  ===    -0.000133147 ); assert(  nodecf ===    -0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  1159.278028972 ); assert(      y  ===  5056.601754954 ); assert(      z  ===  4353.494185789 ); assert(   xdot  ===    -5.968060341 ); assert(   ydot  ===    -2.314790406 ); assert(   zdot  ===     4.230722669 ); 
} 
}

// FIXME assert(  xlcof  ===   0.001540606 );  assert(  delmo  ===   1.000092295 );
trait ValladoTLE28057Check[F] { self :  FunSuite => 
  
  //                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_0 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===     0.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -2715.282374856 ); assert(      y  === -6619.264368891 ); assert(      z  ===    -0.013414430 ); assert(   xdot  ===    -1.008587273 ); assert(   ydot  ===     0.422782003 ); assert(   zdot  ===     7.385272942 ); 
} 
// 28057
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_120 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===   120.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -1816.879209419 ); assert(      y  === -1835.787621322 ); assert(      z  ===  6661.079264647 ); assert(   xdot  ===     2.325140071 ); assert(   ydot  ===     6.655669329 ); assert(   zdot  ===     2.463394512 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_240 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===   240.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  1483.173642905 ); assert(      y  ===  5395.212487860 ); assert(      z  ===  4448.659071715 ); assert(   xdot  ===     2.560540387 ); assert(   ydot  ===     4.039025766 ); assert(   zdot  ===    -5.736648561 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_360 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===   360.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  2801.256071573 ); assert(      y  ===  5455.039313331 ); assert(      z  === -3692.128656945 ); assert(   xdot  ===    -0.595095864 ); assert(   ydot  ===    -3.951923117 ); assert(   zdot  ===    -6.298799125 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_480 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===   480.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===   411.093328118 ); assert(      y  === -1728.997691520 ); assert(      z  === -6935.455488101 ); assert(   xdot  ===    -2.935970964 ); assert(   ydot  ===    -6.684085058 ); assert(   zdot  ===     1.492800886 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_600 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===   600.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -2506.525584538 ); assert(      y  === -6628.986550942 ); assert(      z  ===  -988.077844970 ); assert(   xdot  ===    -1.390577189 ); assert(   ydot  ===    -0.556164143 ); assert(   zdot  ===     7.312736468 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_720 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===   720.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -2090.798842662 ); assert(      y  === -2723.228321928 ); assert(      z  ===  6266.133565761 ); assert(   xdot  ===     1.992640665 ); assert(   ydot  ===     6.337529519 ); assert(   zdot  ===     3.411803080 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_840 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===   840.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  1091.805602223 ); assert(      y  ===  4809.882295025 ); assert(      z  ===  5172.428978938 ); assert(   xdot  ===     2.717483546 ); assert(   ydot  ===     4.805518977 ); assert(   zdot  ===    -5.030019896 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_960 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===   960.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  2811.140622996 ); assert(      y  ===  5950.657071710 ); assert(      z  === -2813.237053894 ); assert(   xdot  ===    -0.159662742 ); assert(   ydot  ===    -3.121215491 ); assert(   zdot  ===    -6.775341949 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_1080 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===  1080.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===   805.726983043 ); assert(      y  ===  -812.166279068 ); assert(      z  === -7067.584839683 ); assert(   xdot  ===    -2.798936020 ); assert(   ydot  ===    -6.889265977 ); assert(   zdot  ===     0.472770873 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_1200 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===  1200.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -2249.598375316 ); assert(      y  === -6505.848907139 ); assert(      z  === -1956.723650620 ); assert(   xdot  ===    -1.731234729 ); assert(   ydot  ===    -1.528750230 ); assert(   zdot  ===     7.096660885 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_1320 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===  1320.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -2311.573757974 ); assert(      y  === -3560.991128912 ); assert(      z  ===  5748.167495996 ); assert(   xdot  ===     1.626569751 ); assert(   ydot  ===     5.890482233 ); assert(   zdot  ===     4.293545048 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_1440 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===  1440.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===   688.160565937 ); assert(      y  ===  4124.876189636 ); assert(      z  ===  5794.559944490 ); assert(   xdot  ===     2.810973665 ); assert(   ydot  ===     5.479585563 ); assert(   zdot  ===    -4.224866316 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_1560 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===  1560.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  2759.940882296 ); assert(      y  ===  6329.872717980 ); assert(      z  === -1879.195183309 ); assert(   xdot  ===     0.266930672 ); assert(   ydot  ===    -2.222670878 ); assert(   zdot  ===    -7.119390567 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_1680 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===  1680.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  1171.506771373 ); assert(      y  ===   125.820537476 ); assert(      z  === -7061.966262020 ); assert(   xdot  ===    -2.605687852 ); assert(   ydot  ===    -6.958489749 ); assert(   zdot  ===    -0.556333225 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_1800 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===  1800.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -1951.437084725 ); assert(      y  === -6251.719458202 ); assert(      z  === -2886.954723550 ); assert(   xdot  ===    -2.024131483 ); assert(   ydot  ===    -2.475214272 ); assert(   zdot  ===     6.741537478 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_1920 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===  1920.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -2475.707222879 ); assert(      y  === -4331.905699582 ); assert(      z  ===  5117.312349244 ); assert(   xdot  ===     1.235823539 ); assert(   ydot  ===     5.322743371 ); assert(   zdot  ===     5.091281211 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_2040 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===  2040.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===   281.460978474 ); assert(      y  ===  3353.510571023 ); assert(      z  ===  6302.879006502 ); assert(   xdot  ===     2.840647273 ); assert(   ydot  ===     6.047222485 ); assert(   zdot  ===    -3.337085992 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_2160 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===  2160.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  2650.331188597 ); assert(      y  ===  6584.334348513 ); assert(      z  ===  -908.290271343 ); assert(   xdot  ===     0.675457235 ); assert(   ydot  ===    -1.274044972 ); assert(   zdot  ===    -7.323921567 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_2280 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===  2280.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  ===  1501.172265975 ); assert(      y  ===  1066.311327559 ); assert(      z  === -6918.714729524 ); assert(   xdot  ===    -2.361891904 ); assert(   ydot  ===    -6.889669974 ); assert(   zdot  ===    -1.574718619 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_2400 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===  2400.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -1619.734683344 ); assert(      y  === -5871.140519913 ); assert(      z  === -3760.565870714 ); assert(   xdot  ===    -2.264093975 ); assert(   ydot  ===    -3.376316601 ); assert(   zdot  ===     6.254622256 ); 
} 
//                                     ------------------after sgp4   :---------------
val sgp4CheckSat28057_2520 = (res: Sgp4Result[F]) => { implicit equ: Equality[F] =>
 import res._
 //   inputs : 
 // assert(  isimp  ===               0 ); assert( method  ===             110 ); assert(  aycof  ===     0.001159872 ); assert(  bstar  ===     0.000035940 ); assert(  con41  ===    -0.935550263 ); assert(    cc1  ===     0.000000000 ); 
 assert(    cc4  ===    -0.000000044 ); assert(    cc5  ===     0.000030313 ); assert(     d2  ===     0.000000000 ); assert(     d3  ===     0.000000000 ); assert(     d4  ===     0.000000000 ); 
 assert(   ecco  ===     0.000088400 ); assert(    eta  ===     0.000912426 ); assert(  argpo  ===     1.539317568 ); assert( argpdot ===    -0.000036077 ); assert(  omgcof ===     0.000000000 ); assert(  sinmao ===    -0.999431425 ); 
 assert(      t  ===  2520.000000000 ); assert(  t2cof  ===     0.000000000 ); assert(  t3cof  ===     0.000000000 ); assert(  t4cof  ===     0.000000000 ); assert(  t5cof  ===     0.000000000 ); assert(  x1mth2 ===     0.978516754 ); 
 assert(  x7thm1 ===    -0.849617280 ); assert(  inclo  ===     1.717897912 ); assert(     mo  ===     4.746112232 ); assert(   mdot  ===     0.062634526 ); assert(    xno  ===     0.062672399 ); assert(  nodeo  ===     4.323112489 ); 
 assert( nodedt  ===     0.000011840 ); assert(  xmcof  ===     0.000000000 ); assert(  nodecf ===     0.000000000 ); 
//    outputs : 
// assert(  error  ===               0 ); assert(      x  === -2581.042025049 ); assert(      y  === -5020.055725306 ); assert(      z  ===  4385.923290467 ); assert(   xdot  ===     0.829668458 ); assert(   ydot  ===     4.645048038 ); assert(   zdot  ===     5.789262667 ); 
} 

}



trait  ValladoTLE00005PVCheck[F] { self :  FunSuite => 
   // here, we are comparing km and km/s
  
val pvCheckSgp4_5_0 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  7022.465292664 ); assert(      y  === -1400.082967554 ); assert(      z  ===     0.039951554 ); assert(   xdot  ===     1.893841015 ); assert(   ydot  ===     6.405893759 ); assert(   zdot  ===     4.534807250 ); 
} 
val pvCheckSgp4_5_360 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -7154.031202016 ); assert(      y  === -3783.176825037 ); assert(      z  === -3536.194122942 ); assert(   xdot  ===     4.741887409 ); assert(   ydot  ===    -4.151817765 ); assert(   zdot  ===    -2.093935425 ); 
} 
val pvCheckSgp4_5_720 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -7134.593401193 ); assert(      y  ===  16531.686413336 ); assert(      z  ===  3260.271864826 ); assert(   xdot  ===    -4.113793027 ); assert(   ydot  ===    -2.911922039 ); assert(   zdot  ===    -2.557327851 ); 
} 
val sgp4PVCheckSat5_720 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -7134.593401193 ); assert(      y  ===  6531.686413336 ); assert(      z  ===  3260.271864826 ); assert(   xdot  ===    -4.113793027 ); assert(   ydot  ===    -2.911922039 ); assert(   zdot  ===    -2.557327851 ); 
} 
val sgp4PVCheckSat5_1080 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  5568.539011812 ); assert(      y  ===  4492.069925906 ); assert(      z  ===  3863.876419829 ); assert(   xdot  ===    -4.209106476 ); assert(   ydot  ===     5.159719888 ); assert(   zdot  ===     2.744852980 ); 
} 
val sgp4PVCheckSat5_1440 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  -938.559239429 ); assert(      y  === -6268.187488314 ); assert(      z  === -4294.029247512 ); assert(   xdot  ===     7.536105209 ); assert(   ydot  ===    -0.427127707 ); assert(   zdot  ===     0.989878080 ); 
} 
val sgp4PVCheckSat5_1800 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -9680.561217281 ); assert(      y  ===  2802.477713539 ); assert(      z  ===   124.106880382 ); assert(   xdot  ===    -0.905874102 ); assert(   ydot  ===    -4.659467970 ); assert(   zdot  ===    -3.227347517 ); 
} 
val sgp4PVCheckSat5_2160 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===   190.197969879 ); assert(      y  ===  7746.966536135 ); assert(      z  ===  5110.006754119 ); assert(   xdot  ===    -6.112325142 ); assert(   ydot  ===     1.527008184 ); assert(   zdot  ===    -0.139152358 ); 
} 
val sgp4PVCheckSat5_2520 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  5579.556401157 ); assert(      y  === -3995.613967894 ); assert(      z  === -1518.821089660 ); assert(   xdot  ===     4.767927483 ); assert(   ydot  ===     5.123185301 ); assert(   zdot  ===     4.276837355 ); 
} 
val sgp4PVCheckSat5_2880 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -8650.730822189 ); assert(      y  === -1914.938115252 ); assert(      z  === -3007.036034428 ); assert(   xdot  ===     3.067165127 ); assert(   ydot  ===    -4.828384068 ); assert(   zdot  ===    -2.515322836 ); 
} 
val sgp4PVCheckSat5_3240 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -5429.792041645 ); assert(      y  ===  7574.364937924 ); assert(      z  ===  3747.393052359 ); assert(   xdot  ===    -4.999442110 ); assert(   ydot  ===    -1.800561422 ); assert(   zdot  ===    -2.229392830 ); 
} 
val sgp4PVCheckSat5_3600 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  6759.045837218 ); assert(      y  ===  2001.581982197 ); assert(      z  ===  2783.551925329 ); assert(   xdot  ===    -2.180993947 ); assert(   ydot  ===     6.402085603 ); assert(   zdot  ===     3.644723952 ); 
} 
val sgp4PVCheckSat5_3960 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -3791.445315589 ); assert(      y  === -5712.956178939 ); assert(      z  === -4533.486307144 ); assert(   xdot  ===     6.668817493 ); assert(   ydot  ===    -2.516382327 ); assert(   zdot  ===    -0.082384354 ); 
} 
val sgp4PVCheckSat5_4320 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -9060.473735694 ); assert(      y  ===  4658.709525023 ); assert(      z  ===   813.686731534 ); assert(   xdot  ===    -2.232832783 ); assert(   ydot  ===    -4.110453490 ); assert(   zdot  ===    -3.157345433 ); 
} 
}

trait  ValladoTLE06251PVCheck[F] { self :  FunSuite => 
   // here, we are comparing km
  // implicit val pv06251DoubleEquality = TolerantNumerics.tolerantDoubleEquality(0.08)
  
val sgp4PVCheckSat6251_0 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  3988.310226994 ); assert(      y  ===  5498.966572352 ); assert(      z  ===     0.900558787 ); assert(   xdot  ===    -3.290032738 ); assert(   ydot  ===     2.357652820 ); assert(   zdot  ===     6.496623475 ); 
} 
val sgp4PVCheckSat6251_120 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -3935.698000834 ); assert(      y  ===   409.109808365 ); assert(      z  ===  5471.335773274 ); assert(   xdot  ===    -3.374784183 ); assert(   ydot  ===    -6.635211043 ); assert(   zdot  ===    -1.942056221 ); 
} 
val sgp4PVCheckSat6251_240 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -1675.127669149 ); assert(      y  === -5683.304323518 ); assert(      z  === -3286.215109367 ); assert(   xdot  ===     5.282496925 ); assert(   ydot  ===     1.508674259 ); assert(   zdot  ===    -5.354872978 ); 
} 
val sgp4PVCheckSat6251_360 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  4993.626428356 ); assert(      y  ===  2890.549699000 ); assert(      z  === -3600.401456269 ); assert(   xdot  ===     0.347333429 ); assert(   ydot  ===     5.707031557 ); assert(   zdot  ===     5.070699638 ); 
} 
val sgp4PVCheckSat6251_480 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -1115.079595139 ); assert(      y  ===  4015.116914910 ); assert(      z  ===  5326.997277178 ); assert(   xdot  ===    -5.524279443 ); assert(   ydot  ===    -4.765738774 ); assert(   zdot  ===     2.402255961 ); 
} 
val sgp4PVCheckSat6251_600 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -4329.100081975 ); assert(      y  === -5176.702879352 ); assert(      z  ===   409.653138574 ); assert(   xdot  ===     2.858408303 ); assert(   ydot  ===    -2.933091792 ); assert(   zdot  ===    -6.509690397 ); 
} 
val sgp4PVCheckSat6251_720 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  3692.600300280 ); assert(      y  ===  -976.242652553 ); assert(      z  === -5623.364474929 ); assert(   xdot  ===     3.897257243 ); assert(   ydot  ===     6.415554948 ); assert(   zdot  ===     1.429112190 ); 
} 
val sgp4PVCheckSat6251_840 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  2301.835100373 ); assert(      y  ===  5723.923945532 ); assert(      z  ===  2814.615145803 ); assert(   xdot  ===    -5.110924966 ); assert(   ydot  ===    -0.764510559 ); assert(   zdot  ===     5.662120145 ); 
} 
val sgp4PVCheckSat6251_960 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -4990.916379503 ); assert(      y  === -2303.425478800 ); assert(      z  ===  3920.863355985 ); assert(   xdot  ===    -0.993439372 ); assert(   ydot  ===    -5.967458360 ); assert(   zdot  ===    -4.759110856 ); 
} 
val sgp4PVCheckSat6251_1080 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===   642.277699768 ); assert(      y  === -4332.898219009 ); assert(      z  === -5183.315239096 ); assert(   xdot  ===     5.720542579 ); assert(   ydot  ===     4.216573838 ); assert(   zdot  ===    -2.846576139 ); 
} 
val sgp4PVCheckSat6251_1200 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  4719.783357520 ); assert(      y  ===  4798.069389959 ); assert(      z  ===  -943.588510624 ); assert(   xdot  ===    -2.294860662 ); assert(   ydot  ===     3.492499389 ); assert(   zdot  ===     6.408334723 ); 
} 
val sgp4PVCheckSat6251_1320 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -3299.169936023 ); assert(      y  ===  1576.831683195 ); assert(      z  ===  5678.678406385 ); assert(   xdot  ===    -4.460347074 ); assert(   ydot  ===    -6.202025196 ); assert(   zdot  ===    -0.885874586 ); 
} 
val sgp4PVCheckSat6251_1440 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -2777.146823355 ); assert(      y  === -5663.160317077 ); assert(      z  === -2462.548891232 ); assert(   xdot  ===     4.915493146 ); assert(   ydot  ===     0.123328992 ); assert(   zdot  ===    -5.896495091 ); 
} 
val sgp4PVCheckSat6251_1560 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  4992.315738931 ); assert(      y  ===  1716.623567695 ); assert(      z  === -4287.860655812 ); assert(   xdot  ===     1.640717189 ); assert(   ydot  ===     6.071570434 ); assert(   zdot  ===     4.338797931 ); 
} 
val sgp4PVCheckSat6251_1680 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===    -8.223847547 ); assert(      y  ===  4662.215216680 ); assert(      z  ===  4905.664118573 ); assert(   xdot  ===    -5.891011274 ); assert(   ydot  ===    -3.593173872 ); assert(   zdot  ===     3.365100460 ); 
} 
val sgp4PVCheckSat6251_1800 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -4966.201379626 ); assert(      y  === -4379.591550374 ); assert(      z  ===  1349.333475022 ); assert(   xdot  ===     1.763172581 ); assert(   ydot  ===    -3.981456387 ); assert(   zdot  ===    -6.343279443 ); 
} 
val sgp4PVCheckSat6251_1920 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  2954.493903314 ); assert(      y  === -2080.659846504 ); assert(      z  === -5754.750380565 ); assert(   xdot  ===     4.895893306 ); assert(   ydot  ===     5.858184322 ); assert(   zdot  ===     0.375474825 ); 
} 
val sgp4PVCheckSat6251_2040 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  3363.287943208 ); assert(      y  ===  5559.558411795 ); assert(      z  ===  1956.055422663 ); assert(   xdot  ===    -4.587378863 ); assert(   ydot  ===     0.591943403 ); assert(   zdot  ===     6.107838605 ); 
} 
val sgp4PVCheckSat6251_2160 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -4856.667800700 ); assert(      y  === -1107.034501919 ); assert(      z  ===  4557.212582406 ); assert(   xdot  ===    -2.304158557 ); assert(   ydot  ===    -6.186437070 ); assert(   zdot  ===    -3.956549542 ); 
} 
val sgp4PVCheckSat6251_2280 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  -497.844800712 ); assert(      y  === -4863.460053117 ); assert(      z  === -4700.812112174 ); assert(   xdot  ===     5.960065407 ); assert(   ydot  ===     2.996683369 ); assert(   zdot  ===    -3.767123329 ); 
} 
val sgp4PVCheckSat6251_2400 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  5241.619360959 ); assert(      y  ===  3910.759606834 ); assert(      z  === -1857.934739522 ); assert(   xdot  ===    -1.124834806 ); assert(   ydot  ===     4.406213160 ); assert(   zdot  ===     6.148161299 ); 
} 
val sgp4PVCheckSat6251_2520 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -2451.380459530 ); assert(      y  ===  2610.604632610 ); assert(      z  ===  5729.790220688 ); assert(   xdot  ===    -5.366560525 ); assert(   ydot  ===    -5.500855666 ); assert(   zdot  ===     0.187958716 ); 
} 
val sgp4PVCheckSat6251_2640 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -3791.875206380 ); assert(      y  === -5378.828513819 ); assert(      z  === -1575.827379301 ); assert(   xdot  ===     4.266273592 ); assert(   ydot  ===    -1.199162551 ); assert(   zdot  ===    -6.276154080 ); 
} 
val sgp4PVCheckSat6251_2760 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  4730.539583565 ); assert(      y  ===   524.050064331 ); assert(      z  === -4857.293697253 ); assert(   xdot  ===     2.918056288 ); assert(   ydot  ===     6.135412849 ); assert(   zdot  ===     3.495115636 ); 
} 
val sgp4PVCheckSat6251_2880 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  1159.278028972 ); assert(      y  ===  5056.601754954 ); assert(      z  ===  4353.494185789 ); assert(   xdot  ===    -5.968060341 ); assert(   ydot  ===    -2.314790406 ); assert(   zdot  ===     4.230722669 ); 
} 
}

trait  ValladoTLE28057PVCheck[F] { self :  FunSuite => 
   // here, we are comparing km
  //implicit val pv28057DoubleEquality[F] = TolerantNumerics.tolerantDoubleEquality(0.08)
val sgp4PVCheckSat28057_0 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -2715.282374856 ); assert(      y  === -6619.264368891 ); assert(      z  ===    -0.013414430 ); assert(   xdot  ===    -1.008587273 ); assert(   ydot  ===     0.422782003 ); assert(   zdot  ===     7.385272942 ); 
} 
val sgp4PVCheckSat28057_120 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -1816.879209419 ); assert(      y  === -1835.787621322 ); assert(      z  ===  6661.079264647 ); assert(   xdot  ===     2.325140071 ); assert(   ydot  ===     6.655669329 ); assert(   zdot  ===     2.463394512 ); 
} 
val sgp4PVCheckSat28057_240 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  1483.173642905 ); assert(      y  ===  5395.212487860 ); assert(      z  ===  4448.659071715 ); assert(   xdot  ===     2.560540387 ); assert(   ydot  ===     4.039025766 ); assert(   zdot  ===    -5.736648561 ); 
} 
val sgp4PVCheckSat28057_360 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  2801.256071573 ); assert(      y  ===  5455.039313331 ); assert(      z  === -3692.128656945 ); assert(   xdot  ===    -0.595095864 ); assert(   ydot  ===    -3.951923117 ); assert(   zdot  ===    -6.298799125 ); 
} 
val sgp4PVCheckSat28057_480 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===   411.093328118 ); assert(      y  === -1728.997691520 ); assert(      z  === -6935.455488101 ); assert(   xdot  ===    -2.935970964 ); assert(   ydot  ===    -6.684085058 ); assert(   zdot  ===     1.492800886 ); 
} 
val sgp4PVCheckSat28057_600 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -2506.525584538 ); assert(      y  === -6628.986550942 ); assert(      z  ===  -988.077844970 ); assert(   xdot  ===    -1.390577189 ); assert(   ydot  ===    -0.556164143 ); assert(   zdot  ===     7.312736468 ); 
} 
val sgp4PVCheckSat28057_720 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -2090.798842662 ); assert(      y  === -2723.228321928 ); assert(      z  ===  6266.133565761 ); assert(   xdot  ===     1.992640665 ); assert(   ydot  ===     6.337529519 ); assert(   zdot  ===     3.411803080 ); 
} 
val sgp4PVCheckSat28057_840 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  1091.805602223 ); assert(      y  ===  4809.882295025 ); assert(      z  ===  5172.428978938 ); assert(   xdot  ===     2.717483546 ); assert(   ydot  ===     4.805518977 ); assert(   zdot  ===    -5.030019896 ); 
} 
val sgp4PVCheckSat28057_960 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  2811.140622996 ); assert(      y  ===  5950.657071710 ); assert(      z  === -2813.237053894 ); assert(   xdot  ===    -0.159662742 ); assert(   ydot  ===    -3.121215491 ); assert(   zdot  ===    -6.775341949 ); 
} 
val sgp4PVCheckSat28057_1080 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===   805.726983043 ); assert(      y  ===  -812.166279068 ); assert(      z  === -7067.584839683 ); assert(   xdot  ===    -2.798936020 ); assert(   ydot  ===    -6.889265977 ); assert(   zdot  ===     0.472770873 ); 
} 
val sgp4PVCheckSat28057_1200 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -2249.598375316 ); assert(      y  === -6505.848907139 ); assert(      z  === -1956.723650620 ); assert(   xdot  ===    -1.731234729 ); assert(   ydot  ===    -1.528750230 ); assert(   zdot  ===     7.096660885 ); 
} 
val sgp4PVCheckSat28057_1320 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -2311.573757974 ); assert(      y  === -3560.991128912 ); assert(      z  ===  5748.167495996 ); assert(   xdot  ===     1.626569751 ); assert(   ydot  ===     5.890482233 ); assert(   zdot  ===     4.293545048 ); 
} 
val sgp4PVCheckSat28057_1440 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===   688.160565937 ); assert(      y  ===  4124.876189636 ); assert(      z  ===  5794.559944490 ); assert(   xdot  ===     2.810973665 ); assert(   ydot  ===     5.479585563 ); assert(   zdot  ===    -4.224866316 ); 
} 
val sgp4PVCheckSat28057_1560 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  2759.940882296 ); assert(      y  ===  6329.872717980 ); assert(      z  === -1879.195183309 ); assert(   xdot  ===     0.266930672 ); assert(   ydot  ===    -2.222670878 ); assert(   zdot  ===    -7.119390567 ); 
} 
val sgp4PVCheckSat28057_1680 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  1171.506771373 ); assert(      y  ===   125.820537476 ); assert(      z  === -7061.966262020 ); assert(   xdot  ===    -2.605687852 ); assert(   ydot  ===    -6.958489749 ); assert(   zdot  ===    -0.556333225 ); 
} 
val sgp4PVCheckSat28057_1800 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -1951.437084725 ); assert(      y  === -6251.719458202 ); assert(      z  === -2886.954723550 ); assert(   xdot  ===    -2.024131483 ); assert(   ydot  ===    -2.475214272 ); assert(   zdot  ===     6.741537478 ); 
} 
val sgp4PVCheckSat28057_1920 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -2475.707222879 ); assert(      y  === -4331.905699582 ); assert(      z  ===  5117.312349244 ); assert(   xdot  ===     1.235823539 ); assert(   ydot  ===     5.322743371 ); assert(   zdot  ===     5.091281211 ); 
} 
val sgp4PVCheckSat28057_2040 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===   281.460978474 ); assert(      y  ===  3353.510571023 ); assert(      z  ===  6302.879006502 ); assert(   xdot  ===     2.840647273 ); assert(   ydot  ===     6.047222485 ); assert(   zdot  ===    -3.337085992 ); 
} 
val sgp4PVCheckSat28057_2160 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  2650.331188597 ); assert(      y  ===  6584.334348513 ); assert(      z  ===  -908.290271343 ); assert(   xdot  ===     0.675457235 ); assert(   ydot  ===    -1.274044972 ); assert(   zdot  ===    -7.323921567 ); 
} 
val sgp4PVCheckSat28057_2280 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  ===  1501.172265975 ); assert(      y  ===  1066.311327559 ); assert(      z  === -6918.714729524 ); assert(   xdot  ===    -2.361891904 ); assert(   ydot  ===    -6.889669974 ); assert(   zdot  ===    -1.574718619 ); 
} 
val sgp4PVCheckSat28057_2400 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -1619.734683344 ); assert(      y  === -5871.140519913 ); assert(      z  === -3760.565870714 ); assert(   xdot  ===    -2.264093975 ); assert(   ydot  ===    -3.376316601 ); assert(   zdot  ===     6.254622256 ); 
} 
val sgp4PVCheckSat28057_2520 = (res: PosVel[F]) => { implicit equ: Equality[F] =>
 import res._
assert(  error  ===               0 ); assert(      x  === -2581.042025049 ); assert(      y  === -5020.055725306 ); assert(      z  ===  4385.923290467 ); assert(   xdot  ===     0.829668458 ); assert(   ydot  ===     4.645048038 ); assert(   zdot  ===     5.789262667 ); 
} 
}
