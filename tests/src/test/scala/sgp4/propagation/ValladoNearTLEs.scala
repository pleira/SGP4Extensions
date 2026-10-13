package sgp4.propagation

import org.scalatest.funsuite.AnyFunSuite
import org.scalactic.Equality

/** Reference values from Vallado's cpp implementation of SGP4 (SpaceTrackReport #3, AIAA 2006-6753) for the near Earth
  * satellites 00005, 06251 and 28057.
  *
  * The values are data-driven: the inputs of the sgp4 propagation that are identical at every propagation time are
  * checked once per satellite (plus t per time), and the expected final position/velocity in TEME (km, km/s) is one
  * table row per propagation time. The intermediate x/y/z outputs of the original hand-written per-time checks were
  * commented out in the source and are covered by the position/velocity tables.
  */

trait ValladoNearTLEsCheck[F]
    extends ValladoTLE00005Check[F]
    with ValladoTLE06251Check[F]
    with ValladoTLE28057Check[F] { self: AnyFunSuite =>

  def tlesTimes = List(times00005, times06251, times28057)
}

trait ValladoNearTLEsPVCheck[F]
    extends ValladoTLE00005PVCheck[F]
    with ValladoTLE06251PVCheck[F]
    with ValladoTLE28057PVCheck[F] { self: AnyFunSuite => }

trait ValladoTLE00005Check[F] { self: AnyFunSuite =>

  // ------------------after initl  :---------------
  val checkIntl5 = (ini: Initl[F]) => { implicit equ: Equality[F] =>
    import ini._
    assert(satn === 5); assert(ecco === 0.185966700); assert(epoch === 18441.784950620); assert(inclo === 0.598092919);
    //   in/out :
    assert(no === 0.047206302);
    //   outputs :
    assert(ainv === 0.738607085); assert(ao === 1.353899821); assert(cosio === 0.826410932);
    assert(cosio2 === 0.682955029);
    /*  assert(  einx   ===     0.034583614 ); */
    assert(eccsq === 0.034583614); assert(omeosq === 0.965416386); assert(rp === 1.102119539);
    assert(rteosq === 0.982556048);
    assert(sinio === 0.563067465); assert(gsto === 3.469172342);
  }
  // ------------------after sgp4init :-------------
  val checkSgp4Init5 = (ini: Sgp4Init[F]) => { implicit equ: Equality[F] =>
    import ini._
    assert(satn === 5); assert(yr === 0); assert(bstar === 0.000028098); assert(ecco === 0.185966700);
    assert(epoch === 18441.784950620); assert(argpo === 5.790416027);
    assert(inclo === 0.598092919); assert(mo === 0.337309313);
// in and out variables
    assert(no === 0.047206302);
//    outputs  :
    assert(isimp === 0);
    assert(con41 === 1.048865088); assert(cc1 === 0.000000000); assert(cc4 === 0.000000526);
    assert(cc5 === 0.000016465); assert(d2 === 0.000000000); assert(d3 === 0.000000000); assert(d4 === 0.000000000);
    assert(eta === 0.736909543);
    assert(argpdot === 0.000054293); assert(omgcof === 0.000000000);
    assert(sinmao === 0.330949230); assert(t2cof === 0.000000000); assert(t3cof === 0.000000000);
    assert(t4cof === 0.000000000); assert(t5cof === 0.000000000); assert(gsto === 3.469172342);
    assert(x1mth2 === 0.317044971); assert(x7thm1 === 3.780685205);
    assert(xmcof === -0.000000000); assert(mdot === 0.047229443); assert(nodecf === -0.000000000);
    assert(nodedt === -0.000037171);
    assert(t === 0.000000000); assert(nodeo === 6.086385471);
  }

  /** Inputs of the sgp4 propagation of TLE 5 from Vallado's cpp output. They are identical at every propagation time;
    * only t changes.
    */
  def checkSgp4Inputs5(res: Sgp4Result[F])(using equ: Equality[F]): Unit =
    import res._
    assert(isimp === 0); assert(method === 110); assert(bstar === 0.000028098); assert(cc1 === 0.000000000);
    assert(cc4 === 0.000000526); assert(cc5 === 0.000016465); assert(d2 === 0.000000000); assert(d3 === 0.000000000);
    assert(d4 === 0.000000000); assert(ecco === 0.185966700); assert(eta === 0.736909543);
    assert(argpo === 5.790416027);
    assert(argpdot === 0.000054293); assert(omgcof === 0.000000000); assert(sinmao === 0.330949230);
    assert(t2cof === 0.000000000);
    assert(t3cof === 0.000000000); assert(t4cof === 0.000000000); assert(t5cof === 0.000000000);
    assert(x1mth2 === 0.317044971);
    assert(x7thm1 === 3.780685205); assert(inclo === 0.598092919); assert(mo === 0.337309313);
    assert(mdot === 0.047229443);
    assert(xno === 0.047206302); assert(nodeo === 6.086385471); assert(nodedt === -0.000037171);
    assert(xmcof === -0.000000000);
    assert(nodecf === -0.000000000);

  // propagation times in minutes
  val times00005: List[Double] =
    List(0.0, 360.0, 720.0, 1080.0, 1440.0, 1800.0, 2160.0, 2520.0, 2880.0, 3240.0, 3600.0, 3960.0, 4320.0)

  val check00005: List[Sgp4Result[F] => Equality[F] => Unit] =
    times00005 map { t => res =>
      { implicit equ: Equality[F] =>
        checkSgp4Inputs5(res)(using equ)
        assert(res.t === t)
      }
    }
}

trait ValladoTLE06251Check[F] { self: AnyFunSuite =>

  // ------------------after initl  :---------------
  val checkIntl6251 = (ini: Initl[F]) => { implicit equ: Equality[F] =>
    import ini._
//   assert(   satn  ===            6251 ); assert(             yr  ===           ); assert(   ecco  ===     0.003003500 ); assert(  epoch  === 20630.824120140 ); assert(  inclo  ===     1.013301512 );
    //   in/out :
    assert(no === 0.067918037);
    //   outputs :
//   assert( method  ===               n ); assert(   ainv  ===     0.941319167 ); assert(     ao  ===     1.062338933 ); assert(  con41  ===    -0.160280193 ); assert(  con42  ===    -0.399533012 ); assert(  cosio  ===     0.529062002 );
    assert(cosio2 === 0.279906602);
//   assert(  einx   ===     0.000009021 ); assert(  eccsq  ===     0.000009021 ); assert(  omeosq ===     0.999990979 ); assert(   posq  ===     1.128543648 ); assert(     rp  ===     1.059148199 ); assert(  rteosq ===     0.999995489 );
    assert(sinio === 0.848583171); assert(gsto === 3.673754968);
  }
  // ------------------after sgp4init :-------------
  val checkSgp4Init6251 = (ini: Sgp4Init[F]) => { implicit equ: Equality[F] =>
    import ini._
    assert(satn === 6251); assert(yr === 6); assert(bstar === 0.000128080); assert(ecco === 0.003003500);
    assert(epoch === 20630.824120140); assert(argpo === 2.428744337);
    assert(inclo === 1.013301512); assert(mo === 3.860413487);
// in and out variables
    assert(no === 0.067918037);
//    outputs  :
    assert(isimp === 0); assert(aycof === 0.000994993);
    assert(con41 === -0.160280193); assert(cc1 === 0.000000003); assert(cc4 === 0.000005200);
    assert(cc5 === 0.000650194); assert(d2 === 0.000000000); assert(d3 === 0.000000000); assert(d4 === 0.000000000);
    assert(eta === 0.063675056);
    assert(argpdot === 0.000019407); assert(omgcof === -0.000000052);
    assert(sinmao === -0.658497710); assert(t2cof === 0.000000004); assert(t3cof === 0.000000000);
    assert(t4cof === 0.000000000); assert(t5cof === 0.000000000); assert(gsto === 3.673754968);
    assert(x1mth2 === 0.720093398); assert(x7thm1 === 0.959346217);
    assert(xmcof === -0.000133147); assert(mdot === 0.067910210); assert(nodecf === -0.000000000);
    assert(nodedt === -0.000051677);
//   in and outputs from deep space satellites :
    assert(t === 0.000000000); assert(nodeo === 0.943219561);
  }

  /** Inputs of the sgp4 propagation of TLE 6251 from Vallado's cpp output. They are identical at every propagation
    * time; only t changes.
    */
  def checkSgp4Inputs6251(res: Sgp4Result[F])(using equ: Equality[F]): Unit =
    import res._
    assert(bstar === 0.000128080); assert(cc1 === 0.000000003); assert(cc4 === 0.000005200);
    assert(cc5 === 0.000650194);
    assert(d2 === 0.000000000); assert(d3 === 0.000000000); assert(d4 === 0.000000000); assert(ecco === 0.003003500);
    assert(eta === 0.063675056); assert(argpo === 2.428744337); assert(argpdot === 0.000019407);
    assert(omgcof === -0.000000052);
    assert(sinmao === -0.658497710); assert(t2cof === 0.000000004); assert(t3cof === 0.000000000);
    assert(t4cof === 0.000000000);
    assert(t5cof === 0.000000000); assert(x1mth2 === 0.720093398); assert(x7thm1 === 0.959346217);
    assert(inclo === 1.013301512);
    assert(mo === 3.860413487); assert(mdot === 0.067910210); assert(xno === 0.067918037);
    assert(nodeo === 0.943219561);
    assert(nodedt === -0.000051677); assert(xmcof === -0.000133147); assert(nodecf === -0.000000000);

  // propagation times in minutes
  val times06251: List[Double] = List(0.0, 120.0, 240.0, 360.0, 480.0, 600.0, 720.0, 840.0, 960.0, 1080.0, 1200.0,
    1320.0, 1440.0, 1560.0, 1680.0, 1800.0, 1920.0, 2040.0, 2160.0, 2280.0, 2400.0, 2520.0, 2640.0, 2760.0, 2880.0)

  val check06251: List[Sgp4Result[F] => Equality[F] => Unit] =
    times06251 map { t => res =>
      { implicit equ: Equality[F] =>
        checkSgp4Inputs6251(res)(using equ)
        assert(res.t === t)
      }
    }
}

trait ValladoTLE28057Check[F] { self: AnyFunSuite =>

  // ------------------after initl  :---------------
  val checkIntl28057 = (ini: Initl[F]) => { implicit equ: Equality[F] =>
    import ini._
    assert(satn === 28057); assert(ecco === 0.000088400); assert(epoch === 20631.786158330);
    assert(inclo === 1.717897912);
    //   in/out :
    assert(no === 0.062672399);
    //   outputs :
    assert(ainv === 0.892204404); assert(ao === 1.120819394); assert(cosio === -0.146571640);
    assert(cosio2 === 0.021483246);
    /*  assert(  einx   ===     0.000000008 ); */
    assert(eccsq === 0.000000008); assert(omeosq === 0.999999992); assert(rp === 1.120720314);
    assert(rteosq === 0.999999996);
    assert(sinio === 0.989200058); assert(gsto === 3.451783622);
  }
  // ------------------after sgp4init :-------------
  val checkSgp4Init28057 = (ini: Sgp4Init[F]) => { implicit equ: Equality[F] =>
    import ini._
    assert(satn === 28057); assert(yr === 6); assert(bstar === 0.000035940); assert(ecco === 0.000088400);
    assert(epoch === 20631.786158330); assert(argpo === 1.539317568);
    assert(inclo === 1.717897912); assert(mo === 4.746112232);
// in and out variables
    assert(no === 0.062672399);
//    outputs  :
    assert(isimp === 0); assert(aycof === 0.001159872);
    assert(con41 === -0.935550263); assert(cc1 === 0.000000000); assert(cc4 === -0.000000044);
    assert(cc5 === 0.000030313); assert(d2 === 0.000000000); assert(d3 === 0.000000000); assert(d4 === 0.000000000);
    assert(eta === 0.000912426); assert(argpdot === -0.000036077); assert(omgcof === 0.000000000);
    assert(sinmao === -0.999431425); assert(t2cof === 0.000000000); assert(t3cof === 0.000000000);
    assert(t4cof === 0.000000000); assert(t5cof === 0.000000000); assert(gsto === 3.451783622);
    assert(x1mth2 === 0.978516754); assert(x7thm1 === -0.849617280);
    assert(xmcof === 0.000000000); assert(mdot === 0.062634526); assert(nodecf === 0.000000000);
    assert(nodedt === 0.000011840);
//   in and outputs from deep space satellites :
    assert(t === 0.000000000); assert(nodeo === 4.323112489);
  }

  /** Inputs of the sgp4 propagation of TLE 28057 from Vallado's cpp output. They are identical at every propagation
    * time; only t changes.
    */
  def checkSgp4Inputs28057(res: Sgp4Result[F])(using equ: Equality[F]): Unit =
    import res._
    assert(cc4 === -0.000000044); assert(cc5 === 0.000030313); assert(d2 === 0.000000000); assert(d3 === 0.000000000);
    assert(d4 === 0.000000000); assert(ecco === 0.000088400); assert(eta === 0.000912426);
    assert(argpo === 1.539317568);
    assert(argpdot === -0.000036077); assert(omgcof === 0.000000000); assert(sinmao === -0.999431425);
    assert(t2cof === 0.000000000);
    assert(t3cof === 0.000000000); assert(t4cof === 0.000000000); assert(t5cof === 0.000000000);
    assert(x1mth2 === 0.978516754);
    assert(x7thm1 === -0.849617280); assert(inclo === 1.717897912); assert(mo === 4.746112232);
    assert(mdot === 0.062634526);
    assert(xno === 0.062672399); assert(nodeo === 4.323112489); assert(nodedt === 0.000011840);
    assert(xmcof === 0.000000000);
    assert(nodecf === 0.000000000);

  // propagation times in minutes
  val times28057: List[Double] = List(0.0, 120.0, 240.0, 360.0, 480.0, 600.0, 720.0, 840.0, 960.0, 1080.0, 1200.0,
    1320.0, 1440.0, 1560.0, 1680.0, 1800.0, 1920.0, 2040.0, 2160.0, 2280.0, 2400.0, 2520.0)

  val check28057: List[Sgp4Result[F] => Equality[F] => Unit] =
    times28057 map { t => res =>
      { implicit equ: Equality[F] =>
        checkSgp4Inputs28057(res)(using equ)
        assert(res.t === t)
      }
    }
}

trait ValladoTLE00005PVCheck[F] { self: AnyFunSuite =>
  // here, we are comparing km and km/s

  /** Expected final position (km) and velocity (km/s) in TEME from Vallado's cpp output, one row per propagation time.
    */
  val pvExpected00005: List[(Double, Double, Double, Double, Double, Double)] = List(
    (7022.465292664, -1400.082967554, 0.039951554, 1.893841015, 6.405893759, 4.534807250),
    (-7154.031202016, -3783.176825037, -3536.194122942, 4.741887409, -4.151817765, -2.093935425),
    (-7134.593401193, 6531.686413336, 3260.271864826, -4.113793027, -2.911922039, -2.557327851),
    (5568.539011812, 4492.069925906, 3863.876419829, -4.209106476, 5.159719888, 2.744852980),
    (-938.559239429, -6268.187488314, -4294.029247512, 7.536105209, -0.427127707, 0.989878080),
    (-9680.561217281, 2802.477713539, 124.106880382, -0.905874102, -4.659467970, -3.227347517),
    (190.197969879, 7746.966536135, 5110.006754119, -6.112325142, 1.527008184, -0.139152358),
    (5579.556401157, -3995.613967894, -1518.821089660, 4.767927483, 5.123185301, 4.276837355),
    (-8650.730822189, -1914.938115252, -3007.036034428, 3.067165127, -4.828384068, -2.515322836),
    (-5429.792041645, 7574.364937924, 3747.393052359, -4.999442110, -1.800561422, -2.229392830),
    (6759.045837218, 2001.581982197, 2783.551925329, -2.180993947, 6.402085603, 3.644723952),
    (-3791.445315589, -5712.956178939, -4533.486307144, 6.668817493, -2.516382327, -0.082384354),
    (-9060.473735694, 4658.709525023, 813.686731534, -2.232832783, -4.110453490, -3.157345433)
  )

  val pvCheck00005: List[PosVel[F] => Equality[F] => Unit] =
    pvExpected00005 map { case (ex, ey, ez, evx, evy, evz) =>
      res => { implicit equ: Equality[F] =>
        import res._
        assert(error === 0)
        assert(x === ex)
        assert(y === ey)
        assert(z === ez)
        assert(xdot === evx)
        assert(ydot === evy)
        assert(zdot === evz)
      }
    }
}

trait ValladoTLE06251PVCheck[F] { self: AnyFunSuite =>
  // here, we are comparing km and km/s

  /** Expected final position (km) and velocity (km/s) in TEME from Vallado's cpp output, one row per propagation time.
    */
  val pvExpected06251: List[(Double, Double, Double, Double, Double, Double)] = List(
    (3988.310226994, 5498.966572352, 0.900558787, -3.290032738, 2.357652820, 6.496623475),
    (-3935.698000834, 409.109808365, 5471.335773274, -3.374784183, -6.635211043, -1.942056221),
    (-1675.127669149, -5683.304323518, -3286.215109367, 5.282496925, 1.508674259, -5.354872978),
    (4993.626428356, 2890.549699000, -3600.401456269, 0.347333429, 5.707031557, 5.070699638),
    (-1115.079595139, 4015.116914910, 5326.997277178, -5.524279443, -4.765738774, 2.402255961),
    (-4329.100081975, -5176.702879352, 409.653138574, 2.858408303, -2.933091792, -6.509690397),
    (3692.600300280, -976.242652553, -5623.364474929, 3.897257243, 6.415554948, 1.429112190),
    (2301.835100373, 5723.923945532, 2814.615145803, -5.110924966, -0.764510559, 5.662120145),
    (-4990.916379503, -2303.425478800, 3920.863355985, -0.993439372, -5.967458360, -4.759110856),
    (642.277699768, -4332.898219009, -5183.315239096, 5.720542579, 4.216573838, -2.846576139),
    (4719.783357520, 4798.069389959, -943.588510624, -2.294860662, 3.492499389, 6.408334723),
    (-3299.169936023, 1576.831683195, 5678.678406385, -4.460347074, -6.202025196, -0.885874586),
    (-2777.146823355, -5663.160317077, -2462.548891232, 4.915493146, 0.123328992, -5.896495091),
    (4992.315738931, 1716.623567695, -4287.860655812, 1.640717189, 6.071570434, 4.338797931),
    (-8.223847547, 4662.215216680, 4905.664118573, -5.891011274, -3.593173872, 3.365100460),
    (-4966.201379626, -4379.591550374, 1349.333475022, 1.763172581, -3.981456387, -6.343279443),
    (2954.493903314, -2080.659846504, -5754.750380565, 4.895893306, 5.858184322, 0.375474825),
    (3363.287943208, 5559.558411795, 1956.055422663, -4.587378863, 0.591943403, 6.107838605),
    (-4856.667800700, -1107.034501919, 4557.212582406, -2.304158557, -6.186437070, -3.956549542),
    (-497.844800712, -4863.460053117, -4700.812112174, 5.960065407, 2.996683369, -3.767123329),
    (5241.619360959, 3910.759606834, -1857.934739522, -1.124834806, 4.406213160, 6.148161299),
    (-2451.380459530, 2610.604632610, 5729.790220688, -5.366560525, -5.500855666, 0.187958716),
    (-3791.875206380, -5378.828513819, -1575.827379301, 4.266273592, -1.199162551, -6.276154080),
    (4730.539583565, 524.050064331, -4857.293697253, 2.918056288, 6.135412849, 3.495115636),
    (1159.278028972, 5056.601754954, 4353.494185789, -5.968060341, -2.314790406, 4.230722669)
  )

  val pvCheck06251: List[PosVel[F] => Equality[F] => Unit] =
    pvExpected06251 map { case (ex, ey, ez, evx, evy, evz) =>
      res => { implicit equ: Equality[F] =>
        import res._
        assert(error === 0)
        assert(x === ex)
        assert(y === ey)
        assert(z === ez)
        assert(xdot === evx)
        assert(ydot === evy)
        assert(zdot === evz)
      }
    }
}

trait ValladoTLE28057PVCheck[F] { self: AnyFunSuite =>
  // here, we are comparing km and km/s

  /** Expected final position (km) and velocity (km/s) in TEME from Vallado's cpp output, one row per propagation time.
    */
  val pvExpected28057: List[(Double, Double, Double, Double, Double, Double)] = List(
    (-2715.282374856, -6619.264368891, -0.013414430, -1.008587273, 0.422782003, 7.385272942),
    (-1816.879209419, -1835.787621322, 6661.079264647, 2.325140071, 6.655669329, 2.463394512),
    (1483.173642905, 5395.212487860, 4448.659071715, 2.560540387, 4.039025766, -5.736648561),
    (2801.256071573, 5455.039313331, -3692.128656945, -0.595095864, -3.951923117, -6.298799125),
    (411.093328118, -1728.997691520, -6935.455488101, -2.935970964, -6.684085058, 1.492800886),
    (-2506.525584538, -6628.986550942, -988.077844970, -1.390577189, -0.556164143, 7.312736468),
    (-2090.798842662, -2723.228321928, 6266.133565761, 1.992640665, 6.337529519, 3.411803080),
    (1091.805602223, 4809.882295025, 5172.428978938, 2.717483546, 4.805518977, -5.030019896),
    (2811.140622996, 5950.657071710, -2813.237053894, -0.159662742, -3.121215491, -6.775341949),
    (805.726983043, -812.166279068, -7067.584839683, -2.798936020, -6.889265977, 0.472770873),
    (-2249.598375316, -6505.848907139, -1956.723650620, -1.731234729, -1.528750230, 7.096660885),
    (-2311.573757974, -3560.991128912, 5748.167495996, 1.626569751, 5.890482233, 4.293545048),
    (688.160565937, 4124.876189636, 5794.559944490, 2.810973665, 5.479585563, -4.224866316),
    (2759.940882296, 6329.872717980, -1879.195183309, 0.266930672, -2.222670878, -7.119390567),
    (1171.506771373, 125.820537476, -7061.966262020, -2.605687852, -6.958489749, -0.556333225),
    (-1951.437084725, -6251.719458202, -2886.954723550, -2.024131483, -2.475214272, 6.741537478),
    (-2475.707222879, -4331.905699582, 5117.312349244, 1.235823539, 5.322743371, 5.091281211),
    (281.460978474, 3353.510571023, 6302.879006502, 2.840647273, 6.047222485, -3.337085992),
    (2650.331188597, 6584.334348513, -908.290271343, 0.675457235, -1.274044972, -7.323921567),
    (1501.172265975, 1066.311327559, -6918.714729524, -2.361891904, -6.889669974, -1.574718619),
    (-1619.734683344, -5871.140519913, -3760.565870714, -2.264093975, -3.376316601, 6.254622256),
    (-2581.042025049, -5020.055725306, 4385.923290467, 0.829668458, 4.645048038, 5.789262667)
  )

  val pvCheck28057: List[PosVel[F] => Equality[F] => Unit] =
    pvExpected28057 map { case (ex, ey, ez, evx, evy, evz) =>
      res => { implicit equ: Equality[F] =>
        import res._
        assert(error === 0)
        assert(x === ex)
        assert(y === ey)
        assert(z === ez)
        assert(xdot === evx)
        assert(ydot === evy)
        assert(zdot === evz)
      }
    }
}
