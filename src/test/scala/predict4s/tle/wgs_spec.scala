package predict4s.tle

import org.scalatest._


class wgs_spec extends FunSuite {
  
  test("Check WGS values") {
    val wgs = WGS72Constants.tleDoubleConstants
    import wgs._
    assert(aE === 6378.135)
    assert(J2 === 0.001082616)
    assert(K2 === (J2*aE/2))
    assert(K4 === (-3*J4*aE*aE*aE*aE / 8))
  }
  
}