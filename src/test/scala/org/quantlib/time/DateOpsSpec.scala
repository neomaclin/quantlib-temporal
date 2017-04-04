package org.quantlib.time

import java.time.{Month, Year}

import org.scalatest._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
/**
  * Created by neo on 19/03/2017.
  */
class DateOpsSpec extends FlatSpec with Matchers {

  "date consistency" should "pass all test" in {
    import java.time.Month._
    import org.quantlib.time.implicits.Date._

    val minDate = DateOps.MIN.toNumber
    val maxDate = DateOps.MAX.toNumber

    var dyold = DateOps.fromNumber(minDate-1).doy
    var dold  = DateOps.fromNumber(minDate-1).dom
    var mold  = DateOps.fromNumber(minDate-1).month
    var yold  = DateOps.fromNumber(minDate-1).year
    var wdold = DateOps.fromNumber(minDate-1).dow

    minDate to maxDate foreach { i =>
      val t = DateOps.fromNumber(i)
      var serial = t.toNumber

      // check serial number consistency
      assert(serial == i,
        s"inconsistent serial number:    original: $i   date: $t    serial number: $serial")

      val dy = t.doy
      val d = t.dom
      val m = t.month
      val y = t.year
      val wd = t.dow


      // check if skipping any date
      assert((dy == dyold + 1) ||
        (dy == 1 && dyold == 365 && !yold.isLeap) ||
        (dy == 1 && dyold == 366 && yold.isLeap),
        s"wrong day of year increment: \n date: $t\n    day of year: $dy\n previous:  $dyold")
      dyold = dy


      assert((d == dold+1 && m == mold   && y == yold) ||
        (d == 1      && m == mold + 1  && y == yold) ||
        (d == 1      && m == Month.of(1)      && y == yold + 1),
        s"wrong day,month,year increment:   date: $t day,month,year $d ,$m, $y  previous:    $dold,$mold,$yold")
      dold = d
      mold = m
      yold = y


      val s = DateOps.from(d, m, y)

      assert(s.toNumber == i,
        s"inconsistent serial number: date: $t serial number: $i cloned date: $s serial number: $serial")
    }

  }

  "datetime consistency" should "pass all test" in {
    assert(true)
  }

  "intraday" should "pass all test" in {
    import java.time.Month._
    import org.quantlib.time.implicits.DateTime._

    val d1 = DateOps.from(12, FEBRUARY, Year.of(2015), 10, 45, 13, (234 * 1000 + 76253) * 100)

    assert(d1.year.getValue == 2015, "failed to reproduce year")
    assert(d1.month == FEBRUARY, "failed to reproduce month")
    assert(d1.dom == 12, "failed to reproduce day")
    val (h,m,s,n) = d1.HMSN
    assert(h == 10, "failed to reproduce hour of day")
    assert(m == 45, "failed to reproduce minute of hour")
    assert(s == 13, "failed to reproduce second of minute")


    val d3 = DateOps.from(7, FEBRUARY, Year.of(2015), 1, 4, 2, (3*1000 + 4) * 1000)

    assert(d3.toString == "2015-02-07T01:04:02.003004", "datetime to string failed to reproduce expected result")


  }
}
