package org.quantlib.time

import java.time.{Month, Year}

import org.quantlib.time.daycounts.ActualActual.Convention._
import org.quantlib.time.daycounts.Thirty360.Convention
import org.quantlib.time.daycounts._
import org.quantlib.time.enums.TimeUnit._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
import org.scalatest._

/**
  * Created by neo on 12/03/2017.
  */
class DayCountBasisSpec extends FlatSpec with Matchers {

  final case class SingleCase[D: DateOps](convention: ActualActual.Convention,
                                          start: D,
                                          end: D,
                                          result: Double,
                                          refStart: Option[D] = None,
                                          refEnd: Option[D] = None)

  "ActualActual" should "pass all testing for default policy." in {
    import java.time.Month._
    import org.quantlib.time.implicits.Date._

    val testCases = List(
      SingleCase(ISDA, DateOps.from(1, NOVEMBER, Year.of(2003)), DateOps.from(1, MAY, Year.of(2004)), 0.497724380567),
      SingleCase(ISMA, DateOps.from(1, NOVEMBER, Year.of(2003)), DateOps.from(1, MAY, Year.of(2004)), 0.500000000000,
        Some(DateOps.from(1, NOVEMBER, Year.of(2003))), Some(DateOps.from(1, MAY, Year.of(2004)))),
      SingleCase(AFB, DateOps.from(1, NOVEMBER, Year.of(2003)), DateOps.from(1, MAY, Year.of(2004)), 0.497267759563),
      // short first calculation period (first period)
      SingleCase(ISDA, DateOps.from(1, FEBRUARY, Year.of(1999)), DateOps.from(1, JULY, Year.of(1999)), 0.410958904110),
      SingleCase(ISMA, DateOps.from(1, FEBRUARY, Year.of(1999)), DateOps.from(1, JULY, Year.of(1999)), 0.410958904110,
        Some(DateOps.from(1, JULY, Year.of(1998))), Some(DateOps.from(1, JULY, Year.of(1999)))),
      SingleCase(AFB, DateOps.from(1, FEBRUARY, Year.of(1999)), DateOps.from(1, JULY, Year.of(1999)), 0.410958904110),
      // short first calculation period (second period)
      SingleCase(ISDA, DateOps.from(1, JULY, Year.of(1999)), DateOps.from(1, JULY, Year.of(2000)), 1.001377348600),
      SingleCase(ISMA, DateOps.from(1, JULY, Year.of(1999)), DateOps.from(1, JULY, Year.of(2000)), 1.000000000000,
        Some(DateOps.from(1, JULY, Year.of(1999))), Some(DateOps.from(1, JULY, Year.of(2000)))),
      SingleCase(AFB, DateOps.from(1, JULY, Year.of(1999)), DateOps.from(1, JULY, Year.of(2000)), 1.000000000000),
      // long first calculation period (first period)
      SingleCase(ISDA, DateOps.from(15, AUGUST, Year.of(2002)), DateOps.from(15, JULY, Year.of(2003)), 0.915068493151),
      SingleCase(ISMA, DateOps.from(15, AUGUST, Year.of(2002)), DateOps.from(15, JULY, Year.of(2003)), 0.915760869565,
        Some(DateOps.from(15, JANUARY, Year.of(2003))), Some(DateOps.from(15, JULY, Year.of(2003)))),
      SingleCase(AFB, DateOps.from(15, AUGUST, Year.of(2002)), DateOps.from(15, JULY, Year.of(2003)), 0.915068493151),
      // long first calculation period (second period)
      /* Warning: the ISDA case is in disagreement with mktc1198.pdf */
      SingleCase(ISDA, DateOps.from(15, JULY, Year.of(2003)), DateOps.from(15, JANUARY, Year.of(2004)), 0.504004790778),
      SingleCase(ISMA, DateOps.from(15, JULY, Year.of(2003)), DateOps.from(15, JANUARY, Year.of(2004)), 0.500000000000,
        Some(DateOps.from(15, JULY, Year.of(2003))), Some(DateOps.from(15, JANUARY, Year.of(2004)))),
      SingleCase(AFB, DateOps.from(15, JULY, Year.of(2003)), DateOps.from(15, JANUARY, Year.of(2004)), 0.504109589041),
      // short final calculation period (penultimate period)
      SingleCase(ISDA, DateOps.from(30, JULY, Year.of(1999)), DateOps.from(30, JANUARY, Year.of(2000)), 0.503892506924),
      SingleCase(ISMA, DateOps.from(30, JULY, Year.of(1999)), DateOps.from(30, JANUARY, Year.of(2000)), 0.500000000000,
        Some(DateOps.from(30, JULY, Year.of(1999))), Some(DateOps.from(30, JANUARY, Year.of(2000)))),
      SingleCase(AFB, DateOps.from(30, JULY, Year.of(1999)), DateOps.from(30, JANUARY, Year.of(2000)), 0.504109589041),
      // short final calculation period (final period)
      SingleCase(ISDA, DateOps.from(30, JANUARY, Year.of(2000)), DateOps.from(30, JUNE, Year.of(2000)), 0.415300546448),
      SingleCase(ISMA, DateOps.from(30, JANUARY, Year.of(2000)), DateOps.from(30, JUNE, Year.of(2000)), 0.417582417582,
        Some(DateOps.from(30, JANUARY, Year.of(2000))), Some(DateOps.from(30, JULY, Year.of(2000)))),
      SingleCase(AFB, DateOps.from(30, JANUARY, Year.of(2000)), DateOps.from(30, JUNE, Year.of(2000)), 0.41530054644)
    )
    testCases foreach { case SingleCase(conv, date1, date2, expected, refStart, refEnd) =>


      val calculated = ActualActual(conv).yearFraction(date1, date2, refStart, refEnd)

      assert(Math.abs(calculated - expected) <= 1.0e-10, s"$calculated $expected ${ActualActual(conv)}")


    }
  }

  "One" should "pass all testing for default policy." in {

    val periods = List[Period](Period(3, Months), Period(6, Months), Period(1, Years))
    val expectedOutComes = List[Double](1.0, 1.0, 1.0)

    // 1 years should be enough
    // import org.quantlib.time.implicits.Date._
    // import org.quantlib.time.implicits.DateTime._
    implicit val implicitValue = org.quantlib.time.implicits.Date.LocalDateOps

    val first = DateOps.from(1, Month.JANUARY, Year.of(2004))
    val last = DateOps.from(31, Month.DECEMBER, Year.of(2004))
    val dayCounter = One()

    //for (Date start = first; start <= last; start++) {
    (0 until first.to(last, Days).toInt) foreach { i =>
      periods zip expectedOutComes foreach { case (p, expected) =>
        val start = first + Period(i, Days)
        val end = start + p
        val calculated = dayCounter.yearFraction(start, end)
        assert(Math.abs(calculated - expected) <= 1.0e-12,
          s"DateOps.from $start to $end  calculated: $calculated expected:  $expected")
      }
    }
  }

  "Thirty360_BondBasis" should "pass all testing for default policy." in {
    import java.time.Month._
    import org.quantlib.time.implicits.Date._

    val startDates = List(
      DateOps.from(20, AUGUST, Year.of(2006)),
      DateOps.from(20, FEBRUARY, Year.of(2007)),
      DateOps.from(20, AUGUST, Year.of(2007)),
      DateOps.from(20, FEBRUARY, Year.of(2008)),
      DateOps.from(20, AUGUST, Year.of(2008)),
      DateOps.from(20, FEBRUARY, Year.of(2009)),

      DateOps.from(31, AUGUST, Year.of(2006)),
      DateOps.from(28, FEBRUARY, Year.of(2007)),
      DateOps.from(31, AUGUST, Year.of(2007)),
      DateOps.from(29, FEBRUARY, Year.of(2008)),
      DateOps.from(31, AUGUST, Year.of(2008)),
      DateOps.from(28, FEBRUARY, Year.of(2009)),

      DateOps.from(31, JANUARY, Year.of(2006)),
      DateOps.from(30, JANUARY, Year.of(2006)),
      DateOps.from(28, FEBRUARY, Year.of(2006)),
      DateOps.from(14, FEBRUARY, Year.of(2006)),
      DateOps.from(30, SEPTEMBER, Year.of(2006)),
      DateOps.from(31, OCTOBER, Year.of(2006)),
      DateOps.from(31, AUGUST, Year.of(2007)),
      DateOps.from(28, FEBRUARY, Year.of(2008)),
      DateOps.from(28, FEBRUARY, Year.of(2008)),
      DateOps.from(28, FEBRUARY, Year.of(2008)),
      DateOps.from(26, FEBRUARY, Year.of(2007)),
      DateOps.from(26, FEBRUARY, Year.of(2007)),
      DateOps.from(29, FEBRUARY, Year.of(2008)),
      DateOps.from(28, FEBRUARY, Year.of(2008)),
      DateOps.from(28, FEBRUARY, Year.of(2008))
    )
    val endDates = List(
      DateOps.from(20, FEBRUARY, Year.of(2007)),
      DateOps.from(20, AUGUST, Year.of(2007)),
      DateOps.from(20, FEBRUARY, Year.of(2008)),
      DateOps.from(20, AUGUST, Year.of(2008)),
      DateOps.from(20, FEBRUARY, Year.of(2009)),
      DateOps.from(20, AUGUST, Year.of(2009)),
      DateOps.from(28, FEBRUARY, Year.of(2007)),
      DateOps.from(31, AUGUST, Year.of(2007)),
      DateOps.from(29, FEBRUARY, Year.of(2008)),
      DateOps.from(31, AUGUST, Year.of(2008)),
      DateOps.from(28, FEBRUARY, Year.of(2009)),
      DateOps.from(31, AUGUST, Year.of(2009)),
      DateOps.from(28, FEBRUARY, Year.of(2006)),
      DateOps.from(28, FEBRUARY, Year.of(2006)),
      DateOps.from(3, MARCH, Year.of(2006)),
      DateOps.from(28, FEBRUARY, Year.of(2006)),
      DateOps.from(31, OCTOBER, Year.of(2006)),
      DateOps.from(28, NOVEMBER, Year.of(2006)),
      DateOps.from(28, FEBRUARY, Year.of(2008)),
      DateOps.from(28, AUGUST, Year.of(2008)),
      DateOps.from(30, AUGUST, Year.of(2008)),
      DateOps.from(31, AUGUST, Year.of(2008)),
      DateOps.from(28, FEBRUARY, Year.of(2008)),
      DateOps.from(29, FEBRUARY, Year.of(2008)),
      DateOps.from(28, FEBRUARY, Year.of(2009)),
      DateOps.from(30, MARCH, Year.of(2008)),
      DateOps.from(31, MARCH, Year.of(2008))
    )
    val expecteds = List(180, 180, 180, 180, 180, 180,
      178, 183, 179, 182, 178, 183,
      28, 28, 5, 14, 30, 28,
      178, 180, 182, 183, 362, 363,
      359, 32, 33)

    val dayCounter = Thirty360(Convention.BondBasis)
    startDates zip endDates zip expecteds foreach { case ((startdate, enddate), expected) =>
      val calculated = dayCounter.dayCount(startdate, enddate)
      require(calculated == expected, s"from $startdate to $enddate \n calculated: $calculated  expected:   $expected")
    }
  }

  "Thirty360_EurobondBasis" should "pass all testing for default policy." in {
    import java.time.Month._
    import org.quantlib.time.implicits.Date._

    val startDates = List(
    DateOps.from(20, AUGUST, Year.of(2006)),
    DateOps.from(20, FEBRUARY, Year.of(2007)),
    DateOps.from(20, AUGUST, Year.of(2007)),
    DateOps.from(20, FEBRUARY, Year.of(2008)),
    DateOps.from(20, AUGUST, Year.of(2008)),
    DateOps.from(20, FEBRUARY, Year.of(2009)),

    DateOps.from(28, FEBRUARY, Year.of(2006)),
    DateOps.from(31, AUGUST, Year.of(2006)),
    DateOps.from(28, FEBRUARY, Year.of(2007)),
    DateOps.from(31, AUGUST, Year.of(2007)),
    DateOps.from(29, FEBRUARY, Year.of(2008)),
    DateOps.from(31, AUGUST, Year.of(2008)),
    DateOps.from(28, FEBRUARY, Year.of(2009)),
    DateOps.from(31, AUGUST, Year.of(2009)),
    DateOps.from(28, FEBRUARY, Year.of(2010)),
    DateOps.from(31, AUGUST, Year.of(2010)),
    DateOps.from(28, FEBRUARY, Year.of(2011)),
    DateOps.from(31, AUGUST, Year.of(2011)),

    DateOps.from(31, JANUARY, Year.of(2006)),
    DateOps.from(30, JANUARY, Year.of(2006)),
    DateOps.from(28, FEBRUARY, Year.of(2006)),
    DateOps.from(14, FEBRUARY, Year.of(2006)),
    DateOps.from(30, SEPTEMBER, Year.of(2006)),
    DateOps.from(31, OCTOBER, Year.of(2006)),
    DateOps.from(31, AUGUST, Year.of(2007)),
    DateOps.from(28, FEBRUARY, Year.of(2008)),
    DateOps.from(28, FEBRUARY, Year.of(2008)),
    DateOps.from(28, FEBRUARY, Year.of(2008)),
    DateOps.from(26, FEBRUARY, Year.of(2007)),
    DateOps.from(26, FEBRUARY, Year.of(2007)),
    DateOps.from(29, FEBRUARY, Year.of(2008)),
    DateOps.from(28, FEBRUARY, Year.of(2008)),
    DateOps.from(28, FEBRUARY, Year.of(2008))
    )
    val endDates = List(
    DateOps.from(20, FEBRUARY, Year.of(2007)),
    DateOps.from(20, AUGUST, Year.of(2007)),
    DateOps.from(20, FEBRUARY, Year.of(2008)),
    DateOps.from(20, AUGUST, Year.of(2008)),
    DateOps.from(20, FEBRUARY, Year.of(2009)),
    DateOps.from(20, AUGUST, Year.of(2009)),

    DateOps.from(31, AUGUST, Year.of(2006)),
    DateOps.from(28, FEBRUARY, Year.of(2007)),
    DateOps.from(31, AUGUST, Year.of(2007)),
    DateOps.from(29, FEBRUARY, Year.of(2008)),
    DateOps.from(31, AUGUST, Year.of(2008)),
    DateOps.from(28, FEBRUARY, Year.of(2009)),
    DateOps.from(31, AUGUST, Year.of(2009)),
    DateOps.from(28, FEBRUARY, Year.of(2010)),
    DateOps.from(31, AUGUST, Year.of(2010)),
    DateOps.from(28, FEBRUARY, Year.of(2011)),
    DateOps.from(31, AUGUST, Year.of(2011)),
    DateOps.from(29, FEBRUARY, Year.of(2012)),

    DateOps.from(28, FEBRUARY, Year.of(2006)),
    DateOps.from(28, FEBRUARY, Year.of(2006)),
    DateOps.from(3,  MARCH, Year.of(2006)),
    DateOps.from(28, FEBRUARY, Year.of(2006)),
    DateOps.from(31, OCTOBER, Year.of(2006)),
    DateOps.from(28, NOVEMBER, Year.of(2006)),
    DateOps.from(28, FEBRUARY, Year.of(2008)),
    DateOps.from(28, AUGUST, Year.of(2008)),
    DateOps.from(30, AUGUST, Year.of(2008)),
    DateOps.from(31, AUGUST, Year.of(2008)),
    DateOps.from(28, FEBRUARY, Year.of(2008)),
    DateOps.from(29, FEBRUARY, Year.of(2008)),
    DateOps.from(28, FEBRUARY, Year.of(2009)),
    DateOps.from(30, MARCH, Year.of(2008)),
    DateOps.from(31, MARCH, Year.of(2008))
    )

    val expecteds = List(180, 180, 180, 180, 180, 180,
      182, 178, 182, 179, 181, 178,
      182, 178, 182, 178, 182, 179,
      28,  28,   5,  14,  30,  28,
      178, 180, 182, 182, 362, 363,
      359,  32,  32)
    val dayCounter = Thirty360(Convention.EurobondBasis)
    startDates zip endDates zip expecteds foreach { case ((startdate, enddate), expected) =>
      val calculated = dayCounter.dayCount(startdate, enddate)
      require(calculated == expected, s"from $startdate to $enddate \n calculated: $calculated  expected:   $expected")
    }
  }

  "Intraday" should "pass all testing." in {
    import org.quantlib.time.implicits.DateTime._
    val d1 = DateOps.from(12, Month.FEBRUARY, Year.of(2015))
    val d2 = DateOps.from(14, Month.FEBRUARY, Year.of(2015), 12, 34, 17, 231298 * 1000)

    val tol = 1E-6

    val dayCounters = List(ActualActual(), Actual(365), Actual())

    dayCounters foreach { dc =>
      val nextDay = d1 + Period(1, Days)
      val anotherDay = nextDay + Period(1, Days)

      val expected = ((12 * 60 + 34) * 60 + 17 + 0.231298) * dc.yearFraction(d1, nextDay) / 86400.0 + dc.yearFraction(d1, anotherDay)

      val diff1 = Math.abs(dc.yearFraction(d1, d2))
      val diff2 = Math.abs(dc.yearFraction(d2, d1))
      val gap = diff1 - expected
      assert(Math.abs(dc.yearFraction(d1, d2) - expected) < tol,
        s" $diff1 , $diff2, $expected $gap can not reproduce result for day counter $dc")

      assert(Math.abs(dc.yearFraction(d2, d1) + expected) < tol,
        s"can not reproduce result for day counter $dc")
    }
  }

  "Business252" should "pass all testing." in {
    assert(true)
    //pending implementaion complete on Calendars
  }
}

