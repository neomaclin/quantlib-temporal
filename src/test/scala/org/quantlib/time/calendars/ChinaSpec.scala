package org.quantlib.time.calendars

import java.time.Month._
import java.time.Year

import org.quantlib.time.Period
import org.quantlib.time.enums.TimeUnit
import org.quantlib.time.implicits.Date._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
import org.scalatest._

/**
  * Created by neo on 19/03/2017.
  */
class ChinaSpec extends FlatSpec with Matchers {

  "ChinaSSE Celendar" should "pass all test" in {
    val expectedHolidays = List(
      
      // CHINA SHANGHAI SECURITIES EXCHANGE HOLIDAY LIST IN THE YEAR 2014
      DateOps.from(1, JANUARY, Year.of(2014)),
      DateOps.from(31, JANUARY, Year.of(2014)),
      DateOps.from(3, FEBRUARY, Year.of(2014)),
      DateOps.from(4, FEBRUARY, Year.of(2014)),
      DateOps.from(5, FEBRUARY, Year.of(2014)),
      DateOps.from(6, FEBRUARY, Year.of(2014)),
      DateOps.from(7, APRIL, Year.of(2014)),
      DateOps.from(1, MAY, Year.of(2014)),
      DateOps.from(2, MAY, Year.of(2014)),
      DateOps.from(2, JUNE, Year.of(2014)),
      DateOps.from(8, SEPTEMBER, Year.of(2014)),
      DateOps.from(1, OCTOBER, Year.of(2014)),
      DateOps.from(2, OCTOBER, Year.of(2014)),
      DateOps.from(3, OCTOBER, Year.of(2014)),
      DateOps.from(6, OCTOBER, Year.of(2014)),
      DateOps.from(7, OCTOBER, Year.of(2014)),

      // CHINA SHANGHAI SECURITIES EXCHANGE HOLIDAY LIST IN THE YEAR 2015
      DateOps.from(1, JANUARY, Year.of(2015)),
      DateOps.from(2, JANUARY, Year.of(2015)),
      DateOps.from(18, FEBRUARY, Year.of(2015)),
      DateOps.from(19, FEBRUARY, Year.of(2015)),
      DateOps.from(20, FEBRUARY, Year.of(2015)),
      DateOps.from(23, FEBRUARY, Year.of(2015)),
      DateOps.from(24, FEBRUARY, Year.of(2015)),
      DateOps.from(6, APRIL, Year.of(2015)),
      DateOps.from(1, MAY, Year.of(2015)),
      DateOps.from(22, JUNE, Year.of(2015)),
      DateOps.from(3, SEPTEMBER, Year.of(2015)),
      DateOps.from(4, SEPTEMBER, Year.of(2015)),
      DateOps.from(1, OCTOBER, Year.of(2015)),
      DateOps.from(2, OCTOBER, Year.of(2015)),
      DateOps.from(5, OCTOBER, Year.of(2015)),
      DateOps.from(6, OCTOBER, Year.of(2015)),
      DateOps.from(7, OCTOBER, Year.of(2015)),

      // CHINA SHANGHAI SECURITIES EXCHANGE HOLIDAY LIST IN THE YEAR 2016
      DateOps.from(1, JANUARY, Year.of(2016)),
      DateOps.from(8, FEBRUARY, Year.of(2016)),
      DateOps.from(9, FEBRUARY, Year.of(2016)),
      DateOps.from(10, FEBRUARY, Year.of(2016)),
      DateOps.from(11, FEBRUARY, Year.of(2016)),
      DateOps.from(12, FEBRUARY, Year.of(2016)),
      DateOps.from(4, APRIL, Year.of(2016)),
      DateOps.from(2, MAY, Year.of(2016)),
      DateOps.from(9, JUNE, Year.of(2016)),
      DateOps.from(10, JUNE, Year.of(2016)),
      DateOps.from(15, SEPTEMBER, Year.of(2016)),
      DateOps.from(16, SEPTEMBER, Year.of(2016)),
      DateOps.from(3, OCTOBER, Year.of(2016)),
      DateOps.from(4, OCTOBER, Year.of(2016)),
      DateOps.from(5, OCTOBER, Year.of(2016)),
      DateOps.from(6, OCTOBER, Year.of(2016)),
      DateOps.from(7, OCTOBER, Year.of(2016)),

      // CHINA SHANGHAI SECURITIES EXCHANGE HOLIDAY LIST IN THE YEAR 2017
      DateOps.from(2, JANUARY, Year.of(2017)),
      DateOps.from(27, JANUARY, Year.of(2017)),
      DateOps.from(30, JANUARY, Year.of(2017)),
      DateOps.from(31, JANUARY, Year.of(2017)),
      DateOps.from(1, FEBRUARY, Year.of(2017)),
      DateOps.from(2, FEBRUARY, Year.of(2017)),
      DateOps.from(3, APRIL, Year.of(2017)),
      DateOps.from(4, APRIL, Year.of(2017)),
      DateOps.from(1, MAY, Year.of(2017)),
      DateOps.from(29, MAY, Year.of(2017)),
      DateOps.from(30, MAY, Year.of(2017)),
      DateOps.from(2, OCTOBER, Year.of(2017)),
      DateOps.from(3, OCTOBER, Year.of(2017)),
      DateOps.from(4, OCTOBER, Year.of(2017)),
      DateOps.from(5, OCTOBER, Year.of(2017)),
      DateOps.from(6, OCTOBER, Year.of(2017))

    )

    val china = China(China.Market.SSE)
    val calculatedHolidays = china.holidays(
      DateOps.from(1, JANUARY, Year.of(2014)),
      DateOps.from(31, DECEMBER, Year.of(2017)))


    assert(calculatedHolidays.length == expectedHolidays.length)

    calculatedHolidays zip expectedHolidays foreach { case (calculated, expected) =>
      assert(calculated == expected)
    }
  }

  "ChinaIB Celendar" should "pass all test" in {
    val expectedWorkingWeekEnds = List(
      DateOps.from(26, JANUARY, Year.of(2014)),
      DateOps.from(8, FEBRUARY, Year.of(2014)),
      DateOps.from(4, MAY, Year.of(2014)),
      DateOps.from(28, SEPTEMBER, Year.of(2014)),
      DateOps.from(11, OCTOBER, Year.of(2014)),

      // CHINA INTER BANK WORKING WEEKENDS LIST IN THE YEAR 2015
      DateOps.from(4, JANUARY, Year.of(2015)),
      DateOps.from(15, FEBRUARY, Year.of(2015)),
      DateOps.from(28, FEBRUARY, Year.of(2015)),
      DateOps.from(6, SEPTEMBER, Year.of(2015)),
      DateOps.from(10, OCTOBER, Year.of(2015)),

      // CHINA INTER BANK WORKING WEEKENDS LIST IN THE YEAR 2016
      DateOps.from(6, FEBRUARY, Year.of(2016)),
      DateOps.from(14, FEBRUARY, Year.of(2016)),
      DateOps.from(12, JUNE, Year.of(2016)),
      DateOps.from(18, SEPTEMBER, Year.of(2016)),
      DateOps.from(8, OCTOBER, Year.of(2016)),
      DateOps.from(9, OCTOBER, Year.of(2016)),

      // CHINA INTER BANK WORKING WEEKENDS LIST IN THE YEAR 2017
      DateOps.from(22, JANUARY, Year.of(2017)),
      DateOps.from(4, FEBRUARY, Year.of(2017)),
      DateOps.from(1, APRIL, Year.of(2017)),
      DateOps.from(27, MAY, Year.of(2017)),
      DateOps.from(30, SEPTEMBER, Year.of(2017))
    )

    val china = China(China.Market.IB)

    val start = DateOps.from(1,JANUARY, Year.of(2014))
    val end = DateOps.from(31, DECEMBER, Year.of(2017))
    var current = start
    var k = 0
    while (current <= end) {
      if (china.considerBusinessDay(current) && china.isWeekend(current) ) {
        assert(expectedWorkingWeekEnds(k) == current)
        k = k + 1
      }
      current = current + Period(1, TimeUnit.Days)
    }

    assert(k == expectedWorkingWeekEnds.length)
  }

}
