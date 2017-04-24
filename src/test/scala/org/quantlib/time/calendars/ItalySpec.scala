package org.quantlib.time.calendars

import org.scalatest._
import org.quantlib.time.implicits.Date._
import java.time.Month._
import java.time.Year

import org.quantlib.time.implicits.DateOps
/**
  * Created by neo on 19/03/2017.
  */
class ItalySpec extends  FlatSpec with Matchers {

  val exptectedHolidays = List(
    DateOps.from(1,JANUARY,Year.of(2002)),
    DateOps.from(29,MARCH,Year.of(2002)),
    DateOps.from(1,APRIL,Year.of(2002)),
    DateOps.from(1,MAY,Year.of(2002)),
    DateOps.from(15,AUGUST,Year.of(2002)),
    DateOps.from(24,DECEMBER,Year.of(2002)),
    DateOps.from(25,DECEMBER,Year.of(2002)),
    DateOps.from(26,DECEMBER,Year.of(2002)),
    DateOps.from(31,DECEMBER,Year.of(2002)),

    DateOps.from(1,JANUARY,Year.of(2003)),
    DateOps.from(18,APRIL,Year.of(2003)),
    DateOps.from(21,APRIL,Year.of(2003)),
    DateOps.from(1,MAY,Year.of(2003)),
    DateOps.from(15,AUGUST,Year.of(2003)),
    DateOps.from(24,DECEMBER,Year.of(2003)),
    DateOps.from(25,DECEMBER,Year.of(2003)),
    DateOps.from(26,DECEMBER,Year.of(2003)),
    DateOps.from(31,DECEMBER,Year.of(2003)),

    DateOps.from(1,JANUARY,Year.of(2004)),
    DateOps.from(9,APRIL,Year.of(2004)),
    DateOps.from(12,APRIL,Year.of(2004)),
    DateOps.from(24,DECEMBER,Year.of(2004)),
    DateOps.from(31,DECEMBER,Year.of(2004))
  )

  "ItalyExchange Celendar" should "pass all test" in {
    val italy = Italy(Italy.Market.Exchange)
    val calculatedHolidays = BusinessCalendar.holidays(italy,
      DateOps.from(1, JANUARY, Year.of(2002)),
      DateOps.from(31, DECEMBER, Year.of(2004)))


    assert(calculatedHolidays.length == exptectedHolidays.length)
    calculatedHolidays zip exptectedHolidays foreach { case (calculated, expected) =>
      assert(calculated == expected)
    }
  }
}
