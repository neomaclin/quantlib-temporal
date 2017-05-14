package org.quantlib.time.calendars

import java.time.Month._
import java.time.Year

import org.quantlib.time.implicits.Date._
import org.quantlib.time.implicits.DateOps
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by neo on 19/03/2017.
  */
class GermanySpec extends FlatSpec with Matchers {

  "GermanyFrankfurt Celendar" should "pass all test" in {
    val calendar = Germany(Germany.Market.FrankfurtStockExchange)

    val exptecteds = List(
      DateOps.from(1, JANUARY, Year.of(2003)),
      DateOps.from(18, APRIL, Year.of(2003)),
      DateOps.from(21, APRIL, Year.of(2003)),
      DateOps.from(1, MAY, Year.of(2003)),
      DateOps.from(24, DECEMBER, Year.of(2003)),
      DateOps.from(25, DECEMBER, Year.of(2003)),
      DateOps.from(26, DECEMBER, Year.of(2003)),
      DateOps.from(31, DECEMBER, Year.of(2003)),

      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(9, APRIL, Year.of(2004)),
      DateOps.from(12, APRIL, Year.of(2004)),
      DateOps.from(24, DECEMBER, Year.of(2004)),
      DateOps.from(31, DECEMBER, Year.of(2004))
    )

    val holidays = BusinessCalendar.holidays(calendar,
      DateOps.from(1, JANUARY, Year.of(2003)),
      DateOps.from(31, DECEMBER, Year.of(2004))
    )


    assert(holidays.length == exptecteds.length,
      s"there were ${exptecteds.size} expected holidays, while there are ${holidays.size} calculated holidays")

    holidays zip exptecteds foreach { case (calculated, expected) =>
      assert(expected == calculated)
    }

  }
  "GermanyXetra Celendar" should "pass all test" in {
    val calendar = Germany(Germany.Market.Xetra)

    val exptecteds = List(
      DateOps.from(1, JANUARY, Year.of(2003)),
      DateOps.from(18, APRIL, Year.of(2003)),
      DateOps.from(21, APRIL, Year.of(2003)),
      DateOps.from(1, MAY, Year.of(2003)),
      DateOps.from(24, DECEMBER, Year.of(2003)),
      DateOps.from(25, DECEMBER, Year.of(2003)),
      DateOps.from(26, DECEMBER, Year.of(2003)),
      DateOps.from(31, DECEMBER, Year.of(2003)),

      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(9, APRIL, Year.of(2004)),
      DateOps.from(12, APRIL, Year.of(2004)),
      DateOps.from(24, DECEMBER, Year.of(2004)),
      DateOps.from(31, DECEMBER, Year.of(2004))
    )

    val holidays = BusinessCalendar.holidays(calendar,
      DateOps.from(1, JANUARY, Year.of(2003)),
      DateOps.from(31, DECEMBER, Year.of(2004))
    )


    assert(holidays.length == exptecteds.length,
      s"there were ${exptecteds.size} expected holidays, while there are ${holidays.size} calculated holidays")

    holidays zip exptecteds foreach { case (calculated, expected) =>
      assert(expected == calculated)
    }
  }
  "GermanyEurex Celendar" should "pass all test" in {
    val calendar = Germany(Germany.Market.Eurex)

    val exptecteds = List(
      DateOps.from(1, JANUARY, Year.of(2003)),
      DateOps.from(18, APRIL, Year.of(2003)),
      DateOps.from(21, APRIL, Year.of(2003)),
      DateOps.from(1, MAY, Year.of(2003)),
      DateOps.from(24, DECEMBER, Year.of(2003)),
      DateOps.from(25, DECEMBER, Year.of(2003)),
      DateOps.from(26, DECEMBER, Year.of(2003)),
      DateOps.from(31, DECEMBER, Year.of(2003)),

      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(9, APRIL, Year.of(2004)),
      DateOps.from(12, APRIL, Year.of(2004)),
      DateOps.from(24, DECEMBER, Year.of(2004)),
      DateOps.from(31, DECEMBER, Year.of(2004))
    )

    val holidays = BusinessCalendar.holidays(calendar,
      DateOps.from(1, JANUARY, Year.of(2003)),
      DateOps.from(31, DECEMBER, Year.of(2004))
    )


    assert(holidays.length == exptecteds.length,
      s"there were ${exptecteds.size} expected holidays, while there are ${holidays.size} calculated holidays")

    holidays zip exptecteds foreach { case (calculated, expected) =>
      assert(expected == calculated)
    }
  }
}
