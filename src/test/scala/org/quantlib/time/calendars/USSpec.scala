package org.quantlib.time.calendars

import java.time.Month._
import java.time.Year

import org.quantlib.time.implicits.DateOps
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by neo on 19/03/2017.
  */
class USSpec extends FlatSpec with Matchers {

  "USSettlement Celendar" should "pass all test" in {
    import org.quantlib.time.implicits.Date._
    val usSettlement = UnitedStates(UnitedStates.Market.Settlement)

    val expectatedHolidays = List(
      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(19, JANUARY, Year.of(2004)),
      DateOps.from(16, FEBRUARY, Year.of(2004)),
      DateOps.from(31, MAY, Year.of(2004)),
      DateOps.from(5, JULY, Year.of(2004)),
      DateOps.from(6, SEPTEMBER, Year.of(2004)),
      DateOps.from(11, OCTOBER, Year.of(2004)),
      DateOps.from(11, NOVEMBER, Year.of(2004)),
      DateOps.from(25, NOVEMBER, Year.of(2004)),
      DateOps.from(24, DECEMBER, Year.of(2004)),
      DateOps.from(31, DECEMBER, Year.of(2004)),
      DateOps.from(17, JANUARY, Year.of(2005)),
      DateOps.from(21, FEBRUARY, Year.of(2005)),
      DateOps.from(30, MAY, Year.of(2005)),
      DateOps.from(4, JULY, Year.of(2005)),
      DateOps.from(5, SEPTEMBER, Year.of(2005)),
      DateOps.from(10, OCTOBER, Year.of(2005)),
      DateOps.from(11, NOVEMBER, Year.of(2005)),
      DateOps.from(24, NOVEMBER, Year.of(2005)),
      DateOps.from(26, DECEMBER, Year.of(2005))
    )

    val holidays = BusinessCalendar.holidays(usSettlement,
      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(31, DECEMBER, Year.of(2005)))

    assert(holidays.length == expectatedHolidays.length,
      s"there were ${expectatedHolidays.size} expected holidays, while there are ${holidays.size} calculated holidays")


    val beforeUniformHolidayAct = List(
      DateOps.from(2, JANUARY, Year.of(1961)),
      DateOps.from(22, FEBRUARY, Year.of(1961)),
      DateOps.from(30, MAY, Year.of(1961)),
      DateOps.from(4, JULY, Year.of(1961)),
      DateOps.from(4, SEPTEMBER, Year.of(1961)),
      DateOps.from(10, NOVEMBER, Year.of(1961)),
      DateOps.from(23, NOVEMBER, Year.of(1961)),
      DateOps.from(25, DECEMBER, Year.of(1961))
    )

    val hol = BusinessCalendar.holidays(usSettlement,
      DateOps.from(1, JANUARY, Year.of(1961)),
      DateOps.from(31, DECEMBER, Year.of(1961)))

    assert(hol.size == beforeUniformHolidayAct.size,
      s"there were ${hol.size} expected holidays, while there are ${beforeUniformHolidayAct.size} calculated holidays")

    hol zip beforeUniformHolidayAct foreach { case (expected, calculated) =>
      assert(expected == calculated)
    }
  }

  "USGovernmentBondMarket Celendar" should "pass all test" in {
    import org.quantlib.time.implicits.Date._
    val usGovenmentBond = UnitedStates(UnitedStates.Market.GovernmentBond)

    val expected = List(
      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(19, JANUARY, Year.of(2004)),
      DateOps.from(16, FEBRUARY, Year.of(2004)),
      DateOps.from(9, APRIL, Year.of(2004)),
      DateOps.from(31, MAY, Year.of(2004)),
      DateOps.from(5, JULY, Year.of(2004)),
      DateOps.from(6, SEPTEMBER, Year.of(2004)),
      DateOps.from(11, OCTOBER, Year.of(2004)),
      DateOps.from(11, NOVEMBER, Year.of(2004)),
      DateOps.from(25, NOVEMBER, Year.of(2004)),
      DateOps.from(24, DECEMBER, Year.of(2004))
    )
    val hol = BusinessCalendar.holidays(usGovenmentBond,
      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(31, DECEMBER, Year.of(2004)))

    assert(hol.size == expected.size,
      s"there were ${expected.size} expected holidays, while there are ${hol.size} calculated holidays")

    expected zip hol foreach { case (expected, calculated) =>
      assert(expected == calculated)
    }
  }

  "USNewYorkStockExchange Celendar" should "pass all test" in {
    import org.quantlib.time.implicits.Date._
    val usNYSE = UnitedStates(UnitedStates.Market.NYSE)

    val expected = List(
      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(19, JANUARY, Year.of(2004)),
      DateOps.from(16, FEBRUARY, Year.of(2004)),
      DateOps.from(9, APRIL, Year.of(2004)),
      DateOps.from(31, MAY, Year.of(2004)),
      DateOps.from(11, JUNE, Year.of(2004)),
      DateOps.from(5, JULY, Year.of(2004)),
      DateOps.from(6, SEPTEMBER, Year.of(2004)),
      DateOps.from(25, NOVEMBER, Year.of(2004)),
      DateOps.from(24, DECEMBER, Year.of(2004)),

      DateOps.from(17, JANUARY, Year.of(2005)),
      DateOps.from(21, FEBRUARY, Year.of(2005)),
      DateOps.from(25, MARCH, Year.of(2005)),
      DateOps.from(30, MAY, Year.of(2005)),
      DateOps.from(4, JULY, Year.of(2005)),
      DateOps.from(5, SEPTEMBER, Year.of(2005)),
      DateOps.from(24, NOVEMBER, Year.of(2005)),
      DateOps.from(26, DECEMBER, Year.of(2005)),

      DateOps.from(2, JANUARY, Year.of(2006)),
      DateOps.from(16, JANUARY, Year.of(2006)),
      DateOps.from(20, FEBRUARY, Year.of(2006)),
      DateOps.from(14, APRIL, Year.of(2006)),
      DateOps.from(29, MAY, Year.of(2006)),
      DateOps.from(4, JULY, Year.of(2006)),
      DateOps.from(4, SEPTEMBER, Year.of(2006)),
      DateOps.from(23, NOVEMBER, Year.of(2006)),
      DateOps.from(25, DECEMBER, Year.of(2006))
    )

    val hol = BusinessCalendar.holidays(usNYSE,
      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(31, DECEMBER, Year.of(2006)))

    assert(hol.size == expected.size,
      s"there were ${expected.size} expected holidays, while there are ${hol.size} calculated holidays")

    expected zip hol foreach { case (expected, calculated) =>
      assert(expected == calculated)
    }
  }

  "USNewYorkStockExchange Celendar" should "pass all test on historical days" in {
    import org.quantlib.time.implicits.Date._
    val usNYSE = UnitedStates(UnitedStates.Market.NYSE)

    val expected = List(
      DateOps.from(30, OCTOBER, Year.of(2012)), // HURRICANE SANDY
      DateOps.from(29, OCTOBER, Year.of(2012)), // HURRICANE SANDY
      DateOps.from(11, JUNE, Year.of(2004)), // REAGAN'S FUNERAL
      DateOps.from(14, SEPTEMBER, Year.of(2001)), // SEPTEMBER 11, 2001
      DateOps.from(13, SEPTEMBER, Year.of(2001)), // SEPTEMBER 11, 2001
      DateOps.from(12, SEPTEMBER, Year.of(2001)), // SEPTEMBER 11, 2001
      DateOps.from(11, SEPTEMBER, Year.of(2001)), // SEPTEMBER 11, 2001
      DateOps.from(27, APRIL, Year.of(1994)), // NIXON'S FUNERAL.
      DateOps.from(27, SEPTEMBER, Year.of(1985)), // HURRICANE GLORIA
      DateOps.from(14, JULY, Year.of(1977)), // 1977 BLACKOUT
      DateOps.from(25, JANUARY, Year.of(1973)), // JOHNSON'S FUNERAL.
      DateOps.from(28, DECEMBER, Year.of(1972)), // TRUMAN'S FUNERAL
      DateOps.from(21, JULY, Year.of(1969)), // LUNAR EXPLORATION NAT. DAY
      DateOps.from(31, MARCH, Year.of(1969)), // EISENHOWER'S FUNERAL
      DateOps.from(10, FEBRUARY, Year.of(1969)), // HEAVY SNOW
      DateOps.from(5, JULY, Year.of(1968)), // DAY AFTER INDEPENDENCE DAY
      DateOps.from(9, APRIL, Year.of(1968)), // MOURNING FOR MLK
      DateOps.from(24, DECEMBER, Year.of(1965)), // CHRISTMAS EVE
      DateOps.from(25, NOVEMBER, Year.of(1963)), // KENNEDY'S FUNERAL
      DateOps.from(29, MAY, Year.of(1961)), // DAY BEFORE DECORATION DAY
      DateOps.from(26, DECEMBER, Year.of(1958)), // DAY AFTER CHRISTMAS
      DateOps.from(24, DECEMBER, Year.of(1956)), // CHRISTMAS EVE
      DateOps.from(24, DECEMBER, Year.of(1954)), // CHRISTMAS EVE
      // June 12-Dec. 31, 1968
      // Four day week (closed on Wednesdays) - Paperwork Crisis
      DateOps.from(12, JUNE, Year.of(1968)),
      DateOps.from(19, JUNE, Year.of(1968)),
      DateOps.from(26, JUNE, Year.of(1968)),
      DateOps.from(3, JULY, Year.of(1968)),
      DateOps.from(10, JULY, Year.of(1968)),
      DateOps.from(17, JULY, Year.of(1968)),
      DateOps.from(20, NOVEMBER, Year.of(1968)),
      DateOps.from(27, NOVEMBER, Year.of(1968)),
      DateOps.from(4, DECEMBER, Year.of(1968)),
      DateOps.from(11, DECEMBER, Year.of(1968)),
      DateOps.from(18, DECEMBER, Year.of(1968)),
      // PRESIDENTIAL ELECTION DAYS
      DateOps.from(4, NOVEMBER, Year.of(1980)),
      DateOps.from(2, NOVEMBER, Year.of(1976)),
      DateOps.from(7, NOVEMBER, Year.of(1972)),
      DateOps.from(5, NOVEMBER, Year.of(1968)),
      DateOps.from(3, NOVEMBER, Year.of(1964))
    )

    expected.foreach { date =>
      assert(usNYSE.considerHoliday(date), s"$date should be holiday (historical close)")
    }
  }

}
