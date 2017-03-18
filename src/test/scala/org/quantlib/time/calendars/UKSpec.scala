package org.quantlib.time.calendars

import java.time.Month._
import java.time.Year

import org.quantlib.time.implicits.DateOps
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by neo on 19/03/2017.
  */
class UKSpec extends FlatSpec with Matchers {

  "UKSettlement Celendar" should "pass all test" in {
    import org.quantlib.time.implicits.Date._
    val ukSettlement = UnitedKingdom(UnitedKingdom.Market.Settlement)

    val expectatedHolidays = List(
      DateOps.from(1,JANUARY,Year.of(2004)),
      DateOps.from(9,APRIL,Year.of(2004)),
      DateOps.from(12,APRIL,Year.of(2004)),
      DateOps.from(3,MAY,Year.of(2004)),
      DateOps.from(31,MAY,Year.of(2004)),
      DateOps.from(30,AUGUST,Year.of(2004)),
      DateOps.from(27,DECEMBER,Year.of(2004)),
      DateOps.from(28,DECEMBER,Year.of(2004)),

      DateOps.from(3,JANUARY,Year.of(2005)),
      DateOps.from(25,MARCH,Year.of(2005)),
      DateOps.from(28,MARCH,Year.of(2005)),
      DateOps.from(2,MAY,Year.of(2005)),
      DateOps.from(30,MAY,Year.of(2005)),
      DateOps.from(29,AUGUST,Year.of(2005)),
      DateOps.from(26,DECEMBER,Year.of(2005)),
      DateOps.from(27,DECEMBER,Year.of(2005)),

      DateOps.from(2,JANUARY,Year.of(2006)),
      DateOps.from(14,APRIL,Year.of(2006)),
      DateOps.from(17,APRIL,Year.of(2006)),
      DateOps.from(1,MAY,Year.of(2006)),
      DateOps.from(29,MAY,Year.of(2006)),
      DateOps.from(28,AUGUST,Year.of(2006)),
      DateOps.from(25,DECEMBER,Year.of(2006)),
      DateOps.from(26,DECEMBER,Year.of(2006)),

      DateOps.from(1,JANUARY,Year.of(2007)),
      DateOps.from(6,APRIL,Year.of(2007)),
      DateOps.from(9,APRIL,Year.of(2007)),
      DateOps.from(7,MAY,Year.of(2007)),
      DateOps.from(28,MAY,Year.of(2007)),
      DateOps.from(27,AUGUST,Year.of(2007)),
      DateOps.from(25,DECEMBER,Year.of(2007)),
      DateOps.from(26,DECEMBER,Year.of(2007))
    )

    val holidays = BusinessCalendar.holidays(ukSettlement,
      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(31, DECEMBER, Year.of(2007)))

    assert(holidays.length == expectatedHolidays.length,
      s"there were ${expectatedHolidays.size} expected holidays, while there are ${holidays.size} calculated holidays")

    holidays zip expectatedHolidays foreach { case (calculated,expected) =>
      assert(expected == calculated)
    }
  }

    "UKExchange Celendar" should "pass all test" in {
      import org.quantlib.time.implicits.Date._
      val ukExchages = UnitedKingdom(UnitedKingdom.Market.Exchange)

      val expectatedHolidays = List(
        DateOps.from(1,JANUARY,Year.of(2004)),
        DateOps.from(9,APRIL,Year.of(2004)),
        DateOps.from(12,APRIL,Year.of(2004)),
        DateOps.from(3,MAY,Year.of(2004)),
        DateOps.from(31,MAY,Year.of(2004)),
        DateOps.from(30,AUGUST,Year.of(2004)),
        DateOps.from(27,DECEMBER,Year.of(2004)),
        DateOps.from(28,DECEMBER,Year.of(2004)),

        DateOps.from(3,JANUARY,Year.of(2005)),
        DateOps.from(25,MARCH,Year.of(2005)),
        DateOps.from(28,MARCH,Year.of(2005)),
        DateOps.from(2,MAY,Year.of(2005)),
        DateOps.from(30,MAY,Year.of(2005)),
        DateOps.from(29,AUGUST,Year.of(2005)),
        DateOps.from(26,DECEMBER,Year.of(2005)),
        DateOps.from(27,DECEMBER,Year.of(2005)),

        DateOps.from(2,JANUARY,Year.of(2006)),
        DateOps.from(14,APRIL,Year.of(2006)),
        DateOps.from(17,APRIL,Year.of(2006)),
        DateOps.from(1,MAY,Year.of(2006)),
        DateOps.from(29,MAY,Year.of(2006)),
        DateOps.from(28,AUGUST,Year.of(2006)),
        DateOps.from(25,DECEMBER,Year.of(2006)),
        DateOps.from(26,DECEMBER,Year.of(2006)),

        DateOps.from(1,JANUARY,Year.of(2007)),
        DateOps.from(6,APRIL,Year.of(2007)),
        DateOps.from(9,APRIL,Year.of(2007)),
        DateOps.from(7,MAY,Year.of(2007)),
        DateOps.from(28,MAY,Year.of(2007)),
        DateOps.from(27,AUGUST,Year.of(2007)),
        DateOps.from(25,DECEMBER,Year.of(2007)),
        DateOps.from(26,DECEMBER,Year.of(2007))


      )
      val holidays = BusinessCalendar.holidays(ukExchages,
        DateOps.from(1, JANUARY, Year.of(2004)),
        DateOps.from(31, DECEMBER, Year.of(2007)))

      assert(holidays.length == expectatedHolidays.length,
        s"there were ${expectatedHolidays.size} expected holidays, while there are ${holidays.size} calculated holidays")

      holidays zip expectatedHolidays foreach { case (calculated,expected) =>
        assert(expected == calculated)
      }
    }

  "UKMetal Celendar" should "pass all test" in {
    import org.quantlib.time.implicits.Date._
    val ukSMetals = UnitedKingdom(UnitedKingdom.Market.Metals)

    val expectatedHolidays = List(
      DateOps.from(1,JANUARY,Year.of(2004)),
      DateOps.from(9,APRIL,Year.of(2004)),
      DateOps.from(12,APRIL,Year.of(2004)),
      DateOps.from(3,MAY,Year.of(2004)),
      DateOps.from(31,MAY,Year.of(2004)),
      DateOps.from(30,AUGUST,Year.of(2004)),
      DateOps.from(27,DECEMBER,Year.of(2004)),
      DateOps.from(28,DECEMBER,Year.of(2004)),

      DateOps.from(3,JANUARY,Year.of(2005)),
      DateOps.from(25,MARCH,Year.of(2005)),
      DateOps.from(28,MARCH,Year.of(2005)),
      DateOps.from(2,MAY,Year.of(2005)),
      DateOps.from(30,MAY,Year.of(2005)),
      DateOps.from(29,AUGUST,Year.of(2005)),
      DateOps.from(26,DECEMBER,Year.of(2005)),
      DateOps.from(27,DECEMBER,Year.of(2005)),

      DateOps.from(2,JANUARY,Year.of(2006)),
      DateOps.from(14,APRIL,Year.of(2006)),
      DateOps.from(17,APRIL,Year.of(2006)),
      DateOps.from(1,MAY,Year.of(2006)),
      DateOps.from(29,MAY,Year.of(2006)),
      DateOps.from(28,AUGUST,Year.of(2006)),
      DateOps.from(25,DECEMBER,Year.of(2006)),
      DateOps.from(26,DECEMBER,Year.of(2006)),

      DateOps.from(1,JANUARY,Year.of(2007)),
      DateOps.from(6,APRIL,Year.of(2007)),
      DateOps.from(9,APRIL,Year.of(2007)),
      DateOps.from(7,MAY,Year.of(2007)),
      DateOps.from(28,MAY,Year.of(2007)),
      DateOps.from(27,AUGUST,Year.of(2007)),
      DateOps.from(25,DECEMBER,Year.of(2007)),
      DateOps.from(26,DECEMBER,Year.of(2007))
    )

    val holidays = BusinessCalendar.holidays(ukSMetals,
      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(31, DECEMBER, Year.of(2007)))

    assert(holidays.length == expectatedHolidays.length,
      s"there were ${expectatedHolidays.size} expected holidays, while there are ${holidays.size} calculated holidays")

    holidays zip expectatedHolidays foreach { case (calculated,expected) =>
      assert(expected == calculated)
    }
  }
//
//    val beforeUniformHolidayAct = List(
//      DateOps.from(2, JANUARY, Year.of(1961)),
//      DateOps.from(22, FEBRUARY, Year.of(1961)),
//      DateOps.from(30, MAY, Year.of(1961)),
//      DateOps.from(4, JULY, Year.of(1961)),
//      DateOps.from(4, SEPTEMBER, Year.of(1961)),
//      DateOps.from(10, NOVEMBER, Year.of(1961)),
//      DateOps.from(23, NOVEMBER, Year.of(1961)),
//      DateOps.from(25, DECEMBER, Year.of(1961))
//    )
//
//    val hol = BusinessCalendar.holidays(usSettlement,
//      DateOps.from(1, JANUARY, Year.of(1961)),
//      DateOps.from(31, DECEMBER, Year.of(1961)))
//
//    assert(hol.size == beforeUniformHolidayAct.size,
//      s"there were ${hol.size} expected holidays, while there are ${beforeUniformHolidayAct.size} calculated holidays")
//
//    hol zip beforeUniformHolidayAct foreach { case (expected, calculated) =>
//      assert(expected == calculated)
//    }

}
