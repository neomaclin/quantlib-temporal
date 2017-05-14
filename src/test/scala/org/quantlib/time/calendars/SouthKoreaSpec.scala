package org.quantlib.time.calendars

import java.time.Month._
import java.time.Year

import org.quantlib.time.implicits.Date._
import org.quantlib.time.implicits.DateOps
import org.scalatest._

/**
  * Created by neo on 19/03/2017.
  */
class SouthKoreaSpec  extends  FlatSpec with Matchers {

  "SouthKoreanSettlement Celendar" should "pass all test" in {
    val expectedHolidays = List(
      DateOps.from(1,JANUARY,Year.of(2004)),
      DateOps.from(21,JANUARY,Year.of(2004)),
      DateOps.from(22,JANUARY,Year.of(2004)),
      DateOps.from(23,JANUARY,Year.of(2004)),
      DateOps.from(1,MARCH,Year.of(2004)),
      DateOps.from(5,APRIL,Year.of(2004)),
      DateOps.from(15,APRIL,Year.of(2004)), // ELECTION DAY
      //    DateOps.from(1,MAY,Year.of(2004)), // SATURDAY
      DateOps.from(5,MAY,Year.of(2004)),
      DateOps.from(26,MAY,Year.of(2004)),
      //    DateOps.from(6,JUNE,Year.of(2004)), // SUNDAY
      //    DateOps.from(17,JULY,Year.of(2004)), // SATURDAY
      //    DateOps.from(15,AUGUST,Year.of(2004)), // SUNDAY
      DateOps.from(27,SEPTEMBER,Year.of(2004)),
      DateOps.from(28,SEPTEMBER,Year.of(2004)),
      DateOps.from(29,SEPTEMBER,Year.of(2004)),
      //    DateOps.from(3,OCTOBER,Year.of(2004)), // SUNDAY
      //    DateOps.from(25,DECEMBER,Year.of(2004)), // SATURDAY

      //    DateOps.from(1,JANUARY,Year.of(2005)), // SATURDAY
      DateOps.from(8,FEBRUARY,Year.of(2005)),
      DateOps.from(9,FEBRUARY,Year.of(2005)),
      DateOps.from(10,FEBRUARY,Year.of(2005)),
      DateOps.from(1,MARCH,Year.of(2005)),
      DateOps.from(5,APRIL,Year.of(2005)),
      DateOps.from(5,MAY,Year.of(2005)),
      //    DateOps.from(15,MAY,Year.of(2005)), // SUNDAY
      DateOps.from(6,JUNE,Year.of(2005)),
      //    DateOps.from(17,JULY,Year.of(2005)), // SUNDAY
      DateOps.from(15,AUGUST,Year.of(2005)),
      //    DateOps.from(17,SEPTEMBER,Year.of(2005)), // SATURDAY
      //    DateOps.from(18,SEPTEMBER,Year.of(2005)), // SUNDAY
      DateOps.from(19,SEPTEMBER,Year.of(2005)),
      DateOps.from(3,OCTOBER,Year.of(2005)),
      //    DateOps.from(25,DECEMBER,Year.of(2005)), // SUNDAY

      //    DateOps.from(1,JANUARY,Year.of(2006)), // SUNDAY
      //    DateOps.from(28,JANUARY,Year.of(2006)), // SATURDAY
      //    DateOps.from(29,JANUARY,Year.of(2006)), // SUNDAY
      DateOps.from(30,JANUARY,Year.of(2006)),
      DateOps.from(1,MARCH,Year.of(2006)),
      DateOps.from(1,MAY,Year.of(2006)),
      DateOps.from(5,MAY,Year.of(2006)),
      DateOps.from(31,MAY,Year.of(2006)), // ELECTION
      DateOps.from(6,JUNE,Year.of(2006)),
      DateOps.from(17,JULY,Year.of(2006)),
      DateOps.from(15,AUGUST,Year.of(2006)),
      DateOps.from(3,OCTOBER,Year.of(2006)),
      DateOps.from(5,OCTOBER,Year.of(2006)),
      DateOps.from(6,OCTOBER,Year.of(2006)),
      //    DateOps.from(7,OCTOBER,Year.of(2006)), // SATURDAY
      DateOps.from(25,DECEMBER,Year.of(2006)),

      DateOps.from(1,JANUARY,Year.of(2007)),
      //    DateOps.from(17,FEBRUARY,Year.of(2007)), // SATURDAY
      //    DateOps.from(18,FEBRUARY,Year.of(2007)), // SUNDAY
      DateOps.from(19,FEBRUARY,Year.of(2007)),
      DateOps.from(1,MARCH,Year.of(2007)),
      DateOps.from(1,MAY,Year.of(2007)),
      //    DateOps.from(5,MAY,Year.of(2007)), // SATURDAY
      DateOps.from(24,MAY,Year.of(2007)),
      DateOps.from(6,JUNE,Year.of(2007)),
      DateOps.from(17,JULY,Year.of(2007)),
      DateOps.from(15,AUGUST,Year.of(2007)),
      DateOps.from(24,SEPTEMBER,Year.of(2007)),
      DateOps.from(25,SEPTEMBER,Year.of(2007)),
      DateOps.from(26,SEPTEMBER,Year.of(2007)),
      DateOps.from(3,OCTOBER,Year.of(2007)),
      DateOps.from(19,DECEMBER,Year.of(2007)), // ELECTION
      DateOps.from(25,DECEMBER,Year.of(2007))
    )

    val sk = SouthKorea(SouthKorea.Market.Settlement)
    val calculatedHolidays = sk.holidays(
      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(31, DECEMBER, Year.of(2007))
    )


    assert(calculatedHolidays.length == expectedHolidays.length)

    calculatedHolidays zip expectedHolidays foreach { case (calculated, expected) =>
      assert(calculated == expected)
    }

  }
  "KoreaStockExchange Celendar" should "pass all test" in {
    val expectedHolidays = List(
      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(21, JANUARY, Year.of(2004)),
      DateOps.from(22, JANUARY, Year.of(2004)),
      DateOps.from(23, JANUARY, Year.of(2004)),
      DateOps.from(1, MARCH, Year.of(2004)),
      DateOps.from(5, APRIL, Year.of(2004)),
      DateOps.from(15, APRIL, Year.of(2004)), //ELECTION DAY
      //    DateOps.from(1,MAY,Year.of(2004)), // SATURDAY
      DateOps.from(5, MAY, Year.of(2004)),
      DateOps.from(26, MAY, Year.of(2004)),
      //    DateOps.from(6,JUNE,Year.of(2004)), // SUNDAY
      //    DateOps.from(17,JULY,Year.of(2004)), // SATURDAY
      //    DateOps.from(15,AUGUST,Year.of(2004)), // SUNDAY
      DateOps.from(27, SEPTEMBER, Year.of(2004)),
      DateOps.from(28, SEPTEMBER, Year.of(2004)),
      DateOps.from(29, SEPTEMBER, Year.of(2004)),
      //    DateOps.from(3,OCTOBER,Year.of(2004)), // SUNDAY
      //    DateOps.from(25,DECEMBER,Year.of(2004)), // SATURDAY
      DateOps.from(31, DECEMBER, Year.of(2004)),

      //    DateOps.from(1,JANUARY,Year.of(2005)), // SATURDAY
      DateOps.from(8, FEBRUARY, Year.of(2005)),
      DateOps.from(9, FEBRUARY, Year.of(2005)),
      DateOps.from(10, FEBRUARY, Year.of(2005)),
      DateOps.from(1, MARCH, Year.of(2005)),
      DateOps.from(5, APRIL, Year.of(2005)),
      DateOps.from(5, MAY, Year.of(2005)),
      //    DateOps.from(15,MAY,Year.of(2005)), // SUNDAY
      DateOps.from(6, JUNE, Year.of(2005)),
      //    DateOps.from(17,JULY,Year.of(2005)), // SUNDAY
      DateOps.from(15, AUGUST, Year.of(2005)),
      //    DateOps.from(17,SEPTEMBER,Year.of(2005)), // SATURDAY
      //    DateOps.from(18,SEPTEMBER,Year.of(2005)), // SUNDAY
      DateOps.from(19, SEPTEMBER, Year.of(2005)),
      DateOps.from(3, OCTOBER, Year.of(2005)),
      //    DateOps.from(25,DECEMBER,Year.of(2005)), // SUNDAY
      DateOps.from(30, DECEMBER, Year.of(2005)),

      //    DateOps.from(1,JANUARY,Year.of(2006)), // SUNDAY
      //    DateOps.from(28,JANUARY,Year.of(2006)), // SATURDAY
      //    DateOps.from(29,JANUARY,Year.of(2006)), // SUNDAY
      DateOps.from(30, JANUARY, Year.of(2006)),
      DateOps.from(1, MARCH, Year.of(2006)),
      DateOps.from(1, MAY, Year.of(2006)),
      DateOps.from(5, MAY, Year.of(2006)),
      DateOps.from(31, MAY, Year.of(2006)), // ELECTION
      DateOps.from(6, JUNE, Year.of(2006)),
      DateOps.from(17, JULY, Year.of(2006)),
      DateOps.from(15, AUGUST, Year.of(2006)),
      DateOps.from(3, OCTOBER, Year.of(2006)),
      DateOps.from(5, OCTOBER, Year.of(2006)),
      DateOps.from(6, OCTOBER, Year.of(2006)),
      //    DateOps.from(7,OCTOBER,Year.of(2006)), // SATURDAY
      DateOps.from(25, DECEMBER, Year.of(2006)),
      DateOps.from(29, DECEMBER, Year.of(2006)),

      DateOps.from(1, JANUARY, Year.of(2007)),
      //    DateOps.from(17,FEBRUARY,Year.of(2007)), // SATURDAY
      //    DateOps.from(18,FEBRUARY,Year.of(2007)), // SUNDAY
      DateOps.from(19, FEBRUARY, Year.of(2007)),
      DateOps.from(1, MARCH, Year.of(2007)),
      DateOps.from(1, MAY, Year.of(2007)),
      //    DateOps.from(5,MAY,Year.of(2007)), // SATURDAY
      DateOps.from(24, MAY, Year.of(2007)),
      DateOps.from(6, JUNE, Year.of(2007)),
      DateOps.from(17, JULY, Year.of(2007)),
      DateOps.from(15, AUGUST, Year.of(2007)),
      DateOps.from(24, SEPTEMBER, Year.of(2007)),
      DateOps.from(25, SEPTEMBER, Year.of(2007)),
      DateOps.from(26, SEPTEMBER, Year.of(2007)),
      DateOps.from(3, OCTOBER, Year.of(2007)),
      DateOps.from(19, DECEMBER, Year.of(2007)), // ELECTION
      DateOps.from(25, DECEMBER, Year.of(2007)),
      DateOps.from(31, DECEMBER, Year.of(2007)))

    val sk = SouthKorea(SouthKorea.Market.KRX)
    val calculatedHolidays = sk.holidays(
      DateOps.from(1, JANUARY, Year.of(2004)),
      DateOps.from(31, DECEMBER, Year.of(2007)))


    assert(calculatedHolidays.length == expectedHolidays.length)

    calculatedHolidays zip expectedHolidays foreach { case (calculated, expected) =>
      assert(calculated == expected)
    }
  }
}
