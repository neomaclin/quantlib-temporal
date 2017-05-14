package org.quantlib.time.calendars

import java.time.Month._
import java.time.Year

import org.quantlib.time.implicits.DateOps
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by neo on 19/03/2017.
  */
class BrazilSpec extends FlatSpec with Matchers {

  "Brazil Celendar" should "pass all test" in {
    import org.quantlib.time.implicits.Date._

    val brazil = Brazil()

    val exptecteds = List(
      //DateOps.from(1,JANUARY,Year.of(2005)), // SATURDAY
      DateOps.from(7, FEBRUARY, Year.of(2005)),
      DateOps.from(8, FEBRUARY, Year.of(2005)),
      DateOps.from(25, MARCH, Year.of(2005)),
      DateOps.from(21, APRIL, Year.of(2005)),
      //DateOps.from(1,MAY,Year.of(2005)), // SUNDAY
      DateOps.from(26, MAY, Year.of(2005)),
      DateOps.from(7, SEPTEMBER, Year.of(2005)),
      DateOps.from(12, OCTOBER, Year.of(2005)),
      DateOps.from(2, NOVEMBER, Year.of(2005)),
      DateOps.from(15, NOVEMBER, Year.of(2005)),
      //DateOps.from(25,DECEMBER,Year.of(2005)), // SUNDAY

      //DateOps.from(1,JANUARY,Year.of(2006)), // SUNDAY
      DateOps.from(27, FEBRUARY, Year.of(2006)),
      DateOps.from(28, FEBRUARY, Year.of(2006)),
      DateOps.from(14, APRIL, Year.of(2006)),
      DateOps.from(21, APRIL, Year.of(2006)),
      DateOps.from(1, MAY, Year.of(2006)),
      DateOps.from(15, JUNE, Year.of(2006)),
      DateOps.from(7, SEPTEMBER, Year.of(2006)),
      DateOps.from(12, OCTOBER, Year.of(2006)),
      DateOps.from(2, NOVEMBER, Year.of(2006)),
      DateOps.from(15, NOVEMBER, Year.of(2006)),
      DateOps.from(25, DECEMBER, Year.of(2006))

    )
    val holidays = BusinessCalendar.holidays(brazil,
      DateOps.from(1, JANUARY, Year.of(2005)),
      DateOps.from(31, DECEMBER, Year.of(2006)))

    assert(holidays.length == exptecteds.length,
      s"there were ${exptecteds.size} expected holidays, while there are ${holidays.size} calculated holidays")

    holidays zip exptecteds foreach { case (calculated, expected) =>
      assert(expected == calculated)
    }
  }


}
