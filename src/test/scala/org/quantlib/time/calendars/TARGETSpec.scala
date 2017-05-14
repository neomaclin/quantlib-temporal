package org.quantlib.time.calendars

import java.time.Month._
import java.time.Year

import org.quantlib.time.implicits.Date._
import org.quantlib.time.implicits.DateOps
import org.scalatest._

/**
  * Created by neo on 19/03/2017.
  */
class TARGETSpec extends FlatSpec with Matchers {

  "TARGET Calendar" should "pass all test" in {

    val calendar = TARGET()

    val exptecteds = List(
    DateOps.from(1,JANUARY,Year.of(1999)),
    DateOps.from(31,DECEMBER,Year.of(1999)),

    DateOps.from(21,APRIL,Year.of(2000)),
    DateOps.from(24,APRIL,Year.of(2000)),
    DateOps.from(1,MAY,Year.of(2000)),
    DateOps.from(25,DECEMBER,Year.of(2000)),
    DateOps.from(26,DECEMBER,Year.of(2000)),

    DateOps.from(1,JANUARY,Year.of(2001)),
    DateOps.from(13,APRIL,Year.of(2001)),
    DateOps.from(16,APRIL,Year.of(2001)),
    DateOps.from(1,MAY,Year.of(2001)),
    DateOps.from(25,DECEMBER,Year.of(2001)),
    DateOps.from(26,DECEMBER,Year.of(2001)),
    DateOps.from(31,DECEMBER,Year.of(2001)),

    DateOps.from(1,JANUARY,Year.of(2002)),
    DateOps.from(29,MARCH,Year.of(2002)),
    DateOps.from(1,APRIL,Year.of(2002)),
    DateOps.from(1,MAY,Year.of(2002)),
    DateOps.from(25,DECEMBER,Year.of(2002)),
    DateOps.from(26,DECEMBER,Year.of(2002)),

    DateOps.from(1,JANUARY,Year.of(2003)),
    DateOps.from(18,APRIL,Year.of(2003)),
    DateOps.from(21,APRIL,Year.of(2003)),
    DateOps.from(1,MAY,Year.of(2003)),
    DateOps.from(25,DECEMBER,Year.of(2003)),
    DateOps.from(26,DECEMBER,Year.of(2003)),

    DateOps.from(1,JANUARY,Year.of(2004)),
    DateOps.from(9,APRIL,Year.of(2004)),
    DateOps.from(12,APRIL,Year.of(2004)),

    DateOps.from(25,MARCH,Year.of(2005)),
    DateOps.from(28,MARCH,Year.of(2005)),
    DateOps.from(26,DECEMBER,Year.of(2005)),

    DateOps.from(14,APRIL,Year.of(2006)),
    DateOps.from(17,APRIL,Year.of(2006)),
    DateOps.from(1,MAY,Year.of(2006)),
    DateOps.from(25,DECEMBER,Year.of(2006)),
    DateOps.from(26,DECEMBER,Year.of(2006))
    )

    val holidays = BusinessCalendar.holidays(calendar,
      DateOps.from(1, JANUARY, Year.of(1999)),
      DateOps.from(31, DECEMBER, Year.of(2006))
    )

    assert(holidays.length == exptecteds.length,
      s"there were ${exptecteds.size} expected holidays, while there are ${holidays.size} calculated holidays")

    holidays zip exptecteds foreach { case (calculated, expected) =>
      assert(expected == calculated)
    }

  }


}
