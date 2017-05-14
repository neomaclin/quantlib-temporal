package org.quantlib.time.calendars

import java.time.Month._
import java.time.{LocalDate, Year}

import org.quantlib.time.Period
import org.quantlib.time.enums.TimeUnit
import org.quantlib.time.implicits.Date._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
import org.scalatest._
/**
  * Created by neo on 12/03/2017.
  */
class BusinessCalendarSpec extends FlatSpec with Matchers {

  "ModifiedCalendars Calendar" should "pass all test" in {
    val target = TARGET()
    val nyse = UnitedStates(UnitedStates.Market.NYSE)
    val d1 = DateOps.from(1,MAY,Year.of(2004))      // holiday for both calendars
    val d2 = DateOps.from(26,APRIL,Year.of(2004))   // business day

    assert(target.considerHoliday(d1), "wrong assumption---correct the test")
    assert(target.considerBusinessDay(d2), "wrong assumption---correct the test")

    assert(nyse.considerHoliday(d1), "wrong assumption---correct the test");
    assert(nyse.considerBusinessDay(d2), "wrong assumption---correct the test");

    var modifcation: BusinessCalendar[LocalDate] with Modification[LocalDate]  = AdHodCalendar(Some(target),"Target AdHoc", Nil)
    modifcation = modifcation.removeHoliday(d1)
    modifcation = modifcation.addHoliday(d2)

//  //changes only apply to the new calendar
    assert(modifcation.considerBusinessDay(d1), "wrong assumption---correct the test")
    assert(modifcation.considerHoliday(d2), "wrong assumption---correct the test")

    //original calendars are not effected
    assert(target.considerHoliday(d1), "wrong assumption---correct the test")
    assert(target.considerBusinessDay(d2), "wrong assumption---correct the test")
    assert(nyse.considerHoliday(d1), "wrong assumption---correct the test");
    assert(nyse.considerBusinessDay(d2), "wrong assumption---correct the test");

  }

  "JointCalendars Calendar" should "pass all test" in {

    val c1 = TARGET()
    val c2 = UnitedKingdom()
    val c3 = UnitedStates(UnitedStates.Market.NYSE)
    val c4 = Japan()

    val c12h = JointCalendar(List(c1,c2),JointCalendar.JointCalendarRule.JoinHolidays)
    val c12b = JointCalendar(List(c1,c2),JointCalendar.JointCalendarRule.JoinBusinessDays)
    val c123h = JointCalendar(List(c1,c2,c3),JointCalendar.JointCalendarRule.JoinHolidays)
    val c123b = JointCalendar(List(c1,c2,c3),JointCalendar.JointCalendarRule.JoinBusinessDays)
    val c1234h = JointCalendar(List(c1,c2,c3,c4),JointCalendar.JointCalendarRule.JoinHolidays)
    val c1234b = JointCalendar(List(c1,c2,c3,c4),JointCalendar.JointCalendarRule.JoinBusinessDays)

    val firstDate = DateOps.now
    val endDate = firstDate + Period(1, TimeUnit.Years)

    firstDate.toNumber to endDate.toNumber foreach { i =>
      val d = DateOps.fromNumber(i)
      val b1 = c1.considerBusinessDay(d)
      val b2 = c2.considerBusinessDay(d)
      val b3 = c3.considerBusinessDay(d)
      val b4 = c4.considerBusinessDay(d)

      assert((b1 && b2) == c12h.considerBusinessDay(d))
      assert((b1 || b2) == c12b.considerBusinessDay(d))
      assert((b1 && b2 && b3) == c123h.considerBusinessDay(d))
      assert((b1 || b2 || b3) == c123b.considerBusinessDay(d))
      assert((b1 && b2 && b3 && b4) == c1234h.considerBusinessDay(d))
      assert((b1 || b2 || b3 || b4) == c1234b.considerBusinessDay(d))

    }
  }


  "EndOfMonth Calendar" should "pass all test" in {

    val c = TARGET(); // any calendar would be OK

    val eom = DateOps.MIN
    val last = DateOps.MAX - Period(2,TimeUnit.Months)

    eom.toNumber to last.toNumber foreach { counter =>
      val d = DateOps.fromNumber(counter)
      val eom = c.endOfMonth(d)
      // check that eom is eom
      assert(c.isEndOfMonth(eom))
      // check that eom is in the same month as counter
      assert(eom.month == d.month)
    }
  }

}