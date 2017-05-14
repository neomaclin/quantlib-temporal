package org.quantlib.time


import java.time.Year

import java.time.Month._

import org.quantlib.time.calendars._
import org.quantlib.time.enums.BusinessDayConvention._
import org.quantlib.time.enums.Frequency.Daily
import org.quantlib.time.enums.{DateGenerationRule, TimeUnit}
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
import org.scalatest.{FlatSpec, Matchers}


/**
  * Created by neo on 12/03/2017.
  */
class ScheduleSpec extends FlatSpec with Matchers {

  def checkDates[D: DateOps](s: Schedule[D], expected: List[D]): Unit ={
    assert(s.dates.length == expected.length, s" expected ${expected.length} dates, but found ${s.dates.length}")
    s.dates zip expected foreach { case (generated, expectation) =>
      assert(generated == expectation,s"expected $expectation but found $generated")
    }
  }

  "Schedule Generation" should " be able to generate daily Schedule" in {
    import org.quantlib.time.implicits.Date._

    val startDate = DateOps.from(17, JANUARY, Year.of(2012))

    val expected = List(
      DateOps.from(17,JANUARY,Year.of(2012)),
      DateOps.from(18,JANUARY,Year.of(2012)),
      DateOps.from(19,JANUARY,Year.of(2012)),
      DateOps.from(20,JANUARY,Year.of(2012)),
      DateOps.from(23,JANUARY,Year.of(2012)),
      DateOps.from(24,JANUARY,Year.of(2012))
    )

    val s =
      Schedule.from(effectiveDate = startDate,
        terminationDate = startDate + 7,
        calendar = TARGET(),
        tenor = Period.from(Daily),
        convention = Preceding)

    checkDates(s, expected)
  }

  "Schedule Generation" should " be able to generate end date for schedule with end-of-month adjustment" in {
    import org.quantlib.time.implicits.Date._

    val startDate = DateOps.from(30, SEPTEMBER, Year.of(2009))
    val endDate = DateOps.from(15, JUNE, Year.of(2012))

    val expected = List(
      DateOps.from(30,SEPTEMBER,Year.of(2009)),
      DateOps.from(31,MARCH,Year.of(2010)),
      DateOps.from(30,SEPTEMBER,Year.of(2010)),
      DateOps.from(31,MARCH,Year.of(2011)),
      DateOps.from(30,SEPTEMBER,Year.of(2011)),
      DateOps.from(30,MARCH,Year.of(2012)),
      DateOps.from(29,JUNE,Year.of(2012))
    )

    val s =
      Schedule.from(effectiveDate = startDate,
        terminationDate = endDate,
        calendar = Japan(),
        tenor = Period(6, TimeUnit.Months),
        convention = Following,
        terminationDateConvention = Some(Following),
        rule = DateGenerationRule.Forward,
        endOfMonth = true)

    checkDates(s, expected)
  }

  "Schedule Generation" should " be able to schedule with end-of-month adjustment" in {
    import org.quantlib.time.implicits.Date._

    val startDate = DateOps.from(28, MARCH, Year.of(2013))
    val endDate = DateOps.from(30, MARCH, Year.of(2015))

    val expected = List(
      DateOps.from(31,MARCH,Year.of(2013)),
      DateOps.from(31,MARCH,Year.of(2014)),
      DateOps.from(30,MARCH,Year.of(2015))
    )

    val s =
      Schedule.from(effectiveDate = startDate,
        terminationDate = endDate,
        calendar = TARGET(),
        tenor = Period(1, TimeUnit.Years),
        convention = Unadjusted,
        rule = DateGenerationRule.Forward,
        endOfMonth = true)

    checkDates(s, expected)
    assert(!s.isRegular(1),"last period should not be regular")
  }

  "Schedule Generation" should " be able to remove next-to-last date when it is same as end date ." in {
    import org.quantlib.time.implicits.Date._

    val startDate = DateOps.from(28, MARCH, Year.of(2013))
    val endDate = DateOps.from(31, MARCH, Year.of(2015))

    val expected = List(
      DateOps.from(31,MARCH,Year.of(2013)),
      DateOps.from(31,MARCH,Year.of(2014)),
      DateOps.from(31,MARCH,Year.of(2015))
    )

    val s =
      Schedule.from(effectiveDate = startDate,
        terminationDate = endDate,
        calendar = TARGET(),
        tenor = Period(1, TimeUnit.Years),
        convention = Unadjusted,
        terminationDateConvention = Some(Unadjusted),
        rule = DateGenerationRule.Forward,
        endOfMonth = true)

    checkDates(s, expected)
    assert(s.isRegular(1),"last period should be regular")
  }

  "Schedule Generation" should " be able to unadjust last date is for EOM " +
                                 " when termination date convention is unadjusted..." in {
    import org.quantlib.time.implicits.Date._

    val startDate = DateOps.from(31, AUGUST, Year.of(1996))
    val endDate = DateOps.from(15, SEPTEMBER, Year.of(1997))

    val expected = List(
      DateOps.from(31,AUGUST,Year.of(1996)),
      DateOps.from(28,FEBRUARY,Year.of(1997)),
      DateOps.from(31,AUGUST,Year.of(1997)),
      DateOps.from(15,SEPTEMBER,Year.of(1997))
    )

    val s =
      Schedule.from(effectiveDate = startDate,
        terminationDate = endDate,
        calendar = UnitedStates(UnitedStates.Market.GovernmentBond),
        tenor = Period(6, TimeUnit.Months),
        convention = Unadjusted,
        terminationDateConvention = Some(Unadjusted),
        rule = DateGenerationRule.Forward,
        endOfMonth = true)

    checkDates(s, expected)
  }

  "Schedule Generation" should " be able to unadjust first date is for EOM " +
                                " going backwards when termination date convention is unadjusted..." in {
    import org.quantlib.time.implicits.Date._

    val startDate = DateOps.from(22, AUGUST, Year.of(1996))
    val endDate = DateOps.from(31, AUGUST, Year.of(1997))

    val expected = List(
      DateOps.from(22,AUGUST,Year.of(1996)),
      DateOps.from(31,AUGUST,Year.of(1996)),
      DateOps.from(28,FEBRUARY,Year.of(1997)),
      DateOps.from(31,AUGUST,Year.of(1997))
    )

    val s =
      Schedule.from(effectiveDate = startDate,
        terminationDate = endDate,
        calendar = UnitedStates(UnitedStates.Market.GovernmentBond),
        tenor = Period(6, TimeUnit.Months),
        convention = Unadjusted,
        terminationDateConvention = Some(Unadjusted),
        rule = DateGenerationRule.Backward,
        endOfMonth = true)

    checkDates(s, expected)
  }

  "Schedule Generation" should " be able not duplicate first date for EOM when going backwards.." in {
    import org.quantlib.time.implicits.Date._

    val startDate = DateOps.from(22, AUGUST, Year.of(1996))
    val endDate = DateOps.from(31, AUGUST, Year.of(1997))

    val expected = List(
      DateOps.from(30,AUGUST,Year.of(1996)),
      DateOps.from(28,FEBRUARY,Year.of(1997)),
      DateOps.from(29,AUGUST,Year.of(1997))
    )

    val s =
      Schedule.from(effectiveDate = startDate,
        terminationDate = endDate,
        calendar = UnitedStates(UnitedStates.Market.GovernmentBond),
        tenor = Period(6, TimeUnit.Months),
        convention = Following,
        terminationDateConvention = Some(Following),
        rule = DateGenerationRule.Backward,
        endOfMonth = true)

    checkDates(s, expected)
  }

}
