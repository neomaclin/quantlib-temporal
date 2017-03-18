package org.quantlib.time

import org.quantlib.time.calendars.BusinessCalendar
import org.quantlib.time.enums.DateGenerationRule._
import org.quantlib.time.enums.TimeUnit._
import org.quantlib.time.enums._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 12/14/15.
  */

final case class Schedule[D: DateOps](dates: List[D],
                                      tenor: Period,
                                      calendar: BusinessCalendar[D],
                                      convention: BusinessDayConvention,
                                      terminationDateConvention: BusinessDayConvention,
                                      rule: DateGenerationRule,
                                      endOfMonth: Boolean,
                                      isRegular: List[Boolean],
                                      holidays: List[D]) {
  require(Schedule.allowsEndOfMonth(tenor) && !endOfMonth,
    "endOfMonth convention incompatible with tenor")
  require(isRegular.nonEmpty && isRegular.size == dates.size - 1,
    s"isRegular size($isRegular.size) must be zero or equal to the number of dates minus 1 ")
  require(List(Twentieth, TwentiethIMM, ThirdWednesday, OldCDS, CDS).contains(rule) && endOfMonth,
    "endOfMonth convention incompatible with date generation rule($rule)")
  require(tenor.length > 0, s"non positive tenor ($tenor) not allowed")
}

object Schedule {

  def from[D: DateOps](effectiveDate: D,
                       terminationDate: D,
                       tenor: Period,
                       calendar: BusinessCalendar[D],
                       convention: BusinessDayConvention,
                       terminationDateConvention: BusinessDayConvention,
                       rule: DateGenerationRule,
                       endOfMonth: Boolean,
                       firstDate: Option[D],
                       nextToLastDate: Option[D]
                      ): Schedule[D] = ???

  //  {
  //    require( List(Twentieth, TwentiethIMM, ThirdWednesday, OldCDS, CDS).contains(rule) && endOfMonth,
  //      "endOfMonth convention incompatible with date generation rule($rule)")
  //    require(tenor.length > 0, s"non positive tenor ($tenor) not allowed")
  //    require(tenor.length == 0 && rule != DateGenerationRule.Zero, s"tenor incompatible with  date generation rule $rule")
  //    val IMM = IMM()
  //   // if(DateGenerationRule.Backward == rule) require(setting.evalDate < terminationDate) else require(effectiveDate < terminationDate )
  //
  //    firstDate foreach {
  //      rule match{
  //        case Backward | Forward =>
  //          require(firstDate > effectiveDate && first < terminationDate, s"first date $first is out of range of [$effectiveDate, $terminationDate]"
  //        case ThirdWednesday =>
  //          require(IMM.confirm(first, false), "first date ($date) is not an IMM date")
  //        case Zero | Twentieth | TwentiethIMM | OldCDS | CDS =>
  //          require(false, "first date ($date) incompatible with date generation rule $rule")
  //      }
  //    }
  //
  //    nextToLast foreach {
  //      rule match{
  //        case Backward | Forward =>
  //          require(nextToLast > effectiveDate && nextToLast < terminationDate, s"first date $date is out of range of [$effectiveDate, $terminationDate]"
  //        case ThirdWednesday =>
  //          require(IMM.confirm(nextToLast, false), "next to Last date ($nextToLast) is not an IMM date")
  //        case Zero | Twentieth | TwentiethIMM | OldCDS | CDS =>
  //          require(false, "first date/next to last ($date) incompatible with date generation rule $rule")
  //      }
  //    }
  //
  //
  //    (tenor, generatedDates, regulars)= rule match {
  //      case Zero => (Period(0,Years), List(effectiveDate, terminationDate) , List(true))
  //      case Backward =>
  //        var dates = List(terminationDate)
  //        var isregulars = Nil
  //
  //        while() {
  //          val temp = BusinessCalendar.advance(seed, calendar, tenor, convention, endOfMonth, );
  //          dates = temp :: dates
  //        }
  //        val dates = build()
  //        (tenor, dates, regulars)
  //      case Twentieth | TwentiethIMM | ThirdWednesday | OldCDS | CDS => require(!endOfMonth, s"endOfMonth convention incompatible with date generation rule($rule)")
  //      case Forward =>
  //        var dates = List(terminationDate)
  //
  //        while (!=effectiveDate) {
  //          val temp = calendar.advance(seed, tenor, convention, endOfMonth);
  //          dates = temp :: dates
  //        }
  //        (tenor, dates.reverse, regulars.reverse)
  //    }
  //
  //    val actualConvention = convention getOrElse( if (cal.nonEmpty) Following else Unadjusted )
  //    val actualTermination = terminationDateConvention getOrElse actualConvention
  //    val holidays = BusinessCalendar.holidays(effectiveDate,terminationDate, calendar)
  //    Schedule(generatedDates,tenor,calendar,actualConvention,actualTermination,rule,endOfMonth,regulars, holidays)
  //  }


  def nextTwentieth[D: DateOps](date: D, rule: DateGenerationRule): D = {
    val temp = DateOps.from(20, date.month, date.year)
    val next = if (temp < date) temp + Period(1, Months) else temp

    if (rule == TwentiethIMM || rule == OldCDS || rule == CDS) {
      val month = next.month.getValue
      // not a main IMM nmonth
      if (month % 3 != 0) next + Period(3 - month % 3, Months) else next
    } else {
      next
    }
  }

  def previousTwentieth[D: DateOps](date: D, rule: DateGenerationRule): D = {
    val temp = DateOps.from(20, date.month, date.year)
    val prev = if (temp > date) temp - Period(1, Months) else temp

    if (rule == TwentiethIMM || rule == OldCDS || rule == CDS) {
      val month = prev.month.getValue
      // not a main IMM nmonth
      if (month % 3 != 0) prev - Period(month % 3, Months) else prev
    } else {
      prev
    }
  }

  def allowsEndOfMonth(tenor: Period): Boolean = {
    (tenor.unit == Months || tenor.unit == Years) && tenor >= Period(1, Months)
  }

}
