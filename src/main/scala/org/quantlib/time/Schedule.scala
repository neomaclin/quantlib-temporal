package org.quantlib.time

import java.time.LocalDate

import org.quantlib.time.calendars.BusinessCalendar
import org.quantlib.time.enums.BusinessDayConvention._
import org.quantlib.time.enums.DateGenerationRule._
import org.quantlib.time.enums.TimeUnit._
import org.quantlib.time.enums._

/**
  * Created by neo on 12/14/15.
  */

final case class Schedule(dates: List[LocalDate],
                          calendar: BusinessCalendar[LocalDate],
                          convention: BusinessDayConvention = Unadjusted,
                          terminationDateConvention: BusinessDayConvention,
                          tenor: Period,
                          rule: DateGenerationRule,
                          endOfMonth: Boolean) {


}

object Schedule {

  def from(effectiveDate: LocalDate,
           terminationDate: LocalDate,
           tenor: Period,
           calendar: BusinessCalendar[LocalDate],
           convention: BusinessDayConvention,
           terminationDateConvention: BusinessDayConvention,
           rule: DateGenerationRule,
           endOfMonth: Boolean,
           firstDate: Option[LocalDate],
           nextToLastDate: Option[LocalDate]
          ): Schedule = ???





  def nextTwentieth(date: LocalDate, rule: DateGenerationRule): LocalDate = ???
//  {
//    val temp = LocalDate.of(date.getYear, date.getMonthValue, 20)
//    val next = if (temp < date) temp + Period(1, Months) else temp
//
//    if (rule == TwentiethIMM || rule == OldCDS || rule == CDS) {
//      val month = next.getMonthValue
//      if (month % 3 != 0) next + Period(3 - month % 3, Months) // not a main IMM nmonth
//      else next
//    } else {
//      next
//    }
//  }

  def previousTwentieth(date: LocalDate, rule: DateGenerationRule): LocalDate = ???
//  {
//    val temp = LocalDate.of(date.getYear, date.getMonthValue, 20)
//    val prev = if (temp > date) temp - Period(1, Months) else temp
//
//    if (rule == TwentiethIMM || rule == OldCDS || rule == CDS) {
//      val month = prev.getMonthValue
//      if (month % 3 != 0) prev - Period(month % 3, Months) // not a main IMM nmonth
//      else prev
//    } else {
//      prev
//    }
//  }

  def allowsEndOfMonth(tenor: Period): Boolean = ???
  //  {
   // (tenor.unit == Months || tenor.unit == Years) && tenor >= Period(1, Months)
 // }

}
