package org.quantlib.time


import org.quantlib.time.calendars.BusinessCalendar
import org.quantlib.time.enums.BusinessDayConvention._
import org.quantlib.time.enums.DateGenerationRule._
import org.quantlib.time.enums.TimeUnit._
import org.quantlib.time.enums._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
import org.quantlib.time.standards.IMM

import scala.annotation.tailrec


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

  require(isRegular.nonEmpty && isRegular.size == dates.size - 1,
    s"isRegular size($isRegular.size) must be zero or equal to the number of dates minus 1 ")

  require(tenor.length > 0, s"non positive tenor ($tenor) not allowed")

  if (List(Twentieth, TwentiethIMM, ThirdWednesday, OldCDS, CDS).contains(rule)) {
    require(!endOfMonth, s"endOfMonth convention incompatible with date generation rule($rule)")
  }
  if (Schedule.allowsEndOfMonth(tenor)) {
    (require(endOfMonth), s"tenor allow for End Of Month, but provided $endOfMonth")
  }
  if (tenor.length == 0) {
    require(rule == DateGenerationRule.Zero, s"tenor incompatible with  date generation rule $rule")
  }
}

object Schedule {
  def from[D: DateOps](effectiveDate: D,
                       terminationDate: D,
                       tenor: Period,
                       calendar: BusinessCalendar[D],
                       convention: BusinessDayConvention = Unadjusted,
                       terminationDateConvention: Option[BusinessDayConvention] = None,
                       rule: DateGenerationRule = Backward,
                       endOfMonth: Boolean = false,
                       firstDate: Option[D] = None,
                       nextToLastDate: Option[D] = None
                      ): Schedule[D] = {

    require(tenor.length > 0, s"non positive tenor ($tenor) not allowed")

    if (List(Twentieth, TwentiethIMM, ThirdWednesday, OldCDS, CDS).contains(rule)) {
      require(!endOfMonth, s"endOfMonth convention incompatible with date generation rule($rule)")
    }
    if (allowsEndOfMonth(tenor)) {
      (require(endOfMonth), s"tenor allow for End Of Month, but provided $endOfMonth")
    }
    if (tenor.length == 0) {
      require(rule == DateGenerationRule.Zero, s"tenor incompatible with  date generation rule $rule")
    }

    firstDate foreach { first =>
      rule match {
        case Backward | Forward =>
          require(first > effectiveDate && first < terminationDate,
            s"first date $first is out of range of [$effectiveDate, $terminationDate]")
        case ThirdWednesday =>
          require(IMM.confirm(first, false), "first date ($date) is not an IMM date")
        case Zero | Twentieth | TwentiethIMM | OldCDS | CDS | CDS2015 =>
          require(false, "first date ($date) incompatible with date generation rule $rule")
      }
    }

    nextToLastDate foreach { nextToLast =>
      rule match {
        case Backward | Forward =>
          require(nextToLast > effectiveDate && nextToLast < terminationDate,
            s"next to Last date date $nextToLast is out of range of [$effectiveDate, $terminationDate]")
        case ThirdWednesday =>
          require(IMM.confirm(nextToLast, false), s"next to Last date ($nextToLast) is not an IMM date")
        case Zero | Twentieth | TwentiethIMM | OldCDS | CDS | CDS2015 =>
          require(false, s"next to last ($nextToLast) incompatible with date generation rule $rule")
      }
    }


    def isRegular(date1: D, date2: D, direction: Int): Boolean = {
      date2 == BusinessCalendar.advance(calendar, date1, tenor * direction, convention, endOfMonth)
    }

    val actualTermination = terminationDateConvention getOrElse convention
    val seed = if (rule == Backward) nextToLastDate getOrElse terminationDate else firstDate getOrElse effectiveDate
    val exit = if (rule == Backward) firstDate getOrElse effectiveDate else nextToLastDate getOrElse terminationDate


    val (generatedDates, regulars) = rule match {
      case Zero =>
        (List(effectiveDate, terminationDate), List(true))
      case Backward =>
        val datesInit = List(terminationDate)
        val regularsInit = nextToLastDate.map(nextToLast => isRegular(terminationDate, nextToLast, -1)).toList

        @tailrec
        def acc(n: Int, dates: List[D], regulars: List[Boolean]): (List[D], List[Boolean]) = {
          val temp = calendar.advance(seed, tenor * -n, convention, endOfMonth)
          if (temp < exit) {
            firstDate.filter { first =>
              calendar.adjust(dates.head, convention) != calendar.adjust(first, convention)
            }.map { first =>
              (first :: dates, false :: regulars)
            }.getOrElse {
              (dates, regulars)
            }
          } else {
            if (calendar.adjust(dates.head, convention) != calendar.adjust(temp, convention)) {
              acc(n + 1, temp :: dates, true :: regulars)
            } else {
              acc(n + 1, dates, regulars)
            }

          }
        }

        val (datesBeforeEffective, regularsBeforeEffective) = acc(1, nextToLastDate.toList ++ datesInit, regularsInit)

        if (calendar.adjust(datesBeforeEffective.head, convention) != calendar.adjust(effectiveDate, convention)) {
          (effectiveDate :: datesBeforeEffective, false :: regularsBeforeEffective)
        } else {
          (datesBeforeEffective, regularsBeforeEffective)
        }

      case _ =>
        val datesInit = List(effectiveDate)
        val regularsInit = firstDate.map(first => isRegular(effectiveDate, first, 1)).toList

        @tailrec
        def acc(n: Int, dates: List[D], regulars: List[Boolean]): (List[D], List[Boolean]) = {
          val temp = calendar.advance(seed, tenor * n, convention, endOfMonth)
          if (temp > exit) {
            nextToLastDate.filter { nextToLast =>
              calendar.adjust(dates.head, convention) != calendar.adjust(nextToLast, convention)
            }.map { nextToLast =>
              (nextToLast :: dates, false :: regulars)
            }.getOrElse {
              (dates, regulars)
            }
          } else {
            if (calendar.adjust(dates.head, convention) != calendar.adjust(temp, convention)) {
              acc(n + 1, temp :: dates, true :: regulars)
            } else {
              acc(n + 1, dates, false :: regulars)
            }

          }
        }

        val (datesBeforeTerminationDate, regularsBeforeTerminationDate) = acc(1, firstDate.toList ++ datesInit, regularsInit)
        if (calendar.adjust(datesBeforeTerminationDate.head, convention) != calendar.adjust(terminationDate, convention)) {
          ((terminationDate :: datesBeforeTerminationDate).reverse, (false :: regularsBeforeTerminationDate).reverse)
        } else {
          (datesBeforeTerminationDate.reverse, regularsBeforeTerminationDate.reverse)
        }


    }

    var adjustedDates = if (endOfMonth && calendar.isEndOfMonth(seed)) {
      // adjust to end of month
      val adjustedRange = if (convention == Unadjusted) {
        generatedDates.tail.init.map(_.endOfMonth)
      } else {
        generatedDates.tail.init.map(calendar.endOfMonth)
      }
      val (adjustedHead, adjustedLast) = if (actualTermination == Unadjusted) {
        if (rule == Backward) {
          (generatedDates.head, generatedDates.last.endOfMonth)
        } else {
          (generatedDates.head.endOfMonth, generatedDates.last)
        }
      } else {

        (calendar.endOfMonth(generatedDates.head), calendar.endOfMonth(generatedDates.last))
      }

      adjustedHead +: adjustedRange :+ adjustedLast
    } else {
      generatedDates
    }

    var adjustedRegulars = regulars

      if (adjustedDates.length >= 2 && adjustedDates.init.last >= adjustedDates.last) {
        adjustedDates = adjustedDates.init.init :+ adjustedDates.last
        adjustedRegulars = adjustedRegulars.init
      }

      if (adjustedDates.length >= 2 && adjustedDates.tail.head <= adjustedDates.head) {
        adjustedDates = adjustedDates.head +: adjustedDates.tail.tail
        adjustedRegulars = adjustedRegulars.tail
      }

    val holidays = calendar.holidays(effectiveDate, terminationDate)
    Schedule(adjustedDates, tenor, calendar, convention, actualTermination, rule, endOfMonth, adjustedRegulars, holidays)
  }


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
