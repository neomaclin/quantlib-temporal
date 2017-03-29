package org.quantlib.time.daycounts


import java.time.{Month, Year}

import org.quantlib.time.Period
import org.quantlib.time.calendars.{BusinessCalendar, _}
import org.quantlib.time.enums.TimeUnit._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._


final case class Business252[D: DateOps](calendar: BusinessCalendar[D]) extends DayCountBasis[D] {

  override val toString = s"Business/252($calendar)"
  type OuterCache = Map[Year, Int]
  type Cache = Map[(Year, Month), Int]

  private var monthlyCache = Map.empty[(Year, Month), Int]
  private var yearlyCache = Map.empty[Year, Int]

  private def businessDays(month: Month, year: Year): Int = {
    monthlyCache.get((year, month)) match {
      case Some(size) => size
      case None => {
        val d1 = from(1, month, year)
        val d2 = d1 + Period(1, Months)
        val length = BusinessCalendar.businessDaysBetween(calendar, d1, d2)
        monthlyCache = monthlyCache + ((year, month) -> length)
        length
      }
    }
  }

  private def businessDays(year: Year): Int = {
    yearlyCache.get(year) match {
      case Some(size) => size
      case None => {
        val length = (1 to 12).map{m =>businessDays( Month.of(m), year) }.sum
        yearlyCache = yearlyCache + (year -> length)
        length
      }
    }
  }

  override def dayCount(d1: D, d2: D): Int = {
    if (d1.isSameMonthOf(d2) || d1 >= d2) {
      BusinessCalendar.businessDaysBetween(calendar, d1, d2)
    } else if (d1.isSameYearOf(d2)) {
      // first, we get to the beginning of next month.
      var endDate = from(1, d1.month, d1.year) + Period(1, Months)

      var total = BusinessCalendar.businessDaysBetween(calendar, d1, endDate)
      while (!endDate.isSameMonthOf(d2)) {
        total = total + businessDays(endDate.month, endDate.year)
        endDate = endDate + Period(1, Months)
      }
      total = total + BusinessCalendar.businessDaysBetween(calendar, endDate, d2)
      total
    } else {
      var endDate = from(1, d1.month, d1.year) + Period(1, Months)

      var total = BusinessCalendar.businessDaysBetween(calendar, d1, endDate)

      (d1.month.getValue + 1) to 12 foreach {
        m => total = total + businessDays(Month.of(m), endDate.year)
      }

      endDate = from(1, Month.JANUARY, Year.of(d1.year.getValue + 1))
      while (!endDate.isSameYearOf(d2)) {
        total = total + businessDays(endDate.year)
        endDate = endDate + Period(1, Years)
      }

      1 until d2.month.getValue foreach {
        m => total = total + businessDays(Month.of(m), d2.year)
      }
      // ...then the last bit.
      endDate = from(1, d2.month, d2.year)
      total = total + BusinessCalendar.businessDaysBetween(calendar, endDate, d2)
      total
    }
  }

  override def yearFraction(date1: D,
                            date2: D,
                            refDate1: Option[D] = None,
                            refDate2: Option[D] = None): Double = dayCount(date1, date2) / 252.0

}

