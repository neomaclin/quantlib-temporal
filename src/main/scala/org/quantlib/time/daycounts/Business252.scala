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
        val length = BusinessCalendar.businessDaysBetween(d1, d2,calendar)
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
      // we treat the case of d1 > d2 here, since we'd need a second cache to get it right
      // (our cached figures are for first included, last excluded and might have to be changed going the other way.)
      BusinessCalendar.businessDaysBetween(d1, d2,calendar)
    } else if (d1.isSameYearOf(d2)) {
      // Cache& cache = monthlyFigures_[calendar_.name()];
      // Date::serial_type total = 0;
      // Date d;
      // first, we get to the beginning of next month.
      var endDate = from(1, d1.month, d1.year) + Period(1, Months)

      var total = BusinessCalendar.businessDaysBetween(d1, endDate,calendar)
      // then, we add any whole months (whose figures might be cached already) in the middle of our period.
      while (!endDate.isSameMonthOf(d2)) {
        total = total + businessDays(endDate.month, endDate.year)
        endDate = endDate + Period(1, Months)
      }
      // finally, we get to the end of the period.
      total = total + BusinessCalendar.businessDaysBetween(endDate, d2,calendar)
      total
    } else {
      // Cache& cache = monthlyFigures_[calendar_.name()];
      // OuterCache& outerCache = yearlyFigures_[calendar_.name()];
      // Date::serial_type total = 0;
      var endDate = from(1, d1.month, d1.year) + Period(1, Months)

      // first, we get to the beginning of next year.
      // The first bit gets us to the end of this month...
      //d = Date(1,d1.month(),d1.year()) + 1*Months;
      var total = BusinessCalendar.businessDaysBetween(d1, endDate,calendar)
      // ...then we add any remaining months, possibly cached
      d1.month.getValue to 12 foreach {
        m => total = total + businessDays(Month.of(m), endDate.year)
      }
      // then, we add any whole year in the middle of our period.
      endDate = from(1, Month.JANUARY, Year.of(d1.year.getValue + 1))
      while (!endDate.isSameYearOf(d2)) {
        total = total + businessDays(endDate.year)
        endDate = endDate + Period(1, Years)
      }
      // finally, we get to the end of the period.
      // First, we add whole months...
      1 until d2.month.getValue foreach {
        m =>
          total = total + businessDays(Month.of(m), d2.year)
      }
      // ...then the last bit.
      endDate = from(1, d2.month, d2.year)
      total = total + BusinessCalendar.businessDaysBetween(endDate, d2,calendar)
      total
    }
  }

  override def yearFraction(date1: D,
                            date2: D,
                            refDate1: Option[D] = None,
                            refDate2: Option[D] = None): Double = dayCount(date1, date2) / 252.0

}

