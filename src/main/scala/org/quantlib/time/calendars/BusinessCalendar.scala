package org.quantlib.time.calendars

import java.time.DayOfWeek._
import java.time.Month._
import java.time.Year

import org.quantlib.time.Period
import org.quantlib.time.enums.BusinessDayConvention._
import org.quantlib.time.enums.TimeUnit._
import org.quantlib.time.enums.{BusinessDayConvention, TimeUnit}
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

trait BusinessCalendar[D] {

  def considerBusinessDay(date: D): Boolean

  def isWeekend(date: D): Boolean

  def considerHoliday(date: D): Boolean = !considerBusinessDay(date)
}

abstract class WeekendSatSun[D: DateOps] {
  def isWeekend(date: D): Boolean = date.dow match {
    case SUNDAY | SATURDAY => true
    case _ => false
  }
}

abstract class WeekendThursFri[D: DateOps] {
  def isWeekend(date: D): Boolean = date.dow match {
    case THURSDAY | FRIDAY => true
    case _ => false
  }
}

abstract class WeekendFriSat[D: DateOps] {
  def isWeekend(date: D): Boolean = date.dow match {
    case FRIDAY | SATURDAY => true
    case _ => false
  }
}

trait Modification[D] {

  def addHoliday(date: D): BusinessCalendar[D] with Modification[D]

  def removeHoliday(date: D): BusinessCalendar[D] with Modification[D]

  def addWeekend(date: D): BusinessCalendar[D] with Modification[D]

  def removeWeekend(date: D): BusinessCalendar[D] with Modification[D]

}


object BusinessCalendar {

  def holidays[D: DateOps](from: D, to: D, inCalendar: BusinessCalendar[D],
                           includeWeekEnd: Boolean = false): List[D] = {
    def isHoliday(date: D) = inCalendar.considerHoliday(date) && (includeWeekEnd || !inCalendar.isWeekend(date))

    dayRanges(from, to, inCalendar, includeLast = true).filter(inCalendar.considerHoliday).toList
  }

  def businessDaysBetween[D: DateOps](from: D, to: D, inCalendar: BusinessCalendar[D],
                                      includeFirst: Boolean = true, includeLast: Boolean = false): Int = {
    dayRanges(from, to, inCalendar, includeFirst, includeLast).count(inCalendar.considerBusinessDay)
  }

  def isEndOfMonth[D: DateOps](date: D, inCalendar: BusinessCalendar[D]): Boolean = {
    date.month != adjust(date + Period(1, Days), inCalendar, Following ).month
  }

  def adjust[D: DateOps](date: D, inCalendar: BusinessCalendar[D],
                         convention: BusinessDayConvention = Following): D = {

    val nextBusinessDay: D = Stream.from(0).map(n => date + Period(n, Days)).find(inCalendar.considerBusinessDay).get
    val priorBusinessDay: D = Stream.from(0).map(n => date - Period(n, Days)).find(inCalendar.considerBusinessDay).get

    def intoNextMonth = nextBusinessDay.month != date.month

    def intoPrevMonth = priorBusinessDay.month != date.month

    def passedMidMonth = date.dom <= 15 && nextBusinessDay.dom > 15

    convention match {
      case Unadjusted => date
      case Following => nextBusinessDay
      case ModifiedFollowing => if (intoNextMonth) adjust(date, inCalendar, Preceding) else nextBusinessDay
      case HalfMonthModifiedFollowing => if (passedMidMonth || intoNextMonth) adjust(date, inCalendar, Preceding) else nextBusinessDay
      case Preceding => priorBusinessDay
      case ModifiedPreceding => if (intoPrevMonth) adjust(date, inCalendar, Following) else priorBusinessDay
    }
  }
  def endOfMonth[D: DateOps](date: D, inCalendar: BusinessCalendar[D]): D = ??? // adjust(date.withDayOfMonth(date.lengthOfMonth), Preceding)


  def advance[D: DateOps](date: D, period: Period, inCalendar: BusinessCalendar[D],
                          convention: BusinessDayConvention = Following, toEndOfMonth: Boolean = false): D = {
    val Period(n, unit) = period



    if (n == 0) {
      adjust(date, inCalendar, convention)
    } else {
      unit match {
        case TimeUnit.Days => adjust(date + Period(n, Days), inCalendar, convention)
        case TimeUnit.Weeks => adjust(date + Period(n, Weeks), inCalendar, convention)
        case TimeUnit.Months | TimeUnit.Years =>
          val advancedDate = if (unit == TimeUnit.Months) date + Period(n, Months) else date + Period(n, Years)
          if (toEndOfMonth && isEndOfMonth(date, inCalendar)) endOfMonth(advancedDate, inCalendar) else adjust(advancedDate, inCalendar, convention)
      }
    }
  }


  def dayRanges[D: DateOps](from: D, to: D, inCalendar: BusinessCalendar[D],
                            includeFirst: Boolean = true, includeLast: Boolean = false) = {
    val isPositiveFlow = from < to

    def adjustment(date: D, offset: Int) = if (isPositiveFlow) date + Period(offset, Days) else date - Period(offset, Days)

    val startDate = if (includeFirst) from else adjustment(from, 1)
    val endDate = if (includeLast) to else adjustment(to, -1)

    //val range = (0 to Days.daysBetween(startDate, endDate).getDays)
    //def inRange(date: D) = if (isPositiveFlow) date <= endDate else date >= endDate

    Stream.from(0, startDate.to(endDate, Days).toInt).map(adjustment(startDate, _))
  }

  object Western {
    private val EasterMonday = Vector(
      98, 90, 103, 95, 114, 106, 91, 111, 102, // 1901-1909
      87, 107, 99, 83, 103, 95, 115, 99, 91, 111, // 1910-1919
      96, 87, 107, 92, 112, 103, 95, 108, 100, 91, // 1920-1929
      111, 96, 88, 107, 92, 112, 104, 88, 108, 100, // 1930-1939
      85, 104, 96, 116, 101, 92, 112, 97, 89, 108, // 1940-1949
      100, 85, 105, 96, 109, 101, 93, 112, 97, 89, // 1950-1959
      109, 93, 113, 105, 90, 109, 101, 86, 106, 97, // 1960-1969
      89, 102, 94, 113, 105, 90, 110, 101, 86, 106, // 1970-1979
      98, 110, 102, 94, 114, 98, 90, 110, 95, 86, // 1980-1989
      106, 91, 111, 102, 94, 107, 99, 90, 103, 95, // 1990-1999
      115, 106, 91, 111, 103, 87, 107, 99, 84, 103, // 2000-2009
      95, 115, 100, 91, 111, 96, 88, 107, 92, 112, // 2010-2019
      104, 95, 108, 100, 92, 111, 96, 88, 108, 92, // 2020-2029
      112, 104, 89, 108, 100, 85, 105, 96, 116, 101, // 2030-2039
      93, 112, 97, 89, 109, 100, 85, 105, 97, 109, // 2040-2049
      101, 93, 113, 97, 89, 109, 94, 113, 105, 90, // 2050-2059
      110, 101, 86, 106, 98, 89, 102, 94, 114, 105, // 2060-2069
      90, 110, 102, 86, 106, 98, 111, 102, 94, 114, // 2070-2079
      99, 90, 110, 95, 87, 106, 91, 111, 103, 94, // 2080-2089
      107, 99, 91, 103, 95, 115, 107, 91, 111, 103, // 2090-2099
      88, 108, 100, 85, 105, 96, 109, 101, 93, 112, // 2100-2109
      97, 89, 109, 93, 113, 105, 90, 109, 101, 86, // 2110-2119
      106, 97, 89, 102, 94, 113, 105, 90, 110, 101, // 2120-2129
      86, 106, 98, 110, 102, 94, 114, 98, 90, 110, // 2130-2139
      95, 86, 106, 91, 111, 102, 94, 107, 99, 90, // 2140-2149
      103, 95, 115, 106, 91, 111, 103, 87, 107, 99, // 2150-2159
      84, 103, 95, 115, 100, 91, 111, 96, 88, 107, // 2160-2169
      92, 112, 104, 95, 108, 100, 92, 111, 96, 88, // 2170-2179
      108, 92, 112, 104, 89, 108, 100, 85, 105, 96, // 2180-2189
      116, 101, 93, 112, 97, 89, 109, 100, 85, 105 // 2190-2199
    )

    def easterMonday(year: Year): Int = EasterMonday(year.getValue - 1901)

    def isGoodFriday[D: DateOps](date: D): Boolean = date.doy == easterMonday(date.year) - 3

    def isEasterMonday[D: DateOps](date: D): Boolean = date.doy == easterMonday(date.year)

    def isAscension[D: DateOps](date: D): Boolean = date.doy == easterMonday(date.year) + 38

    def isWhitMonday[D: DateOps](date: D): Boolean = date.doy == easterMonday(date.year) + 49

    def isCorpusChristi[D: DateOps](date: D): Boolean = date.doy == easterMonday(date.year) + 59


  }

  object InternationalHolidays {

    def inDecember[D: DateOps](date: D): Boolean = date.month == DECEMBER

    def inJanuary[D: DateOps](date: D): Boolean = date.month == JANUARY

    def inFebruary[D: DateOps](date: D): Boolean = date.month == FEBRUARY

    def inJune[D: DateOps](date: D): Boolean = date.month == JUNE

    def inJuly[D: DateOps](date: D): Boolean = date.month == JULY

    def inApril[D: DateOps](date: D): Boolean = date.month == APRIL

    def inAugust[D: DateOps](date: D): Boolean = date.month == AUGUST

    def inSeptember[D: DateOps](date: D): Boolean = date.month == SEPTEMBER

    def inOctober[D: DateOps](date: D): Boolean = date.month == OCTOBER

    def inNovember[D: DateOps](date: D): Boolean = date.month == NOVEMBER

    def inMay[D: DateOps](date: D): Boolean = date.month == MAY

    def isNewYearEve[D: DateOps](date: D): Boolean = date.dom == 31 && inDecember(date)

    def isNewYear[D: DateOps](date: D): Boolean = date.dom == 1 && inJanuary(date)

    def isNewYearOnMonday[D: DateOps](date: D): Boolean = date.dom == 2 && isMonday(date) && inJanuary(date)

    def isMonday[D: DateOps](date: D): Boolean = date.dow == MONDAY

    def isTuesDay[D: DateOps](date: D): Boolean = date.dow == TUESDAY

    def isFriday[D: DateOps](date: D): Boolean = date.dow == FRIDAY

    def isFirstMonday[D: DateOps](date: D): Boolean = date.dom <= 7 && isMonday(date)

    def isSecondMonday[D: DateOps](date: D): Boolean = {
      val dom = date.dom
      (dom > 7 && dom <= 14) && isMonday(date)
    }

    def isChristmasMT[D: DateOps](date: D): Boolean = {
      val dom = date.dom
      (dom == 25 || dom == 27 && (isMonday(date) || isTuesDay(date))) && inDecember(date)
    }

    def isBoxingDayMT[D: DateOps](date: D): Boolean = {
      val dom = date.dom
      (dom == 26 || dom == 28 && (isMonday(date) || isTuesDay(date))) && inDecember(date)
    }

    def isChristmasEve[D: DateOps](date: D): Boolean = {
      date.dom == 24 && inDecember(date)
    }

    def isChristmas[D: DateOps](date: D): Boolean = {
      date.dom == 25 && inDecember(date)
    }

    def isBoxingDay[D: DateOps](date: D): Boolean = {
      date.dom == 26 && inDecember(date)
    }



  }

  object Orthodox {
    private val EasterMonday = Array(
      105, 118, 110, 102, 121, 106, 126, 118, 102, // 1901-1909
      122, 114, 99, 118, 110, 95, 115, 106, 126, 111, // 1910-1919
      103, 122, 107, 99, 119, 110, 123, 115, 107, 126, // 1920-1929
      111, 103, 123, 107, 99, 119, 104, 123, 115, 100, // 1930-1939
      120, 111, 96, 116, 108, 127, 112, 104, 124, 115, // 1940-1949
      100, 120, 112, 96, 116, 108, 128, 112, 104, 124, // 1950-1959
      109, 100, 120, 105, 125, 116, 101, 121, 113, 104, // 1960-1969
      117, 109, 101, 120, 105, 125, 117, 101, 121, 113, // 1970-1979
      98, 117, 109, 129, 114, 105, 125, 110, 102, 121, // 1980-1989
      106, 98, 118, 109, 122, 114, 106, 118, 110, 102, // 1990-1999
      122, 106, 126, 118, 103, 122, 114, 99, 119, 110, // 2000-2009
      95, 115, 107, 126, 111, 103, 123, 107, 99, 119, // 2010-2019
      111, 123, 115, 107, 127, 111, 103, 123, 108, 99, // 2020-2029
      119, 104, 124, 115, 100, 120, 112, 96, 116, 108, // 2030-2039
      128, 112, 104, 124, 116, 100, 120, 112, 97, 116, // 2040-2049
      108, 128, 113, 104, 124, 109, 101, 120, 105, 125, // 2050-2059
      117, 101, 121, 113, 105, 117, 109, 101, 121, 105, // 2060-2069
      125, 110, 102, 121, 113, 98, 118, 109, 129, 114, // 2070-2079
      106, 125, 110, 102, 122, 106, 98, 118, 110, 122, // 2080-2089
      114, 99, 119, 110, 102, 115, 107, 126, 118, 103, // 2090-2099
      123, 115, 100, 120, 112, 96, 116, 108, 128, 112, // 2100-2109
      104, 124, 109, 100, 120, 105, 125, 116, 108, 121, // 2110-2119
      113, 104, 124, 109, 101, 120, 105, 125, 117, 101, // 2120-2129
      121, 113, 98, 117, 109, 129, 114, 105, 125, 110, // 2130-2139
      102, 121, 113, 98, 118, 109, 129, 114, 106, 125, // 2140-2149
      110, 102, 122, 106, 126, 118, 103, 122, 114, 99, // 2150-2159
      119, 110, 102, 115, 107, 126, 111, 103, 123, 114, // 2160-2169
      99, 119, 111, 130, 115, 107, 127, 111, 103, 123, // 2170-2179
      108, 99, 119, 104, 124, 115, 100, 120, 112, 103, // 2180-2189
      116, 108, 128, 119, 104, 124, 116, 100, 120, 112 // 2190-2199
    )

    def easterMonday(year: Year): Int = EasterMonday(year.getValue - 1901)
  }

}