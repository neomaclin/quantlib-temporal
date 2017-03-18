package org.quantlib.time.calendars

import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */

object Japan {
  // equinox calculation
  private val exact_vernal_equinox_time: Double = 20.69115
  private val exact_autumnal_equinox_time: Double = 23.09
  private val diff_per_year: Double = 0.242194


}

final case class Japan[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D] {

  import Japan._

  private def movingAmount(y: Int): Double = (y - 2000) * diff_per_year

  private def numberOfLeapYears(y: Int): Int = (y - 2000) / 4 + (y - 2000) / 100 - (y - 2000) / 400

  private def adjustments(y: Int): Double = movingAmount(y) - numberOfLeapYears(y)

  import java.time.Month._
  import java.time.DayOfWeek._

  private def isComingOfAgeDay(date: D) = {
    val dow = date.dow
    val (y, m, dom) = date.YMD
    (dow.isMonday && (dom >= 8 && dom <= 14) && m == JANUARY && y >= 2000) ||
      ((dom == 15 || (dom == 16 && dow.isMonday)) && m == JANUARY && y < 2000)

  }

  private def isNationalFoundationDay(date: D) = {
    val d = date.dom
    (d == 11 || (d == 12 && date.dow.isMonday)) && date.inFebruary
  }

  private def isGreeneryDay(date: D) = {
    val d = date.dom
    (d == 29 || (d == 30 && date.dow.isMonday)) && date.inApril

  }

  private def isBankHoliday(date: D) = {
    val dom = date.dom
    ((dom == 2 || dom == 3) && date.inJanuary) || (dom == 31 && date.inDecember)
  }

  private def isDaysInMay(date: D) = {
    val d = date.dom
    val m = date.month
    val w = date.dow
    // Constitution Memorial Day
    (d == 3 && m == MAY) ||
      // Holiday for a NATIon
      (d == 4 && m == MAY) ||
      // Children's Day
      (d == 5 && m == MAY) ||
      // any of the thrEE Above observed later if on Saturday or Sunday
      (d == 6 && m == MAY && (w == MONDAY || w == TUESDAY || w == WEDNESDAY))
  }

  private def isMarineMay(date: D) = {
    // Marine Day (3rd Monday in July),
    // was July 20th until 2003, not a holiday before 1996

    val w = date.dow
    val (y, m, d) = date.YMD
    (w == MONDAY && (d >= 15 && d <= 21) && m == JULY && y >= 2003) ||
      ((d == 20 || (d == 21 && w == MONDAY)) && m == JULY && y >= 1996 && y < 2003)

  }

  private def isMountainMay(date: D) = {
    val w = date.dow
    val (y, m, d) = date.YMD
    (d == 11 || (d == 12 && w == MONDAY)) && m == AUGUST && y >= 2016
  }

  private def isRespectfortheAgedDay(date: D) = {
    // Respect for the Aged Day (3rd Monday in September),
    // was September 15th until 2003
    val w = date.dow
    val (y, m, d) = date.YMD
    (w == MONDAY && (d >= 15 && d <= 21) && m == SEPTEMBER && y >= 2003) ||
      ((d == 15 || (d == 16 && w == MONDAY)) && m == SEPTEMBER && y < 2003)
  }

  private def isRandomHolidays(date: D) = {
    // one-shot holidays
    // Marriage of Prince Akihito
    val w = date.dow
    val (y, m, d) = date.YMD
    (d == 10 && m == APRIL && y === 1959) ||
      // Rites of Imperial Funeral
      (d == 24 && m == FEBRUARY && y === 1989) ||
      // Enthronement Ceremony
      (d == 12 && m == NOVEMBER && y === 1990) ||
      // Marriage of Prince Naruhito
      (d == 9 && m == JUNE && y === 1993)
  }

  private def isHealthAndSportsDay(date: D) = {
    val w = date.dow
    val (y, m, d) = date.YMD
    (w == MONDAY && (d >= 8 && d <= 14) && m == OCTOBER && y >= 2000) ||
      ((d == 10 || (d == 11 && w == MONDAY)) && m == OCTOBER && y < 2000)

  }

  private def isNationalCultureDay(date: D) = {
    val w = date.dow
    val (y, m, d) = date.YMD
    (d == 3 || (d == 4 && w == MONDAY)) && date.inNovember


  }

  private def isLaborThanksgivingDay(date: D) = {
    val w = date.dow
    val (y, m, d) = date.YMD
    (d == 23 || (d == 24 && w == MONDAY)) && date.inNovember

  }

  private def isEmperorsBirthday(date: D) = {
    val w = date.dow
    val (y, m, d) = date.YMD
    (d == 23 || (d == 24 && w == MONDAY)) && date.inDecember && y >= 1989

  }

  private def isVernalEquinox(date: D) = {

    val w = date.dow
    val (y, m, d) = date.YMD
    val ve = (exact_vernal_equinox_time + adjustments(y.getValue)).toInt
    (d == ve || (d == ve + 1 && w == MONDAY)) && m == MARCH

  }

  private def isAutumnalEquinox(date: D) = {
    val w = date.dow
    val (y, m, d) = date.YMD
    val ae = (exact_autumnal_equinox_time + adjustments(y.getValue)).toInt
    (w == TUESDAY && d + 1 == ae && d >= 16 && d <= 22 && m == SEPTEMBER && y >= 2003) ||
      ((d == ae || (d == ae + 1 && w == MONDAY)) && m == SEPTEMBER)
  }

  import BusinessCalendar.InternationalHolidays._

  private val holidays = List[D => Boolean](
    isWeekend,
    isNewYear,
    isBankHoliday,
    isComingOfAgeDay,
    isNationalFoundationDay,
    isGreeneryDay,
    isDaysInMay,
    isMarineMay,
    isMountainMay,
    isRespectfortheAgedDay,
    isRandomHolidays,
    isHealthAndSportsDay,
    isNationalCultureDay,
    isLaborThanksgivingDay,
    isEmperorsBirthday,
    isVernalEquinox,
    isAutumnalEquinox
  )



  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
