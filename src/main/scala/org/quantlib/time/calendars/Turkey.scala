package org.quantlib.time.calendars

import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
/**
  * Created by neo on 11/03/2017.
  */
final case class Turkey[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D]{

  import BusinessCalendar.InternationalHolidays._
  private def isNationalHoliday(date: D) = {
    val d = date.dom
     (d == 23 && date.inApril) ||
    // 1 may/ National Holiday
     (d == 1 && date.inMay) ||
    // 19 may/ National Holiday
      (d == 19 && date.inMay) ||
    // 30 aug/ National Holiday
      (d == 30 && date.inAugust) ||
    ///29 ekim  National Holiday
     (d == 29 && date.inOctober)
  }
  private def isLocalHoliday(date: D) = {
    // Local Holidays
    val d = date.dom
    date.year.getValue match {
      case 2004 =>
        // Kurban
        (date.inFebruary && d <= 4) ||
          // Ramadan
          (date.inNovember && d >= 14 && d <= 16)
      case 2005 =>
        // Kurban
        (date.inJanuary && d >= 19 && d <= 21) ||
          // Ramadan
          (date.inNovember && d >= 2 && d <= 5)
      case 2006 =>
        // Kurban
        (date.inJanuary && d >= 10 && d <= 13) ||
          // Ramadan
          (date.inOctober && d >= 23 && d <= 25) ||
          // Kurban
          (date.inDecember && d == 31)
      case 2007 =>
        // Kurban
        (date.inJanuary && d <= 3) ||
          // Ramadan
          (date.inOctober && d >= 12 && d <= 14) ||
          // Kurban
          (date.inDecember && d >= 20 && d <= 23)
      case 2008 =>
        // Ramadan
        (date.inSeptember && d == 30) ||
          (date.inOctober && d <= 2) ||
          // Kurban
          (date.inDecember && d >= 8 && d <= 11)
      case 2009 =>
        // Ramadan
        (date.inSeptember && d >= 20 && d <= 22) ||
          // Kurban
          (date.inNovember && d >= 27 && d <= 30)
      case 2010 =>
        // Ramadan
        (date.inSeptember && d >= 9 && d <= 11) ||
          // Kurban
          (date.inNovember && d >= 16 && d <= 19)
      case 2011 =>
        // not clear from borsainstanbul.com
        (date.inOctober && d == 1) ||
          (date.inNovember && d >= 9 && d <= 13)
      case 2012 =>
        // Ramadan
        (date.inAugust && d >= 18 && d <= 21) ||
          // Kurban
          (date.inOctober && d >= 24 && d <= 28)
      case 2013 =>
        // Ramadan
        (date.inAugust && d >= 7 && d <= 10) ||
          // Kurban
          (date.inOctober && d >= 14 && d <= 18) ||
          // additional holiday for Republic Day
          (date.inOctober && d == 28)
      case 2014 =>
        // Ramadan
        (date.inJuly && d >= 27 && d <= 30) ||
          // Kurban
          (date.inOctober && d >= 4 && d <= 7) ||
          // additional holiday for Republic Day
          (date.inOctober && d == 29)
    }

  }
  private val holidays = List[D => Boolean](
    isWeekend,
    isNewYear,
    isNationalHoliday,
    isLocalHoliday
  )
  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
