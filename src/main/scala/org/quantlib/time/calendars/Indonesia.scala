package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.Indonesia.Market
import org.quantlib.time.calendars.Indonesia.Market.IDX
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object Indonesia {

  sealed trait Market

  object Market {

    case object EJ extends Market

    case object JSX extends Market

    case object IDX extends Market

  }

}

final case class Indonesia[D: DateOps](market: Market = IDX) extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._

  override val toString: String = "Jakarta stock exchange"

  private def isIndependenceDay(date: D) = date.dom == 17 && date.inAugust

  private def isLocalHolidays(date: D) = {
    val d = date.dom
    date.year.getValue match {
      case 2005 =>
        // Idul Adha
        (d == 21 && date.inJanuary) ||
          // Imlek
          (d == 9 && date.inFebruary) ||
          // Moslem's New Year Day
          (d == 10 && date.inFebruary) ||
          // Nyepi
          (d == 11 && date.inMarch) ||
          // Birthday of Prophet Muhammad SAW
          (d == 22 && date.inApril) ||
          // Waisak
          (d == 24 && date.inMay) ||
          // Ascension of Prophet Muhammad SAW
          (d == 2 && date.inSeptember) ||
          // Idul Fitri
          ((d == 3 || d == 4) && date.inNovember) ||
          // National leaves
          ((d == 2 || d == 7 || d == 8) && date.inNovember) ||
          (d == 26 && date.inDecember)
      case 2006 =>
        // Idul Adha
        (d == 10 && date.inJanuary) ||
          // Moslem's New Year Day
          (d == 31 && date.inJanuary) ||
          // Nyepi
          (d == 30 && date.inMarch) ||
          // Birthday of Prophet Muhammad SAW
          (d == 10 && date.inApril) ||
          // Ascension of Prophet Muhammad SAW
          (d == 21 && date.inAugust) ||
          // Idul Fitri
          ((d == 24 || d == 25) && date.inOctober) ||
          // National leaves
          ((d == 23 || d == 26 || d == 27) && date.inOctober)
      case 2007 =>
        // Nyepi
        (d == 19 && date.inMarch) ||
          // Waisak
          (d == 1 && date.inJune) ||
          // Ied Adha
          (d == 20 && date.inDecember) ||
          // National leaves
          (d == 18 && date.inMay) ||
          ((d == 12 || d == 15 || d == 16) && date.inOctober) ||
          ((d == 21 || d == 24) && date.inOctober)
      case 2008 =>
        // Islamic New Year
        ((d == 10 || d == 11) && date.inJanuary) ||
          // Chinese New Year
          ((d == 7 || d == 8) && date.inFebruary) ||
          // Saka's New Year
          (d == 7 && date.inMarch) ||
          // Birthday of the prophet Muhammad SAW
          (d == 20 && date.inMarch) ||
          // Vesak Day
          (d == 20 && date.inMay) ||
          // Isra' Mi'raj of the prophet Muhammad SAW
          (d == 30 && date.inJuly) ||
          // National leave
          (d == 18 && date.inAugust) ||
          // Ied Fitr
          (d == 30 && date.inSeptember) ||
          ((d == 1 || d == 2 || d == 3) && date.inOctober) ||
          // Ied Adha
          (d == 8 && date.inDecember) ||
          // Islamic New Year
          (d == 29 && date.inDecember) ||
          // New Year's Eve
          (d == 31 && date.inDecember)
      case 2009 =>
        // Public holiday
        (d == 2 && date.inJanuary) ||
          // Chinese New Year
          (d == 26 && date.inJanuary) ||
          // Birthday of the prophet Muhammad SAW
          (d == 9 && date.inMarch) ||
          // Saka's New Year
          (d == 26 && date.inMarch) ||
          // National leave
          (d == 9 && date.inApril) ||
          // Isra' Mi'raj of the prophet Muhammad SAW
          (d == 20 && date.inJuly) ||
          // Ied Fitr
          (d >= 18 && d <= 23 && date.inSeptember) ||
          // Ied Adha
          (d == 27 && date.inNovember) ||
          // Islamic New Year
          (d == 18 && date.inDecember) ||
          // Public Holiday
          (d == 24 && date.inDecember) ||
          // Trading holiday
          (d == 31 && date.inDecember)
      case 2010 =>
        // Birthday of the prophet Muhammad SAW
        (d == 26 && date.inFebruary) ||
          // Saka's New Year
          (d == 16 && date.inMarch) ||
          // Birth of Buddha
          (d == 28 && date.inMay) ||
          // Ied Fitr
          (d >= 8 && d <= 14 && date.inSeptember) ||
          // Ied Adha
          (d == 17 && date.inNovember) ||
          // Islamic New Year
          (d == 7 && date.inDecember) ||
          // Public Holiday
          (d == 24 && date.inDecember) ||
          // Trading holiday
          (d == 31 && date.inDecember)
      case 2011 =>
        // Chinese New Year
        (d == 3 && date.inFebruary) ||
          // Birthday of the prophet Muhammad SAW
          (d == 15 && date.inFebruary) ||
          // Birth of Buddha
          (d == 17 && date.inMay) ||
          // Isra' Mi'raj of the prophet Muhammad SAW
          (d == 29 && date.inJune) ||
          // Ied Fitr
          (d >= 29 && date.inAugust) ||
          (d <= 2 && date.inSeptember) ||
          // Public Holiday
          (d == 26 && date.inDecember)
      case 2012 =>
        // Chinese New Year
        (d == 23 && date.inJanuary) ||
          // Saka New Year
          (d == 23 && date.inMarch) ||
          // Ied ul-Fitr
          (d >= 20 && d <= 22 && date.inAugust) ||
          // Eid ul-Adha
          (d == 26 && date.inOctober) ||
          // Islamic New Year
          (d >= 15 && d <= 16 && date.inNovember) ||
          // Public Holiday
          (d == 24 && date.inDecember) ||
          // Trading Holiday
          (d == 31 && date.inDecember)
      case 2013 =>
        // Birthday of the prophet Muhammad SAW
        (d == 24 && date.inJanuary) ||
          // Saka New Year
          (d == 12 && date.inMarch) ||
          // Isra' Mi'raj of the prophet Muhammad SAW
          (d == 6 && date.inJune) ||
          // Ied ul-Fitr
          (d >= 5 && d <= 9 && date.inAugust) ||
          // Eid ul-Adha
          (d >= 14 && d <= 15 && date.inOctober) ||
          // Islamic New Year
          (d == 5 && date.inNovember) ||
          // Public Holiday
          (d == 26 && date.inDecember) ||
          // Trading Holiday
          (d == 31 && date.inDecember)
      case 2014 =>
        // Birthday of the prophet Muhammad SAW
        (d == 14 && date.inJanuary) ||
          // Chinese New Year
          (d == 31 && date.inJanuary) ||
          // Saka New Year
          (d == 31 && date.inMarch) ||
          // Labour Day
          (d == 1 && date.inMay) ||
          // Birth of Buddha
          (d == 15 && date.inMay) ||
          // Isra' Mi'raj of the prophet Muhammad SAW
          (d == 27 && date.inMay) ||
          // Ascension Day of Jesus Christ
          (d == 29 && date.inMay) ||
          // Ied ul-Fitr
          ((d >= 28 && date.inJuly) || (d == 1 && date.inAugust)) ||
          // Public Holiday
          (d == 26 && date.inDecember) ||
          // Trading Holiday
          (d == 31 && date.inDecember)

    }

  }

  private val holidays = List[D => Boolean](
    isWeekend,
    isNewYear,
    Western.isGoodFriday,
    Western.isAscension,
    isIndependenceDay,
    isChristmas,
    isLocalHolidays
  )

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
