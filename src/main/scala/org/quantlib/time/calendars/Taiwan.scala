package org.quantlib.time.calendars

import org.quantlib.time.calendars.Taiwan.Market
import org.quantlib.time.calendars.Taiwan.Market.TSEC
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object Taiwan {

  sealed trait Market

  object Market {

    case object TSEC extends Market

  }

}

final case class Taiwan[D: DateOps](market: Market = TSEC) extends WeekendSatSun[D] with BusinessCalendar[D] {
  override val toString: String = "Taiwan Stock Exchange"

  private def isPeaceMemorialDay(date: D) = date.dom == 28 && date.inFebruary

  private def isDoubleTenth(date: D) = date.dom == 10 && date.inOctober

  import java.time.Month._

  private def isYearlyHolidays(date: D) = {
    val d = date.dom
    val m = date.month
    date.year.getValue match {
      case 2002 =>
        // DRAGON BOAT FESTIVAL AND MOON FESTIVAL FALL ON SATURDAY
        // CHINESE LUNAR NEW YEAR
        (d >= 9 && d <= 17 && m == FEBRUARY) ||
          // TOMB SWEEPING DAY
          (d == 5 && m == APRIL)
      case 2003 =>
        // TOMB SWEEPING DAY FALLS ON SATURDAY
        // CHINESE LUNAR NEW YEAR
        ((d >= 31 && m == JANUARY) || (d <= 5 && m == FEBRUARY)) ||
          // DRAGON BOAT FESTIVAL
          (d == 4 && m == JUNE) ||
          // MOON FESTIVAL
          (d == 11 && m == SEPTEMBER)
      case 2004 =>
        // TOMB SWEEPING DAY FALLS ON SUNDAY
        // CHINESE LUNAR NEW YEAR
        (d >= 21 && d <= 26 && m == JANUARY) ||
          // DRAGON BOAT FESTIVAL
          (d == 22 && m == JUNE) ||
          // MOON FESTIVAL
          (d == 28 && m == SEPTEMBER)
      case 2005 =>
        // DRAGON BOAT AND MOON FESTIVAL FALL ON SATURDAY OR SUNDAY
        // CHINESE LUNAR NEW YEAR
        (d >= 6 && d <= 13 && m == FEBRUARY) ||
          // TOMB SWEEPING DAY
          (d == 5 && m == APRIL) ||
          // MAKE UP FOR LABOR DAY, NOT SEEN IN OTHER YEARS
          (d == 2 && m == MAY)
      case 2006 =>
        // DRAGON BOAT AND MOON FESTIVAL FALL ON SATURDAY OR SUNDAY
        // CHINESE LUNAR NEW YEAR
        ((d >= 28 && m == JANUARY) || (d <= 5 && m == FEBRUARY)) ||
          // TOMB SWEEPING DAY
          (d == 5 && m == APRIL) ||
          // DRAGON BOAT FESTIVAL
          (d == 31 && m == MAY) ||
          // MOON FESTIVAL
          (d == 6 && m == OCTOBER)
      case 2007 =>
        // CHINESE LUNAR NEW YEAR
        (d >= 17 && d <= 25 && m == FEBRUARY) ||
          // TOMB SWEEPING DAY
          (d == 5 && m == APRIL) ||
          // ADJUSTED HOLIDAYS
          (d == 6 && m == APRIL) ||
          (d == 18 && m == JUNE) ||
          // DRAGON BOAT FESTIVAL
          (d == 19 && m == JUNE) ||
          // ADJUSTED HOLIDAY
          (d == 24 && m == SEPTEMBER) ||
          // MOON FESTIVAL
          (d == 25 && m == SEPTEMBER)
      case 2008 =>
        // CHINESE LUNAR NEW YEAR
        (d >= 4 && d <= 11 && m == FEBRUARY) ||
          // TOMB SWEEPING DAY
          (d == 4 && m == APRIL)
      case 2009 =>
        // PUBLIC HOLIDAY
        (d == 2 && m == JANUARY) ||
          // CHINESE LUNAR NEW YEAR
          (d >= 24 && m == JANUARY) ||
          // TOMB SWEEPING DAY
          (d == 4 && m == APRIL) ||
          // DRAGON BOAT FESTIVAL
          ((d == 28 || d == 29) && m == MAY) ||
          // MOON FESTIVAL
          (d == 3 && m == OCTOBER)
      case 2010 =>
        // CHINESE LUNAR NEW YEAR
        (d >= 13 && d <= 21 && m == JANUARY) ||
          // TOMB SWEEPING DAY
          (d == 5 && m == APRIL) ||
          // DRAGON BOAT FESTIVAL
          (d == 16 && m == MAY) ||
          // MOON FESTIVAL
          (d == 22 && m == SEPTEMBER)
      case 2011 =>
        // SPRING FESTIVAL
        (d >= 2 && d <= 7 && m == FEBRUARY) ||
          // CHILDREN'S DAY
          (d == 4 && m == APRIL) ||
          // TOMB SWEEPING DAY
          (d == 5 && m == APRIL) ||
          // LABOUR DAY
          (d == 2 && m == MAY) ||
          // DRAGON BOAT FESTIVAL
          (d == 6 && m == JUNE) ||
          // MID-AUTUMN FESTIVAL
          (d == 12 && m == SEPTEMBER)
      case 2012 =>
        // SPRING FESTIVAL
        (d >= 23 && d <= 27 && m == JANUARY) ||
          // PEACE MEMORIAL DAY
          (d == 27 && m == FEBRUARY) ||
          // CHILDREN'S DAY
          // TOMB SWEEPING DAY
          (d == 4 && m == APRIL) ||
          // LABOUR DAY
          (d == 1 && m == MAY) ||
          // DRAGON BOAT FESTIVAL
          (d == 23 && m == JUNE) ||
          // MID-AUTUMN FESTIVAL
          (d == 30 && m == SEPTEMBER) ||
          // MEMORIAL DAY:
          // FOUNDING OF THE REPUBLIC OF CHINA
          (d == 31 && m == DECEMBER)
      case 2013 =>
        // SPRING FESTIVAL
        (d >= 10 && d <= 15 && m == FEBRUARY) ||
          // CHILDREN'S DAY
          (d == 4 && m == APRIL) ||
          // TOMB SWEEPING DAY
          (d == 5 && m == APRIL) ||
          // LABOUR DAY
          (d == 1 && m == MAY) ||
          // DRAGON BOAT FESTIVAL
          (d == 12 && m == JUNE) ||
          // MID-AUTUMN FESTIVAL
          (d >= 19 && d <= 20 && m == SEPTEMBER)
      case 2014 =>
        // LUNAR NEW YEAR
        (d >= 28 && d <= 30 && m == JANUARY) ||
          // SPRING FESTIVAL
          ((d == 31 && m == JANUARY) || (d <= 4 && m == FEBRUARY)) ||
          // CHILDREN'S DAY
          (d == 4 && m == APRIL) ||
          // TOMB SWEEPING DAY
          (d == 5 && m == APRIL) ||
          // LABOUR DAY
          (d == 1 && m == MAY) ||
          // DRAGON BOAT FESTIVAL
          (d == 2 && m == JUNE) ||
          // MID-AUTUMN FESTIVAL
          (d == 8 && m == SEPTEMBER)
    }
  }

  import BusinessCalendar.InternationalHolidays._

  private val holidays = List[D => Boolean](
    isWeekend,
    isLabourDay,
    isNewYear,
    isPeaceMemorialDay,
    isDoubleTenth,
    isYearlyHolidays
  )

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}