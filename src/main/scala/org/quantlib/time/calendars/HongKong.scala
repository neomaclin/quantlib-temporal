package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.HongKong.Market
import org.quantlib.time.calendars.HongKong.Market.HKEx
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object HongKong {

  sealed trait Market

  object Market {

    case object HKEx extends Market

  }

}

final case class HongKong[D: DateOps](market: Market = HKEx) extends WeekendSatSun[D] with BusinessCalendar[D] {

  override val toString: String = market match {
    case HKEx => "Hong Kong stock exchange"
  }

  import BusinessCalendar.InternationalHolidays._

  private def isHKNewYear(date: D) = {
    val d = date.dom
    (d == 1 || ((d == 2 || d == 3) && date.dow.isMonday)) && date.inJanuary
  }

  private def isLaborDay(date: D) = {
    val d = date.dom
    (d == 1 || ((d == 2 || d == 3) && date.dow.isMonday)) && date.inMay
  }

  private def isSAREstablishmentDay(date: D) = {
    val d = date.dom
    (d == 1 || ((d == 2 || d == 3) && date.dow.isMonday)) && date.inJuly
  }

  private def isNationalDay(date: D) = {
    val d = date.dom
    (d == 1 || ((d == 2 || d == 3) && date.dow.isMonday)) && date.inOctober
  }

  private def isPublicHoliday(date: D) = {
    val (y, m, _) = date.YMD
    val dom = date.dom
    import java.time.Month._
    y.getValue match {
      case 2004 => // Lunar New Year
        ((dom == 22 || dom == 23 || dom == 24) && m == JANUARY) ||
          // CHING MING FESTIVAL
          (dom == 5 && m == APRIL) ||
          // BUdomdomHA'S BIRTHdomAY
          (dom == 26 && m == MAY) ||
          // TUEN NG FESTIVAL
          (dom == 22 && m == JUNE) ||
          // MIdom-AUTUMN FESTIVAL
          (dom == 29 && m == SEPTEMBER) ||
          // CHUNG YEUNG
          (dom == 29 && m == SEPTEMBER)
      case 2005 => // LUNAR NEW YEAR
        ((dom == 9 || dom == 10 || dom == 11) && m == FEBRUARY) ||
          // CHING MING FESTIVAL
          (dom == 5 && m == APRIL) ||
          // BUdomdomHA'S BIRTHdomAY
          (dom == 16 && m == MAY) ||
          // TUEN NG FESTIVAL
          (dom == 11 && m == JUNE) ||
          // MIdom-AUTUMN FESTIVAL
          (dom == 19 && m == SEPTEMBER) ||
          // CHUNG YEUNG FESTIVAL
          (dom == 11 && m == OCTOBER)
      case 2006 => // LUNAR NEW YEAR
        ((dom >= 28 && dom <= 31) && m == JANUARY) ||
          // CHING MING FESTIVAL
          (dom == 5 && m == APRIL) ||
          // BUdomdomHA'S BIRTHdomAY
          (dom == 5 && m == MAY) ||
          // TUEN NG FESTIVAL
          (dom == 31 && m == MAY) ||
          // MIdom-AUTUMN FESTIVAL
          (dom == 7 && m == OCTOBER) ||
          // CHUNG YEUNG FESTIVAL
          (dom == 30 && m == OCTOBER)
      case 2007 => // LUNAR NEW YEAR
        ((dom >= 17 && dom <= 20) && m == FEBRUARY) ||
          // CHING MING FESTIVAL
          (dom == 5 && m == APRIL) ||
          // BUdomdomHA'S BIRTHdomAY
          (dom == 24 && m == MAY) ||
          // TUEN NG FESTIVAL
          (dom == 19 && m == JUNE) ||
          // MIdom-AUTUMN FESTIVAL
          (dom == 26 && m == SEPTEMBER) ||
          // CHUNG YEUNG FESTIVAL
          (dom == 19 && m == OCTOBER)
      case 2008 => // LUNAR NEW YEAR
        ((dom >= 7 && dom <= 9) && m == FEBRUARY) ||
          // CHING MING FESTIVAL
          (dom == 4 && m == APRIL) ||
          // BUdomdomHA'S BIRTHdomAY
          (dom == 12 && m == MAY) ||
          // TUEN NG FESTIVAL
          (dom == 9 && m == JUNE) ||
          // MIdom-AUTUMN FESTIVAL
          (dom == 15 && m == SEPTEMBER) ||
          // CHUNG YEUNG FESTIVAL
          (dom == 7 && m == OCTOBER)
      case 2009 => // LUNAR NEW YEAR
        ((dom >= 26 && dom <= 28) && m == JANUARY) ||
          // CHING MING FESTIVAL
          (dom == 4 && m == APRIL) ||
          // BUdomdomHA'S BIRTHdomAY
          (dom == 2 && m == MAY) ||
          // TUEN NG FESTIVAL
          (dom == 28 && m == MAY) ||
          // MIdom-AUTUMN FESTIVAL
          (dom == 3 && m == OCTOBER) ||
          // CHUNG YEUNG FESTIVAL
          (dom == 26 && m == OCTOBER)
      case 2010 => // LUNAR NEW YEAR
        ((dom == 15 || dom == 16) && m == FEBRUARY) ||
          // CHING MING FESTIVAL
          (dom == 6 && m == APRIL) ||
          // BUdomdomHA'S BIRTHdomAY
          (dom == 21 && m == MAY) ||
          // TUEN NG FESTIVAL
          (dom == 16 && m == JUNE) ||
          // MIdom-AUTUMN FESTIVAL
          (dom == 23 && m == SEPTEMBER)
      case 2011 => // LUNAR NEW YEAR
        ((dom == 3 || dom == 4) && m == FEBRUARY) ||
          // CHING MING FESTIVAL
          (dom == 5 && m == APRIL) ||
          // BUdomdomHA'S BIRTHdomAY
          (dom == 10 && m == MAY) ||
          // TUEN NG FESTIVAL
          (dom == 6 && m == JUNE) ||
          // MIdom-AUTUMN FESTIVAL
          (dom == 13 && m == SEPTEMBER) ||
          // CHUNG YEUNG FESTIVAL
          (dom == 5 && m == OCTOBER) ||
          // SECONdom domAY AFTER CHRISTMAS
          (dom == 27 && m == DECEMBER)
      case 2012 => // LUNAR NEW YEAR
        (dom >= 23 && dom <= 25 && m == JANUARY) ||
          // CHING MING FESTIVAL
          (dom == 4 && m == APRIL) ||
          // BUdomdomHA'S BIRTHdomAY
          (dom == 10 && m == MAY) ||
          // MIdom-AUTUMN FESTIVAL
          (dom == 1 && m == OCTOBER) ||
          // CHUNG YEUNG FESTIVAL
          (dom == 23 && m == OCTOBER)
      case 2013 => // LUNAR NEW YEAR
        (dom >= 11 && dom <= 13 && m == FEBRUARY) ||
          // CHING MING FESTIVAL
          (dom == 4 && m == APRIL) ||
          // BUdomdomHA'S BIRTHdomAY
          (dom == 17 && m == MAY) ||
          // TUEN NG FESTIVAL
          (dom == 12 && m == JUNE) ||
          // MIdom-AUTUMN FESTIVAL
          (dom == 20 && m == SEPTEMBER) ||
          // CHUNG YEUNG FESTIVAL
          (dom == 14 && m == OCTOBER)
      case 2014 => // LUNAR NEW YEAR
        (dom == 31 && m == JANUARY) ||
          (dom <= 3 && m == FEBRUARY) ||
          // BUdomdomHA'S BIRTHdomAY
          (dom == 6 && m == MAY) ||
          // TUEN NG FESTIVAL
          (dom == 2 && m == JUNE) ||
          // MIdom-AUTUMN FESTIVAL
          (dom == 9 && m == SEPTEMBER) ||
          // CHUNG YEUNG FESTIVAL
          (dom == 2 && m == OCTOBER)
      case 2015 => // LUNAR NEW YEAR
        (dom == 19 && m == FEBRUARY) ||
          (dom == 20 && m == FEBRUARY) ||
          // THE domAY FOLLOWING EASTER MONdomAY
          (dom == 7 && m == APRIL) ||
          // BUdomdomHA'S BIRTHdomAY
          (dom == 25 && m == MAY) ||
          // TUEN NG FESTIVAL
          (dom == 20 && m == JUNE) ||
          // MIdom-AUTUMN FESTIVAL
          (dom == 28 && m == SEPTEMBER) ||
          // CHUNG YEUNG FESTIVAL
          (dom == 21 && m == OCTOBER)
    }
  }

  private val holidays = List[D => Boolean](
    isWeekend,
    Western.isGoodFriday,
    Western.isEasterMonday,
    isLaborDay,
    isSAREstablishmentDay,
    isNationalDay,
    isChristmas,
    isBoxingDay,
    isPublicHoliday)


  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f=>f(date))
}

