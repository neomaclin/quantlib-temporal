package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.InternationalHolidays.isNewYearEve
import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.UnitedKingdom.Market
import org.quantlib.time.calendars.UnitedKingdom.Market._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object UnitedKingdom{
  sealed trait Market
  object Market{
    case object Settlement extends Market
    case object Exchange extends Market
    case object Metals extends Market
  }
}

final case class UnitedKingdom[D: DateOps](market: Market = Settlement) extends WeekendSatSun[D] with BusinessCalendar[D]{

  private def isUKNewYear(date: D) = {
    // New Year's Day (possibly moved to Monday)
    val dom = date.dom
    (dom == 1 || ((dom == 2 || dom == 3) && date.dow.isMonday)) && date.inJanuary
  }

  private def isYearEnd1999(date: D) = {
    // New Year's Day (possibly moved to Monday)
    isNewYearEve(date) && date.year === 1999
  }

  private def isBankHoliday(date: D) = {
    val d = date.dom
    val dow = date.dow
    val year = date.year
    // first Monday of May (Early May Bank Holiday)
    (d <= 7 && dow.isMonday && date.inMay) ||
    // last Monday of May (Spring Bank Holiday)
     (d >= 25 && dow.isMonday && date.inMay && !(year === 2002) && !(year === 2012)) ||
    // last Monday of August (Summer Bank Holiday)
     (d >= 25 && dow.isMonday && date.inAugust) ||
    // June 3rd, 2002 only (Golden Jubilee Bank Holiday)
    // June 4rd, 2002 only (special Spring Bank Holiday)
     ((d == 3 || d == 4) && date.inJune && year === 2002) ||
    // April 29th, 2011 only (Royal Wedding Bank Holiday)
     (d == 29 && date.inApril && year === 2011) ||
    // June 4th, 2012 only (Diamond Jubilee Bank Holiday)
    // June 5th, 2012 only (Special Spring Bank Holiday)
     ((d == 4 || d == 5) && date.inJune && year === 2012)
  }

  import BusinessCalendar.InternationalHolidays._
  override val toString: String = market match {
    case  Settlement => "UK settlement"
    case  Exchange => "London Stock Exchange"
    case  Metals => "London Metals Exchange"
  }
  private val settlementHolidays = List[D => Boolean](
    isWeekend,
    isUKNewYear,
            Western.isGoodFriday,
    Western.isEasterMonday,
    isChristmasMT,
    isBoxingDayMT,
    isYearEnd1999,
      isBankHoliday

  )
  private val exchangeHolidays = settlementHolidays
  private val metalHolidays = settlementHolidays

  override def considerBusinessDay(date: D): Boolean = market match {
      case  Settlement => !settlementHolidays.exists(f => f(date))
      case  Exchange => !exchangeHolidays.exists(f => f(date))
      case  Metals => !metalHolidays.exists(f => f(date))
    }
}
