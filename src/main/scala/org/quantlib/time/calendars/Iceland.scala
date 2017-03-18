package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.Iceland.Market
import org.quantlib.time.calendars.Iceland.Market._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object Iceland {

  sealed trait Market

  object Market {

    case object ICEX extends Market

  }

}

final case class Iceland[D: DateOps](market: Market = ICEX) extends WeekendSatSun[D] with BusinessCalendar[D] {

  override val toString: String = "Iceland Stock Exchange"

  import BusinessCalendar.InternationalHolidays._
  import java.time.DayOfWeek._

  private def isIceLandNewYear(date: D) = {
    val dom = date.dom
    dom == 1 || ((dom == 2 || dom == 3) && date.dow == MONDAY) && date.inJanuary
  }
  private def isIndependenceDay(date: D) = date.dom == 17 && date.inJune

  private def isCommerceDay(date: D) = date.dom <= 7 && date.dow.isMonday && date.inJune

  private def isFirstDayOfSummer(date: D) = {
    val dom = date.dom
    dom >= 19 && dom <= 25 && date.dow == THURSDAY && date.inApril
  }

  private val holidays = List[D => Boolean](
    isWeekend,
    isIceLandNewYear,
    Western.isHolyThursday,
    Western.isGoodFriday,
    Western.isEasterMonday,
    Western.isAscension,
    Western.isWhitMonday,
    isLabourDay,
    isFirstDayOfSummer,
    isIndependenceDay,
    isCommerceDay,
    isChristmas,
    isBoxingDay
  )


  override def considerBusinessDay(date: D): Boolean = holidays.exists(f => f(date))
}