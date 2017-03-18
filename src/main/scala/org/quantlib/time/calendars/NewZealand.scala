package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.BusinessCalendar.InternationalHolidays._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
final case class NewZealand[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D]{

  private def isAnniversaryDay(date: D) = {
    val dom = date.dom
    (dom >= 19 && dom <= 25) && date.dow.isMonday && date.inJanuary
  }
  private def isWaitangiDay(date: D) = date.dom == 6 && date.inFebruary

  private def isANZACDay(date: D) = date.dom == 25 && date.inApril

  private def isQueensBDay(date: D) = {
    date.dom <= 7 && date.dow.isMonday && date.inJune
  }
  private def isNZLabourDay(date: D) = {
    val dom = date.dom
    (dom >= 19 && dom <= 25) && date.dow.isMonday && date.inOctober
  }

  private val holidays = List[D => Boolean](
      isWeekend,
    Western.isGoodFriday,
    Western.isEasterMonday,
    isAnniversaryDay,
    isWaitangiDay,
    isANZACDay,
    isQueensBDay,
    isNZLabourDay,
    isNewYearMT,
    isDayAfterNewYearMT,
    isChristmasMT,
    isBoxingDayMT)

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
