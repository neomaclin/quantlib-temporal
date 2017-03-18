package org.quantlib.time.calendars

import java.time.DayOfWeek

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
final case class Finland[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._

  private val holidays =
    List[D => Boolean](
      isWeekend,
      isNewYear,
      isEpiphany,
      Western.isGoodFriday,
      Western.isEasterMonday,
      Western.isAscension,
      isLabourDay,
      isMidsummerEve,
      isIndependenceDay,
      isChristmasEve,
      isChristmas,
      isBoxingDay)

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(_.apply(date))


  private def isLabourDay(date: D): Boolean = date.dom == 1 && date.inMay

  private def isMidsummerEve(date: D): Boolean = {
    val dom = date.dom
    date.dow == DayOfWeek.FRIDAY && (dom >= 18 && dom <= 24) && date.inJune
  }

  private def isIndependenceDay(date: D): Boolean = date.dom == 6 && date.inDecember


}
