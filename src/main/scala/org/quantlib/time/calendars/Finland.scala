package org.quantlib.time.calendars

import java.time.DayOfWeek

import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
case object Finland extends BusinessCalendar with WeekendSatSun {

  import BusinessCalendar.InternationalHolidays._

  override def considerBusinessDay[D: DateOps](date: D): Boolean = {
    !List[D => Boolean](isWeekend, isNewYear,
      isEpiphany, isGoodFriday,
      isEasterMonday,
      isAscension, isLabourDay,
      isMidsummerEve, isIndependenceDay,
      isChristmasEve, isChristmas, isBoxingDay).exists(_.apply(date))
  }

  private def isEpiphany[D: DateOps](date: D): Boolean = date.dom == 6 && inJanuary(date)

  private def isLabourDay[D: DateOps](date: D): Boolean = date.dom == 1 && inMay(date)

  private def isMidsummerEve[D: DateOps](date: D): Boolean = {
    val dom = date.dom
    date.dow == DayOfWeek.FRIDAY && (dom >= 18 && dom <= 24) && inJune(date)
  }
  private def isIndependenceDay[D: DateOps](date: D): Boolean = {
    date.dom == 6 && inDecember(date)
  }

}
