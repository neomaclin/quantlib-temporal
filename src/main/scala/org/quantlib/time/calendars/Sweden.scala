package org.quantlib.time.calendars


import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
final case class Sweden[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._

  private def isMidsummerEve(date: D) = {
    val dom = date.dom
    date.dow.isFriday && (dom >= 19 && dom <= 25) && date.inJune
  }

  private val holidays = List[D => Boolean](
    isWeekend,
    isNewYear,
    isNewYearEve,
    Western.isGoodFriday,
    Western.isEasterMonday,
    Western.isAscension,
    x => Western.isWhitMonday(x) && x.year < 2005,
    isMidsummerEve,
    isChristmasEve,
    isChristmas,
    isBoxingDay
  )

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
