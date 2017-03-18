package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
/**
  * Created by neo on 11/03/2017.
  */
final case class Norway[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D]{
  import BusinessCalendar.InternationalHolidays._

  private def isNationalIndependenceDay(date: D) ={
    date.dom == 7  && date.inMay
  }
  private val holidays = List[D => Boolean](
    isWeekend,
    Western.isHolyThursday,
    Western.isGoodFriday,
    Western.isEasterMonday,
    Western.isAscension,
    Western.isWhitMonday,
    isNewYear,
    isLabourDay,
    isChristmas,
    isBoxingDay,
    isNationalIndependenceDay
  )

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
