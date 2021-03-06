package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
final case class Switzerland[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._

  private def isNationalDay(date: D) = date.dom == 1 && date.inAugust

  private val holidays = List[D => Boolean](
    isWeekend,
    isNewYear,
    isDayAfterNewYear,
    Western.isGoodFriday,
    Western.isEasterMonday,
    Western.isAscension,
    Western.isWhitMonday,
    isLabourDay,
    isNationalDay,
    isChristmas,
    isBoxingDay
  )

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
