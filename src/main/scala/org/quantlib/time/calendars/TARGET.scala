package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 19/03/2017.
  */

final case class TARGET[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._

  private val holidays = List[D => Boolean](
    isWeekend,
    isNewYear,
    x => Western.isGoodFriday(x) && x.year >= 2000,
    x => Western.isEasterMonday(x) && x.year >= 2000,
    x => isLabourDay(x) && x.year >= 2000,
    isChristmas,
    x => isBoxingDay(x) && x.year >= 2000,
    x => isNewYearEve(x) && (x.year === 1998 || x.year === 1999 || x.year === 2001)
  )

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
