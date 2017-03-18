package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
final case class Denmark[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._

  private def isConstitutionDay(date: D): Boolean = date.dom == 5 && date.inJune

  private val holidays =
    List[D => Boolean](
      isWeekend,
      isNewYear,
      Western.isGoodFriday,
      Western.isEasterMonday,
      Western.isHolyThursday,
      Western.isGeneralPrayerDay,
      Western.isAscension,
      Western.isWhitMonday,
      isConstitutionDay,
      isBoxingDay,
      isChristmas)

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(_.apply(date))

}
