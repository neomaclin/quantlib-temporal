package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
final case class Denmark[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._

  private def isMaundayThursday(date: D): Boolean = {
    date.doy == Western.easterMonday(date.year) - 4
  }

  private def isGeneralPrayerDay(date: D): Boolean = {
    date.doy == Western.easterMonday(date.year) + 25
  }


  private def isConstitutionDay(date: D): Boolean = {
    val dom = date.dom
    dom == 5 && inJune(date)
  }

  override def considerBusinessDay(date: D): Boolean = {
    !List[D => Boolean](isWeekend
      , isNewYear, Western.isGoodFriday
      , Western.isEasterMonday
      , isMaundayThursday, isGeneralPrayerDay
      , Western.isAscension
      , Western.isWhitMonday, isConstitutionDay
      , isBoxingDay
      , isChristmas).exists(_.apply(date))
  }
}
