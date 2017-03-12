package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
case object Denmark extends BusinessCalendar with WeekendSatSun {

  import BusinessCalendar.InternationalHolidays._

  private def isMaundayThursday[D: DateOps](date: D): Boolean = {
    date.doy == Western.easterMonday(date.year) - 4
  }

  private def isGeneralPrayerDay[D: DateOps](date: D): Boolean = {
    date.doy == Western.easterMonday(date.year) + 25
  }


  private def isConstitutionDay[D: DateOps](date: D): Boolean = {
    val dom = date.dom
    dom == 5 && inJune(date)
  }

  override def considerBusinessDay[D: DateOps](date: D): Boolean = {
    !List[D => Boolean](isWeekend
      , isNewYear, isGoodFriday
      , isEasterMonday
      , isMaundayThursday, isGeneralPrayerDay
      , isAscension
      , isWhitMonday, isConstitutionDay
      , isBoxingDay
      , isChristmas).exists(_.apply(date))
  }
}
