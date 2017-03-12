package org.quantlib.time.calendars


import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

case object Australia extends BusinessCalendar with WeekendSatSun {

  import BusinessCalendar.InternationalHolidays._

  private def isAustraliaDay[D: DateOps](date: D) = {
    val dom = date.dom
    (dom == 26 || ((dom == 27 || dom == 28) && isMonday(date))) && inJanuary(date)
  }

  private def isANZACDay[D: DateOps](date: D) = {
    val dom = date.dom
    (dom == 25 || (dom == 26 && isMonday(date))) && inApril(date)
  }

  override def considerBusinessDay[D:DateOps](date: D): Boolean = {
    !List[D => Boolean](
      isWeekend, isEasterMonday, isGoodFriday
      , isNewYear, isAustraliaDay
      , isANZACDay
      , x => isSecondMonday(x) && inJune(x)
      , x => isFirstMonday(x) && inAugust(x)
      , x => isFirstMonday(x) && inOctober(x)
      , isChristmasMT
      , isBoxingDayMT).exists(_.apply(date))

  }

}
