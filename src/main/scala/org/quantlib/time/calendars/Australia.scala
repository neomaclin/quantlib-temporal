package org.quantlib.time.calendars


import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

final case class Australia[D:DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._
  import BusinessCalendar.Western

  private def isAustraliaDay(date: D) = {
    val dom = date.dom
    (dom == 26 || ((dom == 27 || dom == 28) && isMonday(date))) && inJanuary(date)
  }

  private def isANZACDay(date: D) = {
    val dom = date.dom
    (dom == 25 || (dom == 26 && isMonday(date))) && inApril(date)
  }

  override def considerBusinessDay(date: D): Boolean = {
    !List[D => Boolean](
      isWeekend, Western.isEasterMonday, Western.isGoodFriday
      , isNewYear, isAustraliaDay
      , isANZACDay
      , x => isSecondMonday(x) && inJune(x)
      , x => isFirstMonday(x) && inAugust(x)
      , x => isFirstMonday(x) && inOctober(x)
      , isChristmasMT
      , isBoxingDayMT).exists(_.apply(date))

  }

}
