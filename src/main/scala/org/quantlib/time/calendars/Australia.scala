package org.quantlib.time.calendars


import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

final case class Australia[D:DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._
  import BusinessCalendar.Western

  private def isAustraliaDay(date: D) = {
    val dom = date.dom
    (dom == 26 || ((dom == 27 || dom == 28) && date.dow.isMonday)) && date.inJanuary
  }

  private def isANZACDay(date: D) = {
    val dom = date.dom
    (dom == 25 || (dom == 26 && date.dow.isMonday)) && date.inApril
  }
  private val holidays = List[D => Boolean](
    isWeekend,
    Western.isEasterMonday,
    Western.isGoodFriday
    , isNewYear,
    isAustraliaDay
    , isANZACDay
    , x => isSecondMonday(x) && x.inJune
    , x => isFirstMonday(x) && x.inAugust
    , x => isFirstMonday(x) && x.inOctober
    , isChristmasMT
    , isBoxingDayMT)

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(_.apply(date))

}
