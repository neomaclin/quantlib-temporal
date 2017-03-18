package org.quantlib.time.calendars


import org.quantlib.time.calendars.Argentina.Market._
import org.quantlib.time.calendars.Argentina._
import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

final case class Argentina[D: DateOps](market: Market = Merval) extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._

  override val toString: String = "Buenos Aires stock exchange"
  private val holidays =
    List[D => Boolean](
      isWeekend,
      Western.isHolyThursday,
      Western.isGoodFriday,
      isNewYear,
      isMayRevolutionDay,
      isLabourDay,
      isDeathofGeneralManuelBelgrano,
      isIndependenceDay,
      isDeathofGeneralJosédeSanMartín,
      isColumbusDay,
      isImmaculateConception,
      isChristmasEve,
      isNewYearEve)

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))

  private def isMayRevolutionDay(date: D) = date.dom == 1 && date.inMay

  private def isLabourDay(date: D) = date.dom == 25 && date.inMay

  private def theThirdWeek(date: D) = {
    val dom = date.dom
    dom >= 15 && dom <= 21 && date.dow.isMonday
  }

  private def isDeathofGeneralManuelBelgrano(date: D) = theThirdWeek(date) && date.inJune

  private def isIndependenceDay(date: D) = date.dom == 9 && date.inJuly

  private def isDeathofGeneralJosédeSanMartín(date: D) = theThirdWeek(date) && date.inAugust

  private def isColumbusDay(date: D) = {
    val dom = date.dom
    ((dom == 10 || dom == 11 || dom == 12 || dom == 15 || dom == 16) && date.dow.isMonday) && date.inOctober
  }

  private def isImmaculateConception(date: D) = date.dom == 8 && date.inDecember

  private def isNewYearEve(date: D) = {
    val dom = date.dom
    (dom == 31 || (dom == 30 && date.dow.isFriday)) && date.inDecember
  }

}

object Argentina {

  sealed trait Market

  object Market {

    case object Merval extends Market

  }

}
