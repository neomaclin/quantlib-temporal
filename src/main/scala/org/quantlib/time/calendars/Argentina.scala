package org.quantlib.time.calendars


import org.quantlib.time.calendars.Argentina.Market._
import org.quantlib.time.calendars.Argentina._
import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

final case class Argentina[D: DateOps](market: Market = Merval) extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._

  override val toString: String = "Buenos Aires stock exchange"

  override def considerBusinessDay(date:D): Boolean = {
    !List[D => Boolean](isWeekend, isHolyThursday, Western.isGoodFriday,
      isNewYear,
      isMayRevolutionDay,
      isLabourDay,
      isDeathofGeneralManuelBelgrano,
      isIndependenceDay,
      isDeathofGeneralJosédeSanMartín,
      isColumbusDay,
      isImmaculateConception,
      isChristmasEve,
      isNewYearEve).exists(_.apply(date))
  }

  private def isMayRevolutionDay(date: D) = {
    date.dom == 1 && inMay(date)
  }

  private def isLabourDay(date: D) = {
    date.dom == 25 && inMay(date)
  }

  private def isHolyThursday(date: D) = {
    date.doy == Western.easterMonday(date.year) - 4
  }

  private def theThirdWeek(date: D) = {
    val dom = date.dom
    dom >= 15 && dom <= 21 && isMonday(date)
  }

  private def isDeathofGeneralManuelBelgrano(date: D) = {
    theThirdWeek(date) && inJune(date)
  }

  private def isIndependenceDay(date: D) = {
    date.dom == 9 && inJuly(date)
  }

  private def isDeathofGeneralJosédeSanMartín(date: D) = {
    theThirdWeek(date) && inAugust(date)
  }

  private def isColumbusDay(date: D) = {
    val dom = date.dom
    ((dom == 10 || dom == 11 || dom == 12 || dom == 15 || dom == 16) && isMonday(date)) && inOctober(date)
  }

  private def isImmaculateConception(date: D) = date.dom == 8 && inDecember(date)

  private def isNewYearEve(date: D) = {
    val dom = date.dom
    (dom == 31 || (dom == 30 && isFriday(date))) && inDecember(date)
  }

}

object Argentina {

  sealed trait Market

  object Market {

    case object Merval extends Market

  }


}
