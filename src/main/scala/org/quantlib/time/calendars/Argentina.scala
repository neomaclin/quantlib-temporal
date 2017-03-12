package org.quantlib.time.calendars


import org.quantlib.time.calendars.Argentina.Market._
import org.quantlib.time.calendars.Argentina._
import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

final case class Argentina(market: Market = Merval) extends BusinessCalendar with WeekendSatSun {

  import BusinessCalendar.InternationalHolidays._

  override val toString: String = "Buenos Aires stock exchange"

  override def considerBusinessDay[D: DateOps](date:D): Boolean = {
    !List[D => Boolean](isWeekend, isHolyThursday, isGoodFriday,
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

  private def isMayRevolutionDay[D:DateOps](date:D) = {
    date.dom == 1 && inMay(date)
  }

  private def isLabourDay[D:DateOps](date:D) = {
    date.dom == 25 && inMay(date)
  }

  private def isHolyThursday[D:DateOps](date:D) = {
    date.doy == Western.easterMonday(date.year) - 4
  }

  private def theThirdWeek[D:DateOps](date:D) = {
    val dom = date.dom
    dom >= 15 && dom <= 21 && isMonday(date)
  }

  private def isDeathofGeneralManuelBelgrano[D:DateOps](date:D) = {
    theThirdWeek(date) && inJune(date)
  }

  private def isIndependenceDay[D:DateOps](date:D) = {
    date.dom == 9 && inJuly(date)
  }

  private def isDeathofGeneralJosédeSanMartín[D:DateOps](date:D) = {
    theThirdWeek(date) && inAugust(date)
  }

  private def isColumbusDay[D:DateOps](date:D) = {
    val dom = date.dom
    ((dom == 10 || dom == 11 || dom == 12 || dom == 15 || dom == 16) && isMonday(date)) && inOctober(date)
  }

  private def isImmaculateConception[D:DateOps](date:D) = date.dom == 8 && inDecember(date)

  private def isNewYearEve[D:DateOps](date:D) = {
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
