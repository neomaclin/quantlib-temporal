package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.InternationalHolidays._
import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.Germany.Market
import org.quantlib.time.calendars.Germany.Market._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object Germany {

  sealed trait Market

  object Market {

    case object Settlement extends Market

    case object FrankfurtStockExchange extends Market

    case object Xetra extends Market

    case object Eurex extends Market

    case object Euwax extends Market

  }

}

final case class Germany[D:DateOps](market: Market = FrankfurtStockExchange) extends WeekendSatSun[D] with BusinessCalendar[D]  {

  override val toString: String = market match {
    case FrankfurtStockExchange => "Frankfurt stock exchange"
    case Xetra => "Xetra"
    case Eurex => "Eurex"
    case Euwax => "Euwax"
    case Settlement => "German settlement"
  }

  private def isNationalDay(date: D): Boolean = {
    date.dom == 3 && inOctober(date)
  }

  private def isLabourDay(date: D): Boolean = {
    date.dom == 1 && inMay(date)
  }
  private def SettlementBusinessDays(date: D): Boolean = {
    !List[D => Boolean](isWeekend,
      isNewYear, Western.isGoodFriday,
      Western.isEasterMonday, Western.isAscension,
      Western.isWhitMonday, Western.isCorpusChristi, isNationalDay,
      isLabourDay, isChristmasEve, isChristmas,
      isBoxingDay, isNewYearEve).exists(_.apply(date))
  }

  private def FSEBusinessDays(date: D): Boolean = {
    !List[D => Boolean](isWeekend,
      isNewYear, Western.isGoodFriday,
      Western.isEasterMonday, isLabourDay,
      isChristmasEve, isChristmas, isBoxingDay, isNewYearEve).exists(_.apply(date))

  }

  override def considerBusinessDay(date: D): Boolean = market match {
    case FrankfurtStockExchange => FSEBusinessDays(date)
    case Xetra => FSEBusinessDays(date)
    case Eurex => FSEBusinessDays(date)
    case Euwax => FSEBusinessDays(date)
    case Settlement => SettlementBusinessDays(date)
  }
}
