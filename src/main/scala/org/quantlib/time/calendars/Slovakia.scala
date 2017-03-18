package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.Slovakia.Market
import org.quantlib.time.calendars.Slovakia.Market.BSSE
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object Slovakia {
  sealed trait Market
  object Market{
    case object BSSE extends Market
  }
}

final case class Slovakia[D: DateOps](market: Market = BSSE) extends WeekendSatSun[D] with BusinessCalendar[D]{

  import BusinessCalendar.InternationalHolidays._

  override val toString: String = "Bratislava stock exchange"

  private def isConstitutionDay(date: D) = date.dom == 1 && date.inSeptember

  private def isOurLadyoftheSevenSorrows(date: D) = date.dom == 15 && date.inSeptember

  private def isLiberationDay(date: D) = date.dom == 8 && date.inMay

  private def isSSCyrilAndMethodius(date: D) = date.dom == 5 && date.inJuly

  private def isSlovakNationalUprising(date: D) = date.dom == 29 && date.inAugust

  private def isFreedomAndDemocracyofSlovakRepublic(date: D) = date.dom == 17 && date.inNovember


  private def isStockExchangClosing(date: D) = {
    val (y,_,d) = date.YMD
    // unidentified closing days for stock exchange
    (d >= 24 && d <= 31 && date.inDecember && y === 2004) ||
     (d >= 24 && d <= 31 && date.inDecember && y === 2005)
  }

  private val holidays = List[D => Boolean](
    isWeekend,
    isNewYear,
    isEpiphany,
    Western.isGoodFriday,
    Western.isEasterMonday,
    isLabourDay,
    isConstitutionDay,
    isOurLadyoftheSevenSorrows,
    isLiberationDay,
    isSSCyrilAndMethodius,
    isSlovakNationalUprising,
    isFreedomAndDemocracyofSlovakRepublic,
    isStockExchangClosing,
    isAllSaintsDay,
    isChristmasEve,
    isChristmas,
    isBoxingDay
  )
  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}