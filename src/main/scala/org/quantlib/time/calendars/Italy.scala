package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.Italy.Market
import org.quantlib.time.calendars.Italy.Market._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object Italy {

  sealed trait Market

  object Market {

    case object Settlement extends Market

    case object Exchange extends Market

  }

}

final case class Italy[D: DateOps](market: Market = Settlement) extends WeekendSatSun[D] with BusinessCalendar[D] {
  override val toString: String = market match {
    case Settlement => "Italian settlement"
    case Exchange => "Milan stock exchange"
  }

  import BusinessCalendar.InternationalHolidays._

  private def isAssumptionDay(date: D) = date.dom == 15 && date.inAugust

  private def isLiberationDay(date: D) = date.dom == 25 && date.inApril

  private def isRepublicDay(date: D) = date.dom == 2 && date.inJune && date.year >= 2000

  private def isYearEnd1999(date: D) = date.dom == 31 && date.inDecember && date.year === 1999

  private def isImmaculateConception(date: D) = date.dom == 8 && date.inDecember

  private val exchangeHolidays = List[D => Boolean](
    isWeekend,
    isNewYear,
    Western.isGoodFriday,
    Western.isEasterMonday,
    isLabourDay,
    isAssumptionDay,
    isChristmasEve,
    isChristmas,
    isStStephenDay,
    isNewYearEve)

  private val settlementHolidays = List[D => Boolean](
    isWeekend,
    isNewYear,
    isEpiphany,
    Western.isEasterMonday,
    isLabourDay,
    isLiberationDay,
    isRepublicDay,
    isAssumptionDay,
    isAllSaintsDay,
    isImmaculateConception,
    isChristmas,
    isStStephenDay,
    isYearEnd1999
  )

  override def considerBusinessDay(date: D): Boolean = market match {
    case Settlement => !settlementHolidays.exists(f => f(date))
    case Exchange => !exchangeHolidays.exists(f => f(date))
  }
}