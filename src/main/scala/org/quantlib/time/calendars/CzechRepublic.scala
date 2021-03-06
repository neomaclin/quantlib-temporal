package org.quantlib.time.calendars

import java.time.Month

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.CzechRepublic.Market
import org.quantlib.time.calendars.CzechRepublic.Market._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object CzechRepublic {

  sealed trait Market

  object Market {

    case object PSE extends Market

  }

}

final case class CzechRepublic[D: DateOps](market: Market = PSE) extends WeekendSatSun[D] with BusinessCalendar[D] {
  override val toString: String = market match {
    case PSE => "Prague stock exchange"
  }

  import BusinessCalendar.InternationalHolidays._

  private def isLabourDay(date: D) = date.dom == 1 && date.inMay

  private def isLiberationDay(date: D) = date.dom == 8 && date.inMay

  private def isSSCyrilAndMethodius(date: D) = date.dom == 5 && date.inJuly

  private def isJanHusDay(date: D) = date.dom == 6 && date.inJuly

  private def isCzechStatehoodDay(date: D) = date.dom == 28 && date.inSeptember

  private def isChristmas(date: D): Boolean = date.dom == 25 && date.inDecember

  private def isChristmasEve(date: D): Boolean = date.dom == 24 && date.inDecember

  private def isIndependenceDay(date: D) = date.dom == 28 && date.inOctober

  private def isFreedomandDemocracyStruggleDay(date: D) = date.dom == 17 && date.inNovember

  private def closingDateOfExchange(date: D) = {
    val (yy, mm, dd) = date.YMD
    (dd == 2 && mm == Month.JANUARY && yy === 2004) || (dd == 31 && mm == Month.DECEMBER && yy === 2004)
  }

  private val holidays =
    List[D => Boolean](
      isWeekend,
      isNewYear,
      isLiberationDay,
      Western.isEasterMonday,
      isSSCyrilAndMethodius,
      isIndependenceDay,
      isCzechStatehoodDay,
      isJanHusDay,
      isCzechStatehoodDay,
      isFreedomandDemocracyStruggleDay,
      isChristmas,
      isChristmasEve,
      isStStephenDay,
      closingDateOfExchange)

  override def considerBusinessDay(date: D): Boolean = {
    !holidays.exists(f => f(date))
  }
}

