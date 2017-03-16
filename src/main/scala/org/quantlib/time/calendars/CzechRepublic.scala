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

final case class CzechRepublic[D: DateOps](market: Market = PSE) extends WeekendSatSun[D] with BusinessCalendar[D]  {
  override val toString: String = market match {
    case PSE => "Prague stock exchange"
  }

  import BusinessCalendar.InternationalHolidays._


  private def isLabourDay(date: D) = {
    date.dom == 1 && inMay(date)
  }

  private def isLiberationDay(date: D) = {
    date.dom == 8 && inMay(date)
  }

  private def isSSCyrilAndMethodius(date: D) = {
    date.dom == 5 && inJuly(date)
  }

  private def isJanHusDay(date: D) = {
    date.dom == 6 && inJuly(date)
  }

  private def isCzechStatehoodDay(date: D) = {
    date.dom == 28 && inSeptember(date)
  }

  private def isChristmas(date: D): Boolean = {
    date.dom == 25 && inDecember(date)
  }

  private def isChristmasEve(date: D): Boolean = {
    date.dom == 24 && inDecember(date)
  }

  private def isIndependenceDay(date: D) = {
    date.dom == 28 && inOctober(date)
  }

  private def isFreedomandDemocracyStruggleDay(date: D) = {
    date.dom == 17 && inNovember(date)
  }

  private def isStStephenDay(date: D) = {
    date.dom == 26 && inDecember(date)
  }

  private def closingDateOfExchange(date: D) = {
    val (yy, mm, dd) = date.YMD
    (dd == 2 && mm == Month.JANUARY && yy === 2004) || (dd == 31 && mm == Month.DECEMBER && yy === 2004)
  }

  override def considerBusinessDay(date: D): Boolean = {
    !List[D => Boolean](isWeekend, isNewYear, isLiberationDay
      , Western.isEasterMonday
      , isSSCyrilAndMethodius, isIndependenceDay
      , isCzechStatehoodDay
      , isJanHusDay, isCzechStatehoodDay
      , isFreedomandDemocracyStruggleDay
      , isChristmas, isChristmasEve
      , isStStephenDay, closingDateOfExchange).exists(_.apply(date))
  }
}

