package org.quantlib.time.calendars

import java.time.DayOfWeek

import org.quantlib.time.calendars.Brazil.Market
import org.quantlib.time.calendars.Brazil.Market._
import org.quantlib.time.calendars.BusinessCalendar.InternationalHolidays._
import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._


final case class Brazil(market: Market = Settlement) extends BusinessCalendar with WeekendSatSun {

  private def considerBusinessDaySettlement[D: DateOps](date: D): Boolean = {
    !List[D => Boolean](isWeekend, isNewYear, isTiradentesDay,
      isLaborDay, isIndependenceDay,
      isNossaSraAparecidaDay, isAllSoulsDay,
      isRepublicDay, isChristmas, isPassionofChrist,
      isCarnival, isCorpusChristi).exists(_.apply(date))
  }

  private def considerBusinessDayExchanage[D: DateOps](date: D): Boolean = {
    !List[D => Boolean](isWeekend, isNewYear, isSaoPauloCityDay
      , isTiradentesDay
      , isLaborDay, isIndependenceDay
      , isRevolutionDay
      , isNossaSraAparecidaDay, isAllSoulsDay, isBlackConsciousnessDay
      , isRepublicDay, isChristmas, isPassionofChrist
      , isCarnival, isCorpusChristi, isLastBusinessDay).exists(_.apply(date))
  }

  override def considerBusinessDay[D: DateOps](date: D): Boolean = market match {
    case Settlement => considerBusinessDaySettlement(date)
    case Exchange => considerBusinessDayExchanage(date)
  }

  override val toString: String = market match {
    case Settlement => "Brazil"
    case Exchange => "BOVESPA"
  }

  private def isTiradentesDay[D: DateOps](date: D): Boolean = date.dom == 21 && inApril(date)

  private def isLaborDay[D: DateOps](date: D): Boolean = date.dom == 1 && inMay(date)

  private def isRevolutionDay[D: DateOps](date: D): Boolean = date.dom == 9 && inJuly(date)

  private def isIndependenceDay[D: DateOps](date: D): Boolean = date.dom == 7 && inSeptember(date)

  private def isNossaSraAparecidaDay[D: DateOps](date: D): Boolean = date.dom == 12 && inOctober(date)

  private def isAllSoulsDay[D: DateOps](date: D): Boolean = date.dom == 2 && inNovember(date)

  private def isRepublicDay[D: DateOps](date: D): Boolean = date.dom == 15 && inNovember(date)

  private def isBlackConsciousnessDay[D: DateOps](date: D): Boolean = {
    date.dom == 20 && inNovember(date) && date.year.getValue >= 2007
  }

  private def isSaoPauloCityDay[D: DateOps](date: D): Boolean = date.dom == 25 && inJanuary(date)

  private def isLastBusinessDay[D: DateOps](date: D): Boolean = {
    val dom = date.dom
    inDecember(date) && (dom == 31) || (dom >= 29 && date.dow == DayOfWeek.FRIDAY)
  }

  private def isPassionofChrist[D: DateOps](date: D): Boolean = {
    date.doy == Western.easterMonday(date.year) - 3
  }

  private def isCarnival[D: DateOps](date: D): Boolean = {
    val em = Western.easterMonday(date.year)
    val doy = date.doy
    (doy == em - 49) || (doy == em - 48)
  }

}

object Brazil {

  sealed trait Market

  object Market {

    case object Settlement extends Market

    case object Exchange extends Market

  }


}
