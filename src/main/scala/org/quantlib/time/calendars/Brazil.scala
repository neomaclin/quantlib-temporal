package org.quantlib.time.calendars

import java.time.DayOfWeek

import org.quantlib.time.calendars.Brazil.Market
import org.quantlib.time.calendars.Brazil.Market._
import org.quantlib.time.calendars.BusinessCalendar.InternationalHolidays._
import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._


final case class Brazil[D: DateOps](market: Market = Settlement) extends WeekendSatSun[D] with BusinessCalendar[D]  {

  import BusinessCalendar.InternationalHolidays._
  import BusinessCalendar.Western

  private def considerBusinessDaySettlement(date: D): Boolean = {
    !List[D => Boolean](isWeekend, isNewYear, isTiradentesDay,
      isLaborDay, isIndependenceDay,
      isNossaSraAparecidaDay, isAllSoulsDay,
      isRepublicDay, isChristmas, isPassionofChrist,
      isCarnival, Western.isCorpusChristi).exists(_.apply(date))
  }

  private def considerBusinessDayExchanage(date: D): Boolean = {
    !List[D => Boolean](isWeekend, isNewYear, isSaoPauloCityDay
      , isTiradentesDay
      , isLaborDay, isIndependenceDay
      , isRevolutionDay
      , isNossaSraAparecidaDay, isAllSoulsDay, isBlackConsciousnessDay
      , isRepublicDay, isChristmas, isPassionofChrist
      , isCarnival, Western.isCorpusChristi, isLastBusinessDay).exists(_.apply(date))
  }

  override def considerBusinessDay(date: D): Boolean = market match {
    case Settlement => considerBusinessDaySettlement(date)
    case Exchange => considerBusinessDayExchanage(date)
  }

  override val toString: String = market match {
    case Settlement => "Brazil"
    case Exchange => "BOVESPA"
  }

  private def isTiradentesDay(date: D): Boolean = date.dom == 21 && inApril(date)

  private def isLaborDay(date: D): Boolean = date.dom == 1 && inMay(date)

  private def isRevolutionDay(date: D): Boolean = date.dom == 9 && inJuly(date)

  private def isIndependenceDay(date: D): Boolean = date.dom == 7 && inSeptember(date)

  private def isNossaSraAparecidaDay(date: D): Boolean = date.dom == 12 && inOctober(date)

  private def isAllSoulsDay(date: D): Boolean = date.dom == 2 && inNovember(date)

  private def isRepublicDay(date: D): Boolean = date.dom == 15 && inNovember(date)

  private def isBlackConsciousnessDay(date: D): Boolean = {
    date.dom == 20 && inNovember(date) && date.year >= 2007
  }

  private def isSaoPauloCityDay(date: D): Boolean = date.dom == 25 && inJanuary(date)

  private def isLastBusinessDay(date: D): Boolean = {
    val dom = date.dom
    inDecember(date) && (dom == 31) || (dom >= 29 && date.dow == DayOfWeek.FRIDAY)
  }

  private def isPassionofChrist(date: D): Boolean = {
    date.doy == Western.easterMonday(date.year) - 3
  }

  private def isCarnival(date: D): Boolean = {
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
