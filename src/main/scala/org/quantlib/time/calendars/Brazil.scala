package org.quantlib.time.calendars



import org.quantlib.time.calendars.Brazil.Market
import org.quantlib.time.calendars.Brazil.Market._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._


final case class Brazil[D: DateOps](market: Market = Settlement) extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._
  import BusinessCalendar.Western

  private val settlmentHolidays =
    List[D => Boolean](
      isWeekend,
      isNewYear,
      isTiradentesDay,
      isLaborDay,
      isIndependenceDay,
      isNossaSraAparecidaDay,
      isAllSoulsDay,
      isRepublicDay,
      isChristmas,
      Western.isPassionofChrist,
      Western.isCarnival,
      Western.isCorpusChristi)

  private val exchangeHolidays =
    settlmentHolidays ++ List[D => Boolean](
      isSaoPauloCityDay,
      isRevolutionDay,
      isBlackConsciousnessDay,
      isLastBusinessDay)


  override def considerBusinessDay(date: D): Boolean = market match {
    case Settlement => !settlmentHolidays.exists(f => f(date))
    case Exchange => !exchangeHolidays.exists(f => f(date))
  }

  override val toString: String = market match {
    case Settlement => "Brazil"
    case Exchange => "BOVESPA"
  }

  private def isTiradentesDay(date: D): Boolean = date.dom == 21 && date.inApril

  private def isLaborDay(date: D): Boolean = date.dom == 1 && date.inMay

  private def isRevolutionDay(date: D): Boolean = date.dom == 9 && date.inJuly

  private def isIndependenceDay(date: D): Boolean = date.dom == 7 && date.inSeptember

  private def isNossaSraAparecidaDay(date: D): Boolean = date.dom == 12 && date.inOctober

  private def isAllSoulsDay(date: D): Boolean = date.dom == 2 && date.inNovember

  private def isRepublicDay(date: D): Boolean = date.dom == 15 && date.inNovember

  private def isBlackConsciousnessDay(date: D): Boolean = date.dom == 20 && date.inNovember && date.year >= 2007

  private def isSaoPauloCityDay(date: D): Boolean = date.dom == 25 && date.inJanuary

  private def isLastBusinessDay(date: D): Boolean = {
    val dom = date.dom
    date.inDecember && (dom == 31) || (dom >= 29 && date.dow.isFriday)
  }



}

object Brazil {

  sealed trait Market

  object Market {

    case object Settlement extends Market

    case object Exchange extends Market

  }


}
