package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.Canada.Market
import org.quantlib.time.calendars.Canada.Market._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

object Canada {

  sealed trait Market

  object Market {

    case object TSX extends Market

    case object Settlement extends Market

  }

}



final case class Canada[D: DateOps](market: Market = Settlement) extends WeekendSatSun[D] with BusinessCalendar[D]  {

  import BusinessCalendar.InternationalHolidays._

  override val toString: String = market match {
    case TSX => "TSX"
    case Settlement => "Canada"
  }

  private def isFamilyDay(date: D) = {
    val dom = date.dom
    (((dom >= 15 && dom <= 21) && isMonday(date)) && date.year.getValue >= 2008) && inFebruary(date)
  }

  private def isVictoriaDay(date: D) = {
    val dom = date.dom
    ((dom >= 17 && dom <= 24) && isMonday(date)) && inMay(date)
  }

  private def isCanadaDay(date: D) = {
    val dom = date.dom
    (dom == 1 || ((dom == 2 || dom == 3) && isMonday(date))) && inJuly(date)
  }

  private def isNovember11st(date: D) = {
    val dom = date.dom
    (dom == 11 || ((dom == 12 || dom == 13) && isMonday(date))) && inNovember(date)
  }

  override def considerBusinessDay(date: D):Boolean = market match {
    case TSX => considerBusinessDayTSX(date)
    case Settlement => considerBusinessDaySettelment(date)

  }

  private def considerBusinessDayTSX(date: D): Boolean = {
    !List[D => Boolean](isWeekend, Western.isGoodFriday, isNewYear, isNewYearOnMonday
      , isFamilyDay
      , isVictoriaDay
      , isCanadaDay
      , x => isFirstMonday(x) && inAugust(x)
      , x => isFirstMonday(x) && inSeptember(x)
      , x => isSecondMonday(x) && inOctober(x)
      , isChristmas
      , isBoxingDay).exists(_.apply(date))
  }

  private def considerBusinessDaySettelment(date: D): Boolean = {
    !List[D => Boolean](isWeekend, Western.isGoodFriday, isNewYear, isNewYearOnMonday
      , isFamilyDay
      , isVictoriaDay
      , isCanadaDay
      , x => isFirstMonday(x) && inAugust(x)
      , x => isFirstMonday(x) && inSeptember(x)
      , x => isSecondMonday(x) && inOctober(x)
      , isNovember11st
      , isChristmas
      , isBoxingDay).exists(_.apply(date))

  }
}

