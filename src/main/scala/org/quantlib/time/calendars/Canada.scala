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


final case class Canada[D: DateOps](market: Market = Settlement) extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._

  override val toString: String = market match {
    case TSX => "TSX"
    case Settlement => "Canada"
  }

  private def isFamilyDay(date: D) = {
    val dom = date.dom
    (((dom >= 15 && dom <= 21) && date.dow.isMonday) && date.year.getValue >= 2008) && date.inFebruary
  }

  private def isVictoriaDay(date: D) = {
    val dom = date.dom
    ((dom >= 17 && dom <= 24) && date.dow.isMonday) && date.inMay
  }

  private def isCanadaDay(date: D) = {
    val dom = date.dom
    (dom == 1 || ((dom == 2 || dom == 3) && date.dow.isMonday)) && date.inJuly
  }

  private def isNovember11st(date: D) = {
    val dom = date.dom
    (dom == 11 || ((dom == 12 || dom == 13) && date.dow.isMonday)) && date.inNovember
  }

  override def considerBusinessDay(date: D): Boolean = market match {
    case TSX => !TSXHolidays.exists(f => f(date))
    case Settlement => !settlementHolidays.exists(f => f(date))

  }

  private val TSXHolidays =
    List[D => Boolean](
      isWeekend,
      Western.isGoodFriday,
      isNewYear,
      isNewYearOnMonday,
      isFamilyDay,
      isVictoriaDay,
      isCanadaDay,
      x => isFirstMonday(x) && x.inAugust,
      x => isFirstMonday(x) && x.inSeptember,
      x => isSecondMonday(x) && x.inOctober,
      isChristmas,
      isBoxingDay)


  private val settlementHolidays = TSXHolidays ++ List[D => Boolean](isNovember11st)

}

