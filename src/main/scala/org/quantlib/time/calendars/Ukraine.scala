package org.quantlib.time.calendars

import java.time.DayOfWeek

import org.quantlib.time.calendars.Ukraine.Market
import org.quantlib.time.calendars.Ukraine.Market.USE
import org.quantlib.time.calendars.BusinessCalendar.Orthodox
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object Ukraine {

  sealed trait Market

  object Market {

    case object USE extends Market

  }

}

final case class Ukraine[D: DateOps](market: Market = USE) extends WeekendSatSun[D] with BusinessCalendar[D] {

  private def isUkraineNewYear(date: D) = {
     val dom = date.dom
    (dom == 1 || ((dom == 2 || dom == 3) && date.dow.isMonday)) && date.inJanuary
  }
  private def isIndependenceDay(date: D) = date.dom == 24 && date.inAugust

  private def isDefendersDay(date: D) = date.dom == 14 && date.inOctober && date.year >= 2015

  private def isVictoryDay(date: D) = {
    val dom = date.dom
    (dom == 9 || ((dom == 10 || dom == 11) && date.dow == DayOfWeek.MONDAY)) && date.inMay
  }

  private def isConstitutionDay(date: D) = date.dom == 28 && date.inJune

  private def isWomensDay(date: D) = {
    val d = date.dom
    (d == 8 || ((d == 9 || d == 10) && date.dow.isMonday)) && date.inMarch
  }

  private def isWorkersSolidarityDay(date: D) = {
    val d = date.dom
    (d == 1 || d == 2 || (d == 3 && date.dow.isMonday)) && date.inMay
  }
  private val holidays = List[D => Boolean](
    isWeekend,
    isUkraineNewYear,
    Orthodox.isChristmas,
    isWomensDay,
    Orthodox.isEasterMonday,
    Orthodox.isHolyTrinity,
    isWorkersSolidarityDay,
    isVictoryDay,
    isConstitutionDay,
    isIndependenceDay,
    isDefendersDay)

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f=>f(date))
}
