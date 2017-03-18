package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
final case class SouthAfrica[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._

  private def isSANewYears(date: D) = {
    val dom = date.dom
    (dom == 1 || (dom == 2 && date.dow.isMonday)) && date.inJanuary
  }

  private def isHumanRightsDay(date: D) = {
    val dom = date.dom
    (dom == 21 || (dom == 22 && date.dow.isMonday)) && date.inMarch

  }

  private def isFreedomDay(date: D) = {
    val dom = date.dom
    (dom == 27 || (dom == 28 && date.dow.isMonday)) && date.inApril
  }

  private def isWorkersDay(date: D) = {
    val dom = date.dom
    (dom == 1 || (dom == 2 && date.dow.isMonday)) && date.inMay
  }

  private def isYouthDay(date: D) = {
    val dom = date.dom
    (dom == 16 || (dom == 17 && date.dow.isMonday)) && date.inJune

  }

  private def isNationalWomenDay(date: D) = {
    val dom = date.dom
    (dom == 9 || (dom == 10 && date.dow.isMonday)) && date.inAugust

  }

  private def isHeritageDay(date: D) = {
    val dom = date.dom
    (dom == 24 || (dom == 25 && date.dow.isMonday)) && date.inSeptember

  }

  private def isReconciliationDay(date: D) = {
      val dom = date.dom
      (dom == 16 || (dom == 17 && date.dow.isMonday)) && date.inDecember

  }

  private def isGoodwillDay(date: D) = {
     val dom = date.dom
    (dom == 26 || (dom == 27 && date.dow.isMonday)) && date.inDecember

  }

  private def isElectionDay(date: D) = {
    // Election Day, April 14th 2004
    val (y, _, dom) = date.YMD
    (dom == 14 && date.inApril && y === 2004) ||
      // one-shot: Election day 2009
      (dom == 22 && date.inApril && y === 2009) ||
      // one-shot: Election day 2016
      (dom == 3 && date.inAugust && y === 2016)
  }

  private val holidays = List[D => Boolean](
    isWeekend,
    isSANewYears,
    Western.isGoodFriday,
    Western.isEasterMonday,
    isFreedomDay,
    isElectionDay,
    isGoodwillDay,
    isHeritageDay,
    isHumanRightsDay,
    isNationalWomenDay,
    isReconciliationDay,
    isWorkersDay,
    isYouthDay,
    isChristmas
  )

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
