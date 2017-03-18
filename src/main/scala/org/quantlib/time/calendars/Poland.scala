package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps

import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
final case class Poland[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D] {

  private def isConstitutionDay(date: D) = date.dom == 3 && date.inMay

  private def isAssumptionOfBlessedVirginDay(date: D) = date.dom == 15 && date.inAugust

  private def isIndependenceDay(date: D) = date.dom == 11 && date.inNovember

  import BusinessCalendar.InternationalHolidays._
  private val holidays = List[D => Boolean](
    isWeekend,
    Western.isEasterMonday,
    Western.isCorpusChristi,
    isNewYear,
    isEpiphany,
    isConstitutionDay,
    isAssumptionOfBlessedVirginDay,
    isIndependenceDay,
    isChristmas,
    isBoxingDay,
    isAllSaintsDay
  )

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
