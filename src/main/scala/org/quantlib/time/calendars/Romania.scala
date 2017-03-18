package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.{Orthodox, Western}
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
/**
  * Created by neo on 11/03/2017.
  */
final case class Romania[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D]{
  import BusinessCalendar.InternationalHolidays._

  private def isUnificationDay(date: D) = date.dom == 24 && date.inJanuary
  private def isStMarysDay(date: D) = date.dom == 15 && date.inAugust
  private def isFeastofStAndrewDay(date: D) = date.dom == 30 && date.inNovember
  private def isNationalDay(date: D) = date.dom == 1 && date.inDecember

  private val holidays = List[D => Boolean](
    isWeekend,
    isNewYear,
    isDayAfterNewYear,
    isUnificationDay,
    isStMarysDay,
    isFeastofStAndrewDay,
    isNationalDay,
    Orthodox.isEasterMonday,
    isLabourDay,
    Orthodox.isPentecost,
    isChristmas,
    isBoxingDay
  )

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
