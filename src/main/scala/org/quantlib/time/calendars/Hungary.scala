package org.quantlib.time.calendars
import org.quantlib.time.calendars.BusinessCalendar.InternationalHolidays._
import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
/**
  * Created by neo on 11/03/2017.
  */
final case class Hungary[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D]{
  private def isNationalDay(date: D) = date.dom == 15  && date.inMarch

  private def isConstitutionDay(date: D) = date.dom == 20  && date.inAugust

  private def isRepublicDay(date: D) = date.dom == 23  && date.inOctober

  private val holidays =
    List[D => Boolean](
      isWeekend,
      Western.isEasterMonday,
      Western.isWhitMonday,
      isNewYear,
      isNationalDay,
      isConstitutionDay,
      isLabourDay,
      isRepublicDay,
      isAllSaintsDay,
      isChristmas,
      isBoxingDay)

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))

}
