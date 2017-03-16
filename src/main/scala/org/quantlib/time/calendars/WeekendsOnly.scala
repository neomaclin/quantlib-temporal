package org.quantlib.time.calendars

import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
final case class FriSat[D: DateOps]() extends WeekendFriSat[D] with BusinessCalendar[D] {
  def considerBusinessDay(date: D): Boolean = !isWeekend(date)
}

final case class SatSun[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D] {
  def considerBusinessDay(date: D): Boolean = !isWeekend(date)
}