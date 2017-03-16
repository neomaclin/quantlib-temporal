package org.quantlib.time.calendars

import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
final case class Switzerland[D: DateOps]() extends WeekendSatSun[D] with BusinessCalendar[D]{
  override def considerBusinessDay(date: D): Boolean = ???
}
