package org.quantlib.time.calendars

import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
case object WeekendsOnly extends BusinessCalendar with WeekendSatSun {
  def considerBusinessDay[D: DateOps](date: D): Boolean = !isWeekend[D](date)
}
