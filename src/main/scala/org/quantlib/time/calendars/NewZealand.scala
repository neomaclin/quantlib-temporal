package org.quantlib.time.calendars

import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
case object NewZealand extends BusinessCalendar with WeekendSatSun{
  override def considerBusinessDay[D: DateOps](date: D): Boolean = ???
}
