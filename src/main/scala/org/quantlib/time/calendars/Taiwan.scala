package org.quantlib.time.calendars

import org.quantlib.time.calendars.Taiwan.Market
import org.quantlib.time.calendars.Taiwan.Market.TSEC
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
object Taiwan {
  sealed trait Market
  object Market{
    case object TSEC extends Market
  }
}

final  case class Taiwan[D: DateOps](market: Market = TSEC) extends WeekendSatSun[D] with BusinessCalendar[D]{
  override def considerBusinessDay(date: D): Boolean = ???
}