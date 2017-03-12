package org.quantlib.time.calendars

import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
object SouthKorea {
  sealed trait Market
  object Market{
    case object KRX extends Market
    case object Settlement extends Market
  }
}

final case class SouthKorea() extends BusinessCalendar with WeekendSatSun{
  override def considerBusinessDay[D: DateOps](date: D): Boolean = ???
}