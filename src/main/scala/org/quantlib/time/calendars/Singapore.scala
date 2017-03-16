package org.quantlib.time.calendars

import org.quantlib.time.calendars.Singapore.Market
import org.quantlib.time.calendars.Singapore.Market.SGX
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
object Singapore {
  sealed trait Market
  object Market{
    case object SGX extends Market
  }
}

final case class Singapore[D: DateOps](market: Market = SGX ) extends WeekendSatSun[D] with BusinessCalendar[D]{
  override def considerBusinessDay(date: D): Boolean = ???
}
