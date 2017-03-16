package org.quantlib.time.calendars

import org.quantlib.time.calendars.Ukraine.Market
import org.quantlib.time.calendars.Ukraine.Market.USE
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
object Ukraine{
  sealed trait Market
  object Market{
    case object USE extends Market
  }
}

final case class Ukraine[D: DateOps](market: Market = USE)  extends WeekendSatSun[D] with BusinessCalendar[D]{
  override def considerBusinessDay(date: D): Boolean = ???
}
