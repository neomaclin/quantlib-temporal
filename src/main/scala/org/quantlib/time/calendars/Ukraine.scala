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

final case class Ukraine(market: Market = USE)  extends BusinessCalendar with WeekendSatSun{
  override def considerBusinessDay[D: DateOps](date: D): Boolean = ???
}
