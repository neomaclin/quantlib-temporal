package org.quantlib.time.calendars

import org.quantlib.time.calendars.Slovakia.Market
import org.quantlib.time.calendars.Slovakia.Market.BSSE
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
object Slovakia {
  sealed trait Market
  object Market{
    case object BSSE extends Market
  }
}

final case class Slovakia(market: Market = BSSE) extends BusinessCalendar with WeekendSatSun{
  override def considerBusinessDay[D: DateOps](date: D): Boolean = ???
}