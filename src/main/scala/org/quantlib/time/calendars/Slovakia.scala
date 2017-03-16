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

final case class Slovakia[D: DateOps](market: Market = BSSE) extends WeekendSatSun[D] with BusinessCalendar[D]{
  override def considerBusinessDay(date: D): Boolean = ???
}