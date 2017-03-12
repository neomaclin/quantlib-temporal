package org.quantlib.time.calendars

import org.quantlib.time.calendars.Italy.Market
import org.quantlib.time.calendars.Italy.Market._
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
object Italy {
  sealed trait Market
  object Market{
    case object Settlement extends Market
    case object Exchange extends Market
  }

}

final case class Italy(market: Market = Settlement) extends BusinessCalendar with WeekendSatSun{
  override val toString: String = market match {
    case Settlement => "Italian settlement"
    case Exchange => "Milan stock exchange"
  }

  override def considerBusinessDay[D: DateOps](date: D): Boolean = ???
}