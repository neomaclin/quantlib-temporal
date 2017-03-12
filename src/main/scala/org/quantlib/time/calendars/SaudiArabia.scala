package org.quantlib.time.calendars

import org.quantlib.time.calendars.SaudiArabia.Market
import org.quantlib.time.calendars.SaudiArabia.Market.Tadawul
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
object SaudiArabia {
  sealed trait Market
  object Market{
    case object Tadawul extends Market
  }
}

final case class SaudiArabia(market: Market = Tadawul) extends BusinessCalendar with WeekendFriSat{
  override def considerBusinessDay[D: DateOps](date: D): Boolean = ???
}