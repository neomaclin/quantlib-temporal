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

final case class SaudiArabia[D: DateOps](market: Market = Tadawul) extends WeekendFriSat[D] with BusinessCalendar[D]{
  override def considerBusinessDay(date: D): Boolean = ???
}