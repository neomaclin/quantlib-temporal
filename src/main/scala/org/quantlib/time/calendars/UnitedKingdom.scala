package org.quantlib.time.calendars

import org.quantlib.time.calendars.UnitedKingdom.Market
import org.quantlib.time.calendars.UnitedKingdom.Market._
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
object UnitedKingdom{
  sealed trait Market
  object Market{
    case object Settlement extends Market
    case object Exchange extends Market
    case object Metals extends Market
  }
}
final case class UnitedKingdom(market: Market = Settlement) extends BusinessCalendar with WeekendSatSun{
  override val toString: String = market match {
    case  Settlement => "UK settlement"
    case  Exchange => "London Stock Exchange"
    case  Metals => "London Metals Exchange"
  }
  override def considerBusinessDay[D: DateOps](date: D): Boolean = ???
}
