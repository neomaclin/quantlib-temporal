package org.quantlib.time.calendars

import org.quantlib.time.calendars.Israel.Market
import org.quantlib.time.calendars.Israel.Market.{Settlement, TASE}
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
object Israel {
  sealed trait Market
  object Market{
    case object Settlement extends Market
    case object TASE extends Market
  }
}

final case class Israel(market: Market = Settlement) extends BusinessCalendar with WeekendFriSat{
    override val toString: String = market match {
      case Settlement => ""
      case TASE => "Tel Aviv stock exchange"
    }

  override def considerBusinessDay[D: DateOps](date: D): Boolean = ???
}
