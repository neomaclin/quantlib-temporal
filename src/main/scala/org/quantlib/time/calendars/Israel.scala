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

final case class Israel[D: DateOps](market: Market = Settlement) extends WeekendFriSat[D] with BusinessCalendar[D]{
    override val toString: String = market match {
      case Settlement => ""
      case TASE => "Tel Aviv stock exchange"
    }

  override def considerBusinessDay(date: D): Boolean = ???
}
