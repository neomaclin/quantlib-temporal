package org.quantlib.time.calendars

import org.quantlib.time.calendars.Indonesia.Market
import org.quantlib.time.calendars.Indonesia.Market.IDX
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
object Indonesia {

  sealed trait Market
  object Market{
    case object EJ extends Market
    case object JSX extends Market
    case object IDX extends Market
  }

}

final  case class Indonesia(market: Market = IDX) extends  BusinessCalendar with WeekendSatSun{
  override val toString: String = "Jakarta stock exchange"
  override def considerBusinessDay[D: DateOps](date: D): Boolean = ???
}
