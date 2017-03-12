package org.quantlib.time.calendars

import org.quantlib.time.calendars.Russia.Market
import org.quantlib.time.calendars.Russia.Market._
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
object Russia{
  sealed trait Market
  object Market{
    case object Settlement extends Market
    case object MOEX extends Market
  }
}

case class Russia(market: Market = Settlement) extends BusinessCalendar with WeekendSatSun{
  override def considerBusinessDay[D: DateOps](date: D): Boolean = ???
}
