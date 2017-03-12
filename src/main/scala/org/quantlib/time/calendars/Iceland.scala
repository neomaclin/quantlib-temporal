package org.quantlib.time.calendars


import org.quantlib.time.calendars.Iceland.Market
import org.quantlib.time.calendars.Iceland.Market._
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
object Iceland {
  sealed trait Market
  object Market{
    case object ICEX extends Market
  }
}

final case  class Iceland(market: Market = ICEX) extends BusinessCalendar with WeekendSatSun{

  override val toString: String = "Iceland stock exchange"

  override def considerBusinessDay[D:DateOps](date: D): Boolean = ???
}