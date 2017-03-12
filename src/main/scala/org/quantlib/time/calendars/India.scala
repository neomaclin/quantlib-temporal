package org.quantlib.time.calendars


import org.quantlib.time.calendars.India.Market
import org.quantlib.time.calendars.India.Market.NSE
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */


object India {
  sealed trait Market
  object Market{
    case object NSE extends Market
  }
}

final case  class India(market: Market = NSE) extends BusinessCalendar with WeekendSatSun{

  override val toString: String = "National Stock Exchange of India"

  override def considerBusinessDay[D: DateOps](date: D): Boolean = ???
}