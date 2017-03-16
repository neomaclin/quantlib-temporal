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

final case  class India[D: DateOps](market: Market = NSE) extends WeekendSatSun[D] with BusinessCalendar[D]{

  override val toString: String = "National Stock Exchange of India"

  override def considerBusinessDay(date: D): Boolean = ???
}