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

final case  class Iceland[D:DateOps](market: Market = ICEX) extends WeekendSatSun[D] with BusinessCalendar[D]{

  override val toString: String = "Iceland stock exchange"

  override def considerBusinessDay(date: D): Boolean = ???
}