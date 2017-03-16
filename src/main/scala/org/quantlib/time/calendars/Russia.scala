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

case class Russia[D: DateOps](market: Market = Settlement) extends WeekendSatSun[D] with BusinessCalendar[D]{
  override def considerBusinessDay(date: D): Boolean = ???
}
