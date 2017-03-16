package org.quantlib.time.calendars

import org.quantlib.time.calendars.HongKong.Market
import org.quantlib.time.calendars.HongKong.Market.HKEx
import org.quantlib.time.implicits.DateOps
/**
  * Created by neo on 11/03/2017.
  */
object HongKong {

  sealed trait Market

  object Market {

    case object HKEx extends Market

  }

}

final case class HongKong[D: DateOps](market: Market = HKEx) extends WeekendSatSun[D] with BusinessCalendar[D] {

  override val toString: String = market match {
    case HKEx => "Hong Kong stock exchange"
  }

  override def considerBusinessDay(date: D): Boolean = ???
}

