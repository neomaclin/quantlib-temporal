package org.quantlib.time.calendars


import org.quantlib.time.calendars.UnitedStates.Market
import org.quantlib.time.calendars.UnitedStates.Market._
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 12/13/15.
  */


object UnitedStates {

  sealed trait Market

  object Market {

    case object NYSE extends Market

    case object Settlement extends Market

    case object GovernmentBond extends Market

    case object NERC extends Market

    case object LiborImpact extends Market

  }

}


final case class UnitedStates(market: Market = Settlement) extends BusinessCalendar with WeekendSatSun {

  override val toString: String = market match {
    case NYSE => "New York stock exchange"

    case Settlement => "US settlement"

    case GovernmentBond => "US government bond market"

    case NERC => "North American Energy Reliability Council"

    case LiborImpact => "US with Libor impact"
  }

  override def considerBusinessDay[D: DateOps](date: D): Boolean = ???
}
