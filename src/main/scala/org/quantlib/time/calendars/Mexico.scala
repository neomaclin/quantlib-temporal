package org.quantlib.time.calendars

import java.time.{DayOfWeek, Month}

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.Mexico.Market
import org.quantlib.time.calendars.Mexico.Market.BMV
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 18/03/2017.
  */

object Mexico {

  sealed trait Market

  object Market {

    case object BMV extends Market

  }

}

final case class Mexico[D: DateOps](market: Market = BMV) extends WeekendSatSun[D] with BusinessCalendar[D] {
  override val toString: String = "Mexican stock exchange"

  import BusinessCalendar.InternationalHolidays._

  private def isConstitutionDay(date: D) = {
    val (y, _, d) = date.YMD
    (y <= 2005 && d == 5 && date.inFebruary) ||
      (y >= 2006 && d <= 7 && date.dow.isMonday && date.inFebruary)
  }

  private def isBirthdayofBenitoJuarez(date: D) = {
    val (y, _, d) = date.YMD
    (y <= 2005 && d == 21 && date.inMarch) ||
      (y >= 2006 && (d >= 15 && d <= 21) && date.dow.isMonday && date.inMarch)
  }

  private def isNationalDay(date: D) = {
    date.dom == 16 && date.inSeptember
  }

  private def isRevolutionDay(date: D) = {
    val (y, _, d) = date.YMD
    (y <= 2005 && d == 20 && date.inNovember) ||
      (y >= 2006 && (d >= 15 && d <= 21) && date.dow.isMonday && date.inNovember)
  }

  private def isOurLadyofGuadalupeDay(date: D) = date.dom == 12 && date.inDecember

  private val holidays = List[D => Boolean](
    isWeekend,
    isNewYear,
    Western.isHolyThursday,
    Western.isGoodFriday,
    isConstitutionDay,
    isBirthdayofBenitoJuarez,
    isNationalDay,
    isRevolutionDay,
    isOurLadyofGuadalupeDay,
    isLabourDay,
    isChristmas)

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
