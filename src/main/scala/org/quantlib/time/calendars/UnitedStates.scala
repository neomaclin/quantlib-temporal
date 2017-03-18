package org.quantlib.time.calendars


import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.UnitedStates.Market
import org.quantlib.time.calendars.UnitedStates.Market._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

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


final case class UnitedStates[D: DateOps](market: Market = Settlement) extends WeekendSatSun[D] with BusinessCalendar[D] {


  private def isWashingtonBirthday(date: D) = {
    val dom = date.dom
    val dow = date.dow
    if (date.year >= 1971) {
      // third Monday in February
      (dom >= 15 && dom <= 21) && dow.isMonday && date.inFebruary
    } else {
      // February 22nd, possily adjusted
      (dom == 22 || (dom == 23 && dow.isMonday)
        || (dom == 21 && dow.isFriday)) && date.inFebruary
    }
  }

  private def isMemorialDay(date: D) = {
    val dom = date.dom
    val dow = date.dow
    if (date.year >= 1971) {
      // last Monday in May
      dom >= 25 && dow.isMonday && date.inMay
    } else {
      // May 30th, possibly adjusted
      (dom == 30 || (dom == 31 && dow.isMonday) || (dom == 29 && dow.isFriday)) && date.inMay
    }
  }

  // first Monday in September
  private def isLaborDay(date: D) = date.dom <= 7 && date.dow.isMonday && date.inSeptember


  private def isColumbusDay(date: D) = {
    // second Monday in October
    val dom = date.dom
    val dow = date.dow
    (dom >= 8 && dom <= 14) && dow.isMonday && date.inOctober && date.year >= 1971
  }

  private def isVeteransDay(date: D) = {
    val y = date.year
    val dom = date.dom
    val dow = date.dow
    if (y <= 1970 || y >= 1978) {
      // November 11th, adjusted
      (dom == 11 || (dom == 12 && dow.isMonday) || (dom == 10 && dow.isFriday)) && date.inNovember
    } else {
      // fourth Monday in October
      (dom >= 22 && dom <= 28) && dow.isMonday && date.inOctober
    }
  }

  private def isUSNewYear(date: D) = {
    val dom = date.dom
    val dow = date.dow
    (dom == 1 || (dom == 2 && dow.isMonday)) && date.inJanuary
  }

  private def isNewYearEve(date: D) = {
    val dom = date.dom
    val dow = date.dow
    dom == 31 && dow.isFriday && date.inDecember
  }

  private def isLutherKingBirthday(date: D) = {
    val dom = date.dom
    val dow = date.dow
    (dom >= 15 && dom <= 21) && dow.isMonday && date.inJanuary && date.year >= 1983
  }

  private def isIndependenceDay(date: D) = {
    val dom = date.dom
    val dow = date.dow
    (dom == 4 || (dom == 5 && dow.isMonday) || (dom == 3 && dow.isFriday)) && date.inJuly
  }

  private def isPresidentialElectionDay(date: D) = {
    val (y, m, dd) = date.YMD
    val dow = date.dow

    (y <= 1968 || (y <= 1980 && y.getValue % 4 == 0)) && date.inNovember && dd <= 7 && dow.isTuesDay
  }

  private def isThanksgivingDay(date: D) = {
    val dd = date.dom
    (dd >= 22 && dd <= 28) && date.dow.isThursday && date.inNovember
  }

  private def isUSChristmas(date: D) = {
    val (y, m, dd) = date.YMD
    val dow = date.dow

    (dd == 25 || (dd == 26 && dow.isMonday) || (dd == 24 && dow.isFriday)) && date.inDecember
  }


  private def isNERCChristmas(date: D) = {
    val (y, m, dd) = date.YMD
    val dow = date.dow

    (dd == 25 || (dd == 26 && dow.isMonday)) && date.inDecember
  }



  import java.time.Month._

  private def isSpecialClosings(date: D) = {
    val (y, m, d) = date.YMD
    val dow = date.dow

    (y === 2012 && m == OCTOBER && (d == 29 || d == 30)) ||
      // PRESIDENT FORD'S FUNERAL
      (y === 2007 && m == JANUARY && d == 2) ||
      // PRESIDENT REAGAN'S FUNERAL
      (y === 2004 && m == JUNE && d == 11) ||
      // SEPTEMBER 11-14, 2001
      (y === 2001 && m == SEPTEMBER && (11 <= d && d <= 14)) ||
      // PRESIDENT NIXON'S FUNERAL
      (y === 1994 && m == APRIL && d == 27) ||
      // HURRICANE GLORIA
      (y === 1985 && m == SEPTEMBER && d == 27) ||
      // 1977 BLACKOUT
      (y === 1977 && m == JULY && d == 14) ||
      // FUNERAL OF FORMER PRESIDENT LYNDON B. JOHNSON.
      (y === 1973 && m == JANUARY && d == 25) ||
      // FUNERAL OF FORMER PRESIDENT HARRY S. TRUMAN
      (y === 1972 && m == DECEMBER && d == 28) ||
      // NATIONAL DAY OF PARTICIPATION FOR THE LUNAR EXPLORATION.
      (y === 1969 && m == JULY && d == 21) ||
      // FUNERAL OF FORMER PRESIDENT EISENHOWER.
      (y === 1969 && m == MARCH && d == 31) ||
      // CLOSED ALL DAY - HEAVY SNOW.
      (y === 1969 && m == FEBRUARY && d == 10) ||
      // DAY AFTER INDEPENDENCE DAY.
      (y === 1968 && m == JULY && d == 5) ||
      // JUNE 12-DEC. 31, 1968
      // FOUR DAY WEEK (CLOSED ON WEDNESDAYS) - PAPERWORK CRISIS
      (y === 1968 && date.doy >= 163 && date.dow.isWednesday) ||
      // DAY OF MOURNING FOR MARTIN LUTHER KING JR.
      (y === 1968 && m == APRIL && d == 9) ||
      // FUNERAL OF PRESIDENT KENNEDY
      (y === 1963 && m == NOVEMBER && d == 25) ||
      // DAY BEFORE DECORATION DAY
      (y === 1961 && m == MAY && d == 29) ||
      // DAY AFTER CHRISTMAS
      (y === 1958 && m == DECEMBER && d == 26) ||
      // CHRISTMAS EVE
      ((y === 1954 || y === 1956 || y === 1965) && m == DECEMBER && d == 24)
  }


  override val toString: String = market match {
    case NYSE => "New York stock exchange"

    case Settlement => "US settlement"

    case GovernmentBond => "US government bond market"

    case NERC => "North American Energy Reliability Council"

    case LiborImpact => "US with Libor impact"
  }

  private val NYSEHolidays =
    List[D => Boolean](
      isWeekend,
      isUSNewYear,
      isLutherKingBirthday,
      isWashingtonBirthday,
      isMemorialDay,
      Western.isGoodFriday,
      isIndependenceDay,
      isLaborDay,
      isThanksgivingDay,
      isUSChristmas,
      isPresidentialElectionDay,
      isSpecialClosings)

  private val settlementHolidays =
    List[D => Boolean](
      isWeekend,
      isUSNewYear,
      isNewYearEve,
      isLutherKingBirthday,
      isWashingtonBirthday,
      isMemorialDay,
      isIndependenceDay,
      isLaborDay,
      isColumbusDay,
      isVeteransDay,
      isThanksgivingDay,
      isUSChristmas
    )
  private val governmentBondHolidays =
    List[D => Boolean](
      isWeekend,
      isUSNewYear,
      isLutherKingBirthday,
      isWashingtonBirthday,
      Western.isGoodFriday,
      isMemorialDay,
      isIndependenceDay,
      isLaborDay,
      isColumbusDay,
      isVeteransDay,
      isThanksgivingDay,
      isUSChristmas
    )

  private val NERCHolidays =
    List[D => Boolean](
      isWeekend,
      isUSNewYear,
      isMemorialDay,
      isIndependenceDay,
      isLaborDay,
      isThanksgivingDay,
      isNERCChristmas
    )

  private def laborImpact(date: D) = {
    val d = date.dom
    val w = date.dow
    ((d == 5 && w.isMonday) || (d == 3 && w.isFriday)) && date.inJuly && date.year >= 2015
  }


  override def considerBusinessDay(date: D): Boolean = market match {
    case NYSE => !NYSEHolidays.exists(f=>f(date))

    case Settlement => ! settlementHolidays.exists(f=>f(date))

    case GovernmentBond => ! governmentBondHolidays.exists(f=>f(date))

    case NERC => !NERCHolidays.exists(f=>f(date))

    case LiborImpact => laborImpact(date) || !settlementHolidays.exists(f=>f(date))
  }

}
