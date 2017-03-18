package org.quantlib.time.calendars

import org.quantlib.time.calendars.SouthKorea.Market
import org.quantlib.time.calendars.SouthKorea.Market._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object SouthKorea {

  sealed trait Market

  object Market {

    case object KRX extends Market

    case object Settlement extends Market

  }

}

final case class SouthKorea[D: DateOps](market: Market = KRX) extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._

  private def isIndependenceDay(date: D) = date.dom == 1 && date.inMarch

  private def isArbourDay(date: D) = date.dom == 5 && date.inApril && date.year <= 2005

  private def isChildrensDay(date: D) = date.dom == 5 && date.inMay

  private def isMemorialDay(date: D) = date.dom == 6 && date.inJune

  private def isConstitutionDay(date: D) = date.dom == 17 && date.inJuly && date.year <= 2007

  private def isLiberationDay(date: D) = date.dom == 15 && date.inAugust

  private def isNationalFoundationDay(date: D) = date.dom == 3 && date.inOctober

  private def isHangulProclamationofKorea(date: D) = date.dom == 9 && date.inOctober && date.year >= 2013

  import java.time.Month._

  private def isLunarNewYear(date: D) = {
    val (y, m, d) = date.YMD
    ((d == 21 || d == 22 || d == 23) && m == JANUARY && y === 2004) ||
      ((d == 8 || d == 9 || d == 10) && m == FEBRUARY && y === 2005) ||
      ((d == 28 || d == 29 || d == 30) && m == JANUARY && y === 2006) ||
      (d == 19 && m == FEBRUARY && y === 2007) ||
      ((d == 6 || d == 7 || d == 8) && m == FEBRUARY && y === 2008) ||
      ((d == 25 || d == 26 || d == 27) && m == JANUARY && y === 2009) ||
      ((d == 13 || d == 14 || d == 15) && m == FEBRUARY && y === 2010) ||
      ((d == 2 || d == 3 || d == 4) && m == FEBRUARY && y === 2011) ||
      ((d == 23 || d == 24) && m == JANUARY && y === 2012) ||
      (d == 11 && m == FEBRUARY && y === 2013) ||
      ((d == 30 || d == 31) && m == JANUARY && y === 2014) ||
      ((d == 18 || d == 19 || d == 20) && m == FEBRUARY && y === 2015) ||
      ((d >= 7 && d <= 10) && m == FEBRUARY && y === 2016) ||
      ((d == 27 || d == 28 || d == 29) && m == JANUARY && y === 2017) ||
      ((d == 15 || d == 16 || d == 17) && m == FEBRUARY && y === 2018) ||
      ((d == 4 || d == 5 || d == 6) && m == FEBRUARY && y === 2019) ||
      ((d == 24 || d == 25 || d == 26) && m == JANUARY && y === 2020) ||
      ((d == 11 || d == 12 || d == 13) && m == FEBRUARY && y === 2021) ||
      (((d == 31 && m == JANUARY) || ((d == 1 || d == 2) && m == FEBRUARY)) && y === 2022) ||
      ((d == 21 || d == 22 || d == 23) && m == JANUARY && y === 2023) ||
      ((d == 9 || d == 10 || d == 11) && m == FEBRUARY && y === 2024) ||
      ((d == 28 || d == 29 || d == 30) && m == JANUARY && y === 2025) ||
      ((d == 28 || d == 29 || d == 30) && m == JANUARY && y === 2025) ||
      ((d == 16 || d == 17 || d == 18) && m == FEBRUARY && y === 2026) ||
      ((d == 5 || d == 6 || d == 7) && m == FEBRUARY && y === 2027) ||
      ((d == 25 || d == 26 || d == 27) && m == JANUARY && y === 2028) ||
      ((d == 12 || d == 13 || d == 14) && m == FEBRUARY && y === 2029) ||
      ((d == 2 || d == 3 || d == 4) && m == FEBRUARY && y === 2030) ||
      ((d == 22 || d == 23 || d == 24) && m == JANUARY && y === 2031) ||
      ((d == 10 || d == 11 || d == 12) && m == FEBRUARY && y === 2032)
  }

  private def isElectionDay(date: D) = {
    val (y, m, d) = date.YMD
    (d == 15 && m == APRIL && y === 2004) || // National Assembly
      (d == 31 && m == MAY && y === 2006) || // Regional election
      (d == 19 && m == DECEMBER && y === 2007) || // Presidency
      (d == 9 && m == APRIL && y === 2008) || // National Assembly
      (d == 2 && m == JUNE && y === 2010) || // Local election
      (d == 11 && m == APRIL && y === 2012) || // National Assembly
      (d == 19 && m == DECEMBER && y === 2012) || // Presidency
      (d == 4 && m == JUNE && y === 2014) || // Local election
      (d == 13 && m == APRIL && y === 2016) // National Assembly
  }

  private def isBuddhaBirthDay(date: D) = {
    val (y, m, d) = date.YMD
    (d == 15 && m == APRIL && y === 2004) || // National Assembly
      (d == 31 && m == MAY && y === 2006) || // Regional election
      (d == 19 && m == DECEMBER && y === 2007) || // Presidency
      (d == 9 && m == APRIL && y === 2008) || // National Assembly
      (d == 2 && m == JUNE && y === 2010) || // Local election
      (d == 11 && m == APRIL && y === 2012) || // National Assembly
      (d == 19 && m == DECEMBER && y === 2012) || // Presidency
      (d == 4 && m == JUNE && y === 2014) || // Local election
      (d == 13 && m == APRIL && y === 2016) // National Assembly
  }


  // Harvest Moon Day
  private def isHarvestMoonDay(date: D) = {
    val (y, m, d) = date.YMD
    ((d == 27 || d == 28 || d == 29) && m == SEPTEMBER && y === 2004) ||
      ((d == 17 || d == 18 || d == 19) && m == SEPTEMBER && y === 2005) ||
      ((d == 5 || d == 6 || d == 7) && m == OCTOBER && y === 2006) ||
      ((d == 24 || d == 25 || d == 26) && m == SEPTEMBER && y === 2007) ||
      ((d == 13 || d == 14 || d == 15) && m == SEPTEMBER && y === 2008) ||
      ((d == 2 || d == 3 || d == 4) && m == OCTOBER && y === 2009) ||
      ((d == 21 || d == 22 || d == 23) && m == SEPTEMBER && y === 2010) ||
      ((d == 12 || d == 13) && m == SEPTEMBER && y === 2011) ||
      ((d == 1) && m == OCTOBER && y === 2012) ||
      ((d == 18 || d == 19 || d == 20) && m == SEPTEMBER && y === 2013) ||
      ((d == 8 || d == 9 || d == 10) && m == SEPTEMBER && y === 2014) ||
      ((d == 28 || d == 29) && m == SEPTEMBER && y === 2015) ||
      ((d == 14 || d == 15 || d == 16) && m == SEPTEMBER && y === 2016) ||
      ((d == 3 || d == 4 || d == 5) && m == OCTOBER && y === 2017) ||
      ((d == 23 || d == 24 || d == 25) && m == SEPTEMBER && y === 2018) ||
      ((d == 12 || d == 13 || d == 14) && m == SEPTEMBER && y === 2019) ||
      (((d == 30 && m == SEPTEMBER) || ((d == 1 || d == 2) && m == OCTOBER)) && y === 2020) ||
      ((d == 20 || d == 21 || d == 22) && m == SEPTEMBER && y === 2021) ||
      ((d == 9 || d == 10 || d == 11) && m == SEPTEMBER && y === 2022) ||
      ((d == 28 || d == 29 || d == 30) && m == SEPTEMBER && y === 2023) ||
      ((d == 16 || d == 17 || d == 18) && m == SEPTEMBER && y === 2024) ||
      ((d == 5 || d == 6 || d == 7) && m == OCTOBER && y === 2025) ||
      ((d == 24 || d == 25 || d == 26) && m == SEPTEMBER && y === 2026) ||
      ((d == 14 || d == 15 || d == 16) && m == SEPTEMBER && y === 2027) ||
      ((d == 2 || d == 3 || d == 4) && m == OCTOBER && y === 2028) ||
      ((d == 21 || d == 22 || d == 23) && m == SEPTEMBER && y === 2029) ||
      ((d == 11 || d == 12 || d == 13) && m == SEPTEMBER && y === 2030) ||
      (((d == 30 && m == SEPTEMBER) || ((d == 1 || d == 2) && m == OCTOBER)) && y === 2031) ||
      ((d == 18 || d == 19 || d == 20) && m == SEPTEMBER && y === 2032)
  }

  private def isSpecialHoliday(date: D) = {
    //70 years from Independence Day
    date.dom == 14 && date.inAugust && date.year === 2015
  }

  private val holidays = List[D => Boolean](
    isWeekend,
    isNewYear,
    isLabourDay,
    isIndependenceDay,
    isArbourDay,
    isChildrensDay,
    isMemorialDay,
    isConstitutionDay,
    isLiberationDay,
    isNationalFoundationDay,
    isSpecialHoliday,
    isHangulProclamationofKorea,
    isLunarNewYear,
    isElectionDay,
    isBuddhaBirthDay,
    isHarvestMoonDay,
    isChristmas
  )

  private val krxHolidays = holidays ++ List[D => Boolean](
    isYearEndClosing,
    isOccasionalClosing
  )

  private def isYearEndClosing(date: D) = {
    val d = date.dom
    (((d == 29 || d == 30) && date.dow.isFriday) || d == 31) && date.inDecember
  }

  private def isOccasionalClosing(date: D) = date.dom == 6 && date.inMay && date.year >= 2016


  override val toString: String = market match {
    case KRX => "South-Korea Exchange"
    case Settlement => "South-Korean Settlement"
  }

  override def considerBusinessDay(date: D): Boolean = market match {
    case KRX => !krxHolidays.exists(f => f(date))
    case Settlement => !holidays.exists(f => f(date))
  }
}