package org.quantlib.time.calendars

import org.quantlib.time.calendars.Israel.Market
import org.quantlib.time.calendars.Israel.Market.{Settlement, TASE}
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object Israel {

  sealed trait Market

  object Market {

    case object Settlement extends Market

    case object TASE extends Market

  }

}

final case class Israel[D: DateOps](market: Market = Settlement) extends WeekendFriSat[D] with BusinessCalendar[D] {
  override val toString: String = market match {
    case Settlement => ""
    case TASE => "Tel Aviv stock exchange"
  }

  import java.time.Month._

  private def isPurim(date: D) = {
    val (y, m, dom) = date.YMD
    val year = y.getValue
    (dom == 24 && m == FEBRUARY && year == 2013) ||
      (dom == 16 && m == MARCH && year == 2014) ||
      (dom == 5 && m == MARCH && year == 2015) ||
      (dom == 24 && m == MARCH && year == 2016) ||
      (dom == 12 && m == MARCH && year == 2017) ||
      (dom == 1 && m == MARCH && year == 2018) ||
      (dom == 21 && m == MARCH && year == 2019) ||
      (dom == 10 && m == MARCH && year == 2020) ||
      (dom == 26 && m == FEBRUARY && year == 2021) ||
      (dom == 17 && m == MARCH && year == 2022) ||
      (dom == 7 && m == MARCH && year == 2023) ||
      (dom == 24 && m == MARCH && year == 2024) ||
      (dom == 14 && m == MARCH && year == 2025) ||
      (dom == 3 && m == MARCH && year == 2026) ||
      (dom == 23 && m == MARCH && year == 2027) ||
      (dom == 12 && m == MARCH && year == 2028) ||
      (dom == 1 && m == MARCH && year == 2029) ||
      (dom == 19 && m == MARCH && year == 2030) ||
      (dom == 9 && m == MARCH && year == 2031) ||
      (dom == 26 && m == FEBRUARY && year == 2032) ||
      (dom == 15 && m == MARCH && year == 2033) ||
      (dom == 5 && m == MARCH && year == 2034) ||
      (dom == 25 && m == MARCH && year == 2035) ||
      (dom == 13 && m == MARCH && year == 2036) ||
      (dom == 1 && m == MARCH && year == 2037) ||
      (dom == 21 && m == MARCH && year == 2038) ||
      (dom == 10 && m == MARCH && year == 2039) ||
      (dom == 28 && m == FEBRUARY && year == 2040) ||
      (dom == 17 && m == MARCH && year == 2041) ||
      (dom == 6 && m == MARCH && year == 2042) ||
      (dom == 26 && m == MARCH && year == 2043) ||
      (dom == 13 && m == MARCH && year == 2044)
  }

  private def isPassoverIandII(date: D) = {
    val (y, m, dom) = date.YMD
    val year = y.getValue
    ((((dom == 25 || dom == 26 || dom == 31) && m == MARCH) || (dom == 1 && m == APRIL)) && year == 2013) ||
      ((dom == 14 || dom == 15 || dom == 20 || dom == 21) && m == APRIL && year == 2014) ||
      ((dom == 3 || dom == 4 || dom == 9 || dom == 10) && m == APRIL && year == 2015) ||
      ((dom == 22 || dom == 23 || dom == 28 || dom == 29) && m == APRIL && year == 2016) ||
      ((dom == 10 || dom == 11 || dom == 16 || dom == 17) && m == APRIL && year == 2017) ||
      (((dom == 31 && m == MARCH) || ((dom == 5 || dom == 6) && m == APRIL)) && year == 2018) ||
      ((dom == 20 || dom == 25 || dom == 26) && m == APRIL && year == 2019) ||
      ((dom == 8 || dom == 9 || dom == 14 || dom == 15) && m == APRIL && year == 2020) ||
      (((dom == 28 && m == MARCH) || (dom == 3 && m == APRIL)) && year == 2021) ||
      ((dom == 16 || dom == 22) && m == APRIL && year == 2022) ||
      ((dom == 6 || dom == 12) && m == APRIL && year == 2023) ||
      ((dom == 23 || dom == 29) && m == APRIL && year == 2024) ||
      ((dom == 13 || dom == 19) && m == APRIL && year == 2025) ||
      ((dom == 2 || dom == 8) && m == APRIL && year == 2026) ||
      ((dom == 22 || dom == 28) && m == APRIL && year == 2027) ||
      ((dom == 11 || dom == 17) && m == APRIL && year == 2028) ||
      (((dom == 31 && m == MARCH) || (dom == 6 && m == APRIL)) && year == 2029) ||
      ((dom == 18 || dom == 24) && m == APRIL && year == 2030) ||
      ((dom == 8 || dom == 14) && m == APRIL && year == 2031) ||
      (((dom == 27 && m == MARCH) || (dom == 2 && m == APRIL)) && year == 2032) ||
      ((dom == 14 || dom == 20) && m == APRIL && year == 2033) ||
      ((dom == 4 || dom == 10) && m == APRIL && year == 2034) ||
      ((dom == 24 || dom == 30) && m == APRIL && year == 2035) ||
      ((dom == 12 || dom == 18) && m == APRIL && year == 2036) ||
      (((dom == 31 && m == MARCH) || (dom == 6 && m == APRIL)) && year == 2037) ||
      ((dom == 20 || dom == 26) && m == APRIL && year == 2038) ||
      ((dom == 9 || dom == 15) && m == APRIL && year == 2039) ||
      (((dom == 29 && m == MARCH) || (dom == 4 && m == APRIL)) && year == 2040) ||
      ((dom == 16 || dom == 22) && m == APRIL && year == 2041) ||
      ((dom == 5 || dom == 11) && m == APRIL && year == 2042) ||
      (((dom == 25 && m == APRIL) || (dom == 1 && m == MAY)) && year == 2043) ||
      ((dom == 12 || dom == 18) && m == APRIL && year == 2044)
  }

  private def isMemorialAndIndipendenceDay(date: D) = {
    val (y, m, dom) = date.YMD
    val year = y.getValue
    ((dom == 15 || dom == 16) && m == APRIL && year == 2013) ||
      ((dom == 5 || dom == 6) && m == MAY && year == 2014) ||
      ((dom == 22 || dom == 23) && m == APRIL && year == 2015) ||
      ((dom == 11 || dom == 12) && m == MAY && year == 2016) ||
      ((dom == 1 || dom == 2) && m == MAY && year == 2017) ||
      ((dom == 18 || dom == 19) && m == APRIL && year == 2018) ||
      ((dom == 8 || dom == 9) && m == MAY && year == 2019) ||
      ((dom == 28 || dom == 29) && m == APRIL && year == 2020) ||
      ((dom == 14 || dom == 15) && m == APRIL && year == 2021) ||
      ((dom == 4 || dom == 5) && m == MAY && year == 2022) ||
      ((dom == 25 || dom == 26) && m == APRIL && year == 2023) ||
      ((dom == 13 || dom == 14) && m == MAY && year == 2024) ||
      (((dom == 30 && m == APRIL) || (dom == 1 && m == MAY)) && year == 2025) ||
      ((dom == 21 || dom == 22) && m == APRIL && year == 2026) ||
      ((dom == 11 || dom == 12) && m == MAY && year == 2027) ||
      ((dom == 1 || dom == 2) && m == MAY && year == 2028) ||
      ((dom == 18 || dom == 19) && m == APRIL && year == 2029) ||
      ((dom == 7 || dom == 8) && m == MAY && year == 2030) ||
      ((dom == 28 || dom == 29) && m == APRIL && year == 2031) ||
      ((dom == 14 || dom == 15) && m == APRIL && year == 2032) ||
      ((dom == 3 || dom == 4) && m == MAY && year == 2033) ||
      ((dom == 24 || dom == 25) && m == APRIL && year == 2034) ||
      ((dom == 14 || dom == 15) && m == MAY && year == 2035) ||
      (((dom == 30 && m == APRIL) || (dom == 1 && m == MAY)) && year == 2036) ||
      ((dom == 20 || dom == 21) && m == APRIL && year == 2037) ||
      ((dom == 9 || dom == 10) && m == MAY && year == 2038) ||
      ((dom == 27 || dom == 28) && m == APRIL && year == 2039) ||
      ((dom == 17 || dom == 18) && m == APRIL && year == 2040) ||
      ((dom == 6 || dom == 7) && m == MAY && year == 2041) ||
      ((dom == 23 || dom == 24) && m == APRIL && year == 2042) ||
      ((dom == 13 || dom == 14) && m == MAY && year == 2043) ||
      ((dom == 2 || dom == 3) && m == MAY && year == 2044)
  }

  private def isPentecost(date: D) = {
    val (y, m, dom) = date.YMD
    val year = y.getValue
    ((dom == 14 || dom == 15) && m == MAY && year == 2013) ||
      ((dom == 3 || dom == 4) && m == JUNE && year == 2014) ||
      ((dom == 23 || dom == 24) && m == MAY && year == 2015) ||
      ((dom == 11 || dom == 12) && m == JUNE && year == 2016) ||
      ((dom == 30 || dom == 31) && m == MAY && year == 2017) ||
      ((dom == 19 || dom == 20) && m == MAY && year == 2018) ||
      ((dom == 8 || dom == 9) && m == JUNE && year == 2019) ||
      ((dom == 28 || dom == 29) && m == MAY && year == 2020) ||
      (dom == 17 && m == MAY && year == 2021) ||
      (dom == 5 && m == JUNE && year == 2022) ||
      (dom == 26 && m == MAY && year == 2023) ||
      (dom == 12 && m == JUNE && year == 2024) ||
      (dom == 2 && m == JUNE && year == 2025) ||
      (dom == 22 && m == MAY && year == 2026) ||
      (dom == 11 && m == JUNE && year == 2027) ||
      (dom == 31 && m == MAY && year == 2028) ||
      (dom == 20 && m == MAY && year == 2029) ||
      (dom == 7 && m == JUNE && year == 2030) ||
      (dom == 28 && m == MAY && year == 2031) ||
      (dom == 16 && m == MAY && year == 2032) ||
      (dom == 3 && m == JUNE && year == 2033) ||
      (dom == 24 && m == MAY && year == 2034) ||
      (dom == 13 && m == JUNE && year == 2035) ||
      (dom == 1 && m == JUNE && year == 2036) ||
      (dom == 20 && m == MAY && year == 2037) ||
      (dom == 9 && m == JUNE && year == 2038) ||
      (dom == 29 && m == MAY && year == 2039) ||
      (dom == 18 && m == MAY && year == 2040) ||
      (dom == 5 && m == JUNE && year == 2041) ||
      (dom == 25 && m == MAY && year == 2042) ||
      (dom == 14 && m == JUNE && year == 2043) ||
      (dom == 1 && m == JUNE && year == 2044)

  }

  private def isFastDay(date: D) = {
    val (y, m, dom) = date.YMD
    val year = y.getValue
    (dom == 16 && m == JULY && year == 2013) ||
      (dom == 5 && m == AUGUST && year == 2014) ||
      (dom == 26 && m == JULY && year == 2015) ||
      (dom == 14 && m == AUGUST && year == 2016) ||
      (dom == 1 && m == AUGUST && year == 2017) ||
      (dom == 22 && m == JULY && year == 2018) ||
      (dom == 11 && m == AUGUST && year == 2019) ||
      (dom == 30 && m == JULY && year == 2020) ||
      (dom == 18 && m == JULY && year == 2021) ||
      (dom == 7 && m == AUGUST && year == 2022) ||
      (dom == 27 && m == JULY && year == 2023) ||
      (dom == 13 && m == AUGUST && year == 2024) ||
      (dom == 3 && m == AUGUST && year == 2025) ||
      (dom == 23 && m == JULY && year == 2026) ||
      (dom == 12 && m == AUGUST && year == 2027) ||
      (dom == 1 && m == AUGUST && year == 2028) ||
      (dom == 22 && m == JULY && year == 2029) ||
      (dom == 8 && m == AUGUST && year == 2030) ||
      (dom == 29 && m == JULY && year == 2031) ||
      (dom == 18 && m == JULY && year == 2032) ||
      (dom == 4 && m == AUGUST && year == 2033) ||
      (dom == 25 && m == JULY && year == 2034) ||
      (dom == 14 && m == AUGUST && year == 2035) ||
      (dom == 3 && m == AUGUST && year == 2036) ||
      (dom == 21 && m == JULY && year == 2037) ||
      (dom == 10 && m == AUGUST && year == 2038) ||
      (dom == 31 && m == JULY && year == 2039) ||
      (dom == 19 && m == JULY && year == 2040) ||
      (dom == 6 && m == AUGUST && year == 2041) ||
      (dom == 27 && m == JULY && year == 2042) ||
      (dom == 16 && m == AUGUST && year == 2043) ||
      (dom == 2 && m == AUGUST && year == 2044)
  }

  private def isJewishNewYear(date: D) = {
    val (y, m, dom) = date.YMD
    val year = y.getValue
    ((dom == 4 || dom == 5 || dom == 6) && m == SEPTEMBER && year == 2013) ||
      ((dom == 24 || dom == 25 || dom == 26) && m == SEPTEMBER && year == 2014) ||
      ((dom == 13 || dom == 14 || dom == 15) && m == SEPTEMBER && year == 2015) ||
      ((dom == 2 || dom == 3 || dom == 4) && m == OCTOBER && year == 2016) ||
      ((dom == 20 || dom == 21 || dom == 22) && m == SEPTEMBER && year == 2017) ||
      ((dom == 9 || dom == 10 || dom == 11) && m == SEPTEMBER && year == 2018) ||
      ((((dom == 29 || dom == 30) && m == SEPTEMBER) || (dom == 1 && m == OCTOBER)) && year == 2019) ||
      ((dom == 19 || dom == 20) && m == SEPTEMBER && year == 2020) ||
      ((dom == 7 || dom == 8) && m == SEPTEMBER && year == 2021) ||
      ((dom == 26 || dom == 27) && m == SEPTEMBER && year == 2022) ||
      ((dom == 16 || dom == 17) && m == SEPTEMBER && year == 2023) ||
      ((dom == 3 || dom == 4) && m == OCTOBER && year == 2024) ||
      ((dom == 23 || dom == 24) && m == SEPTEMBER && year == 2025) ||
      ((dom == 12 || dom == 13) && m == SEPTEMBER && year == 2026) ||
      ((dom == 2 || dom == 3) && m == OCTOBER && year == 2027) ||
      ((dom == 21 || dom == 22) && m == SEPTEMBER && year == 2028) ||
      ((dom == 10 || dom == 11) && m == SEPTEMBER && year == 2029) ||
      ((dom == 28 || dom == 29) && m == SEPTEMBER && year == 2030) ||
      ((dom == 18 || dom == 19) && m == SEPTEMBER && year == 2031) ||
      ((dom == 6 || dom == 7) && m == SEPTEMBER && year == 2032) ||
      ((dom == 24 || dom == 25) && m == SEPTEMBER && year == 2033) ||
      ((dom == 14 || dom == 15) && m == SEPTEMBER && year == 2034) ||
      ((dom == 4 || dom == 5) && m == OCTOBER && year == 2035) ||
      ((dom == 22 || dom == 23) && m == SEPTEMBER && year == 2036) ||
      ((dom == 10 || dom == 11) && m == SEPTEMBER && year == 2037) ||
      (((dom == 30 && m == SEPTEMBER) || (dom == 1 && m == OCTOBER)) && year == 2038) ||
      ((dom == 19 || dom == 20) && m == SEPTEMBER && year == 2039) ||
      ((dom == 8 || dom == 9) && m == SEPTEMBER && year == 2040) ||
      ((dom == 26 || dom == 27) && m == SEPTEMBER && year == 2041) ||
      ((dom == 15 || dom == 16) && m == SEPTEMBER && year == 2042) ||
      ((dom == 5 || dom == 6) && m == OCTOBER && year == 2043) ||
      ((dom == 22 || dom == 23) && m == SEPTEMBER && year == 2044)
  }

  private def isYomKippur(date: D) = {
    val (y, m, dom) = date.YMD
    val year = y.getValue
    ((dom == 13 || dom == 14) && m == SEPTEMBER && year == 2013) ||
      ((dom == 3 || dom == 4) && m == OCTOBER && year == 2014) ||
      ((dom == 22 || dom == 23) && m == SEPTEMBER && year == 2015) ||
      ((dom == 11 || dom == 12) && m == OCTOBER && year == 2016) ||
      ((dom == 29 || dom == 30) && m == SEPTEMBER && year == 2017) ||
      ((dom == 18 || dom == 19) && m == SEPTEMBER && year == 2018) ||
      ((dom == 8 || dom == 9) && m == OCTOBER && year == 2019) ||
      ((dom == 27 || dom == 28) && m == SEPTEMBER && year == 2020) ||
      ((dom == 15 || dom == 16) && m == SEPTEMBER && year == 2021) ||
      ((dom == 4 || dom == 5) && m == OCTOBER && year == 2022) ||
      ((dom == 24 || dom == 25) && m == SEPTEMBER && year == 2023) ||
      ((dom == 11 || dom == 12) && m == OCTOBER && year == 2024) ||
      ((dom == 1 || dom == 2) && m == OCTOBER && year == 2025) ||
      ((dom == 20 || dom == 21) && m == SEPTEMBER && year == 2026) ||
      ((dom == 10 || dom == 11) && m == OCTOBER && year == 2027) ||
      ((dom == 29 || dom == 30) && m == SEPTEMBER && year == 2028) ||
      ((dom == 18 || dom == 19) && m == SEPTEMBER && year == 2029) ||
      ((dom == 6 || dom == 7) && m == OCTOBER && year == 2030) ||
      ((dom == 26 || dom == 27) && m == SEPTEMBER && year == 2031) ||
      ((dom == 14 || dom == 15) && m == SEPTEMBER && year == 2032) ||
      ((dom == 2 || dom == 3) && m == OCTOBER && year == 2033) ||
      ((dom == 22 || dom == 23) && m == SEPTEMBER && year == 2034) ||
      ((dom == 12 || dom == 13) && m == OCTOBER && year == 2035) ||
      (((dom == 30 && m == SEPTEMBER) || (dom == 1 && m == OCTOBER)) && year == 2036) ||
      ((dom == 18 || dom == 19) && m == SEPTEMBER && year == 2037) ||
      ((dom == 8 || dom == 9) && m == OCTOBER && year == 2038) ||
      ((dom == 27 || dom == 28) && m == SEPTEMBER && year == 2039) ||
      ((dom == 16 || dom == 17) && m == SEPTEMBER && year == 2040) ||
      ((dom == 4 || dom == 5) && m == OCTOBER && year == 2041) ||
      ((dom == 23 || dom == 24) && m == SEPTEMBER && year == 2042) ||
      ((dom == 13 || dom == 14) && m == OCTOBER && year == 2043) ||
      (((dom == 30 && m == SEPTEMBER) || (dom == 1 && m == OCTOBER)) && year == 2044)
  }

  private def isSukkoth(date: D) = {
    val (y, m, dom) = date.YMD
    val year = y.getValue
    ((dom == 18 || dom == 19) && m == SEPTEMBER && year == 2013) ||
      ((dom == 8 || dom == 9) && m == OCTOBER && year == 2014) ||
      ((dom == 27 || dom == 28) && m == SEPTEMBER && year == 2015) ||
      ((dom == 16 || dom == 17) && m == OCTOBER && year == 2016) ||
      ((dom == 4 || dom == 5) && m == OCTOBER && year == 2017) ||
      ((dom == 23 || dom == 24) && m == SEPTEMBER && year == 2018) ||
      ((dom == 13 || dom == 14) && m == OCTOBER && year == 2019) ||
      ((dom == 2 || dom == 3) && m == OCTOBER && year == 2020) ||
      ((dom == 20 || dom == 21) && m == SEPTEMBER && year == 2021) ||
      ((dom == 9 || dom == 10) && m == OCTOBER && year == 2022) ||
      ((dom == 29 || dom == 30) && m == SEPTEMBER && year == 2023) ||
      ((dom == 16 || dom == 17) && m == OCTOBER && year == 2024) ||
      ((dom == 6 || dom == 7) && m == OCTOBER && year == 2025) ||
      ((dom == 25 || dom == 26) && m == SEPTEMBER && year == 2026) ||
      ((dom == 15 || dom == 16) && m == OCTOBER && year == 2027) ||
      ((dom == 4 || dom == 5) && m == OCTOBER && year == 2028) ||
      ((dom == 23 || dom == 24) && m == SEPTEMBER && year == 2029) ||
      ((dom == 11 || dom == 12) && m == OCTOBER && year == 2030) ||
      ((dom == 1 || dom == 2) && m == OCTOBER && year == 2031) ||
      ((dom == 19 || dom == 20) && m == SEPTEMBER && year == 2032) ||
      ((dom == 7 || dom == 8) && m == OCTOBER && year == 2033) ||
      ((dom == 27 || dom == 28) && m == SEPTEMBER && year == 2034) ||
      ((dom == 17 || dom == 18) && m == OCTOBER && year == 2035) ||
      ((dom == 5 || dom == 6) && m == OCTOBER && year == 2036) ||
      ((dom == 23 || dom == 24) && m == SEPTEMBER && year == 2037) ||
      ((dom == 13 || dom == 14) && m == OCTOBER && year == 2038) ||
      ((dom == 2 || dom == 3) && m == OCTOBER && year == 2039) ||
      ((dom == 21 || dom == 22) && m == SEPTEMBER && year == 2040) ||
      ((dom == 9 || dom == 10) && m == OCTOBER && year == 2041) ||
      ((dom == 28 || dom == 29) && m == SEPTEMBER && year == 2042) ||
      ((dom == 18 || dom == 19) && m == OCTOBER && year == 2043) ||
      ((dom == 5 || dom == 6) && m == OCTOBER && year == 2044)
  }

  private def isSimchatTora(date: D) = {
    val (y, m, dom) = date.YMD
    val year = y.getValue
    ((dom == 25 || dom == 26) && m == SEPTEMBER && year == 2013) ||
      ((dom == 15 || dom == 16) && m == OCTOBER && year == 2014) ||
      ((dom == 4 || dom == 5) && m == OCTOBER && year == 2015) ||
      ((dom == 23 || dom == 24) && m == OCTOBER && year == 2016) ||
      ((dom == 11 || dom == 12) && m == OCTOBER && year == 2017) ||
      (((dom == 30 && m == SEPTEMBER) || (dom == 1 && m == OCTOBER)) && year == 2018) ||
      ((dom == 20 || dom == 21) && m == OCTOBER && year == 2019) ||
      ((dom == 9 || dom == 10) && m == OCTOBER && year == 2020) ||
      ((dom == 27 || dom == 28) && m == SEPTEMBER && year == 2021) ||
      ((dom == 16 || dom == 17) && m == OCTOBER && year == 2022) ||
      ((dom == 6 || dom == 7) && m == OCTOBER && year == 2023) ||
      ((dom == 23 || dom == 24) && m == OCTOBER && year == 2024) ||
      ((dom == 13 || dom == 14) && m == OCTOBER && year == 2025) ||
      ((dom == 2 || dom == 3) && m == OCTOBER && year == 2026) ||
      ((dom == 22 || dom == 23) && m == OCTOBER && year == 2027) ||
      ((dom == 11 || dom == 12) && m == OCTOBER && year == 2028) ||
      (((dom == 30 && m == SEPTEMBER) || (dom == 1 && m == OCTOBER)) && year == 2029) ||
      ((dom == 18 || dom == 19) && m == OCTOBER && year == 2030) ||
      ((dom == 8 || dom == 9) && m == OCTOBER && year == 2031) ||
      ((dom == 26 || dom == 27) && m == SEPTEMBER && year == 2032) ||
      ((dom == 14 || dom == 15) && m == OCTOBER && year == 2033) ||
      ((dom == 4 || dom == 5) && m == OCTOBER && year == 2034) ||
      ((dom == 24 || dom == 25) && m == OCTOBER && year == 2035) ||
      ((dom == 12 || dom == 13) && m == OCTOBER && year == 2036) ||
      (((dom == 30 && m == SEPTEMBER) || (dom == 1 && m == OCTOBER)) && year == 2037) ||
      ((dom == 20 || dom == 21) && m == OCTOBER && year == 2038) ||
      ((dom == 9 || dom == 10) && m == OCTOBER && year == 2039) ||
      ((dom == 28 || dom == 29) && m == SEPTEMBER && year == 2040) ||
      ((dom == 16 || dom == 17) && m == OCTOBER && year == 2041) ||
      ((dom == 5 || dom == 6) && m == OCTOBER && year == 2042) ||
      ((dom == 25 || dom == 26) && m == OCTOBER && year == 2043) ||
      ((dom == 12 || dom == 13) && m == OCTOBER && year == 2044)
  }

  private val holidays = List[D => Boolean](
    isWeekend,
    isPurim,
    isPassoverIandII,
    isMemorialAndIndipendenceDay,
    isPentecost,
    isFastDay,
    isJewishNewYear,
    isYomKippur,
    isSukkoth,
    isSimchatTora
  )

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
