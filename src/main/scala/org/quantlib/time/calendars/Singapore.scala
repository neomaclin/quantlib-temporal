package org.quantlib.time.calendars

import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.Singapore.Market
import org.quantlib.time.calendars.Singapore.Market.SGX
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object Singapore {

  sealed trait Market

  object Market {

    case object SGX extends Market

  }

}

final case class Singapore[D: DateOps](market: Market = SGX) extends WeekendSatSun[D] with BusinessCalendar[D] {

  import BusinessCalendar.InternationalHolidays._

  private def isNationalDay(date: D) = {
    val d = date.dom
    (d == 9 || (d == 10 && date.dow.isMonday)) && date.inAugust
  }

  import java.time.Month._

  private def isChineseNewYear(date: D) = {
    val (y, m, d) = date.YMD
    ((d == 22 || d == 23) && m == JANUARY && y === 2004) ||
      ((d == 9 || d == 10) && m == FEBRUARY && y === 2005) ||
      ((d == 30 || d == 31) && m == JANUARY && y === 2006) ||
      ((d == 19 || d == 20) && m == FEBRUARY && y === 2007) ||
      ((d == 7 || d == 8) && m == FEBRUARY && y === 2008) ||
      ((d == 26 || d == 27) && m == JANUARY && y === 2009) ||
      ((d == 15 || d == 16) && m == JANUARY && y === 2010) ||
      ((d == 23 || d == 24) && m == JANUARY && y === 2012) ||
      ((d == 11 || d == 12) && m == FEBRUARY && y === 2013) ||
      (d == 31 && m == JANUARY && y === 2014) ||
      (d == 1 && m == FEBRUARY && y === 2014) ||
      ((d == 19 || d == 20) && m == FEBRUARY && y === 2015) ||
      ((d == 8 || d == 9) && m == FEBRUARY && y === 2016) ||
      ((d == 28 || d == 29) && m == JANUARY && y === 2017)

  }

  private def isHariRayaHaji(date: D) = {
    val (y, m, d) = date.YMD
    ((d == 1 || d == 2) && m == FEBRUARY && y === 2004) ||
      (d == 21 && m == JANUARY && y === 2005) ||
      (d == 10 && m == JANUARY && y === 2006) ||
      (d == 2 && m == JANUARY && y === 2007) ||
      (d == 20 && m == DECEMBER && y === 2007) ||
      (d == 8 && m == DECEMBER && y === 2008) ||
      (d == 27 && m == NOVEMBER && y === 2009) ||
      (d == 17 && m == NOVEMBER && y === 2010) ||
      (d == 26 && m == OCTOBER && y === 2012) ||
      (d == 15 && m == OCTOBER && y === 2013) ||
      (d == 24 && m == SEPTEMBER && y === 2015) ||
      (d == 12 && m == SEPTEMBER && y === 2016) ||
      (d == 1 && m == SEPTEMBER && y === 2017)
  }

  private def isVesakPoya(date: D) = {
    val (y, m, d) = date.YMD
    (d == 2 && m == JUNE && y === 2004) ||
      (d == 22 && m == MAY && y === 2005) ||
      (d == 12 && m == MAY && y === 2006) ||
      (d == 31 && m == MAY && y === 2007) ||
      (d == 18 && m == MAY && y === 2008) ||
      (d == 9 && m == MAY && y === 2009) ||
      (d == 28 && m == MAY && y === 2010) ||
      (d == 5 && m == MAY && y === 2012) ||
      (d == 24 && m == MAY && y === 2013) ||
      (d == 13 && m == MAY && y === 2014) ||
      (d == 1 && m == JUNE && y === 2015) ||
      (d == 21 && m == MAY && y === 2016) ||
      (d == 10 && m == MAY && y === 2017)
  }

  private def isDeepavali(date: D) = {
    val (y, m, d) = date.YMD
    (d == 11 && m == NOVEMBER && y === 2004) ||
      (d == 8 && m == NOVEMBER && y === 2007) ||
      (d == 28 && m == OCTOBER && y === 2008) ||
      (d == 16 && m == NOVEMBER && y === 2009) ||
      (d == 5 && m == NOVEMBER && y === 2010) ||
      (d == 13 && m == NOVEMBER && y === 2012) ||
      (d == 2 && m == NOVEMBER && y === 2013) ||
      (d == 23 && m == OCTOBER && y === 2014) ||
      (d == 10 && m == NOVEMBER && y === 2015) ||
      (d == 29 && m == OCTOBER && y === 2016) ||
      (d == 18 && m == OCTOBER && y === 2017)
  }

  private def isDiwali(date: D) = date.dom == 1 && date.inNovember && date.year === 2005

  private def isHariRayaPuasa(date: D) = {
    val (y, m, d) = date.YMD
    ((d == 14 || d == 15) && m == NOVEMBER && y === 2004) ||
      (d == 3 && m == NOVEMBER && y === 2005) ||
      (d == 24 && m == OCTOBER && y === 2006) ||
      (d == 13 && m == OCTOBER && y === 2007) ||
      (d == 1 && m == OCTOBER && y === 2008) ||
      (d == 21 && m == SEPTEMBER && y === 2009) ||
      (d == 10 && m == SEPTEMBER && y === 2010) ||
      (d == 20 && m == AUGUST && y === 2012) ||
      (d == 8 && m == AUGUST && y === 2013) ||
      (d == 28 && m == JULY && y === 2014)
  }

  private val holidays = List[D => Boolean](
    isWeekend,
    Western.isGoodFriday,
    isLabourDay,
    isChristmas,
    isNationalDay,
    isChineseNewYear,
    isHariRayaHaji,
    isVesakPoya,
    isDeepavali,
    isDiwali,
    isHariRayaHaji
  )

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}
