package org.quantlib.time.calendars

import java.time.Month._
import java.time.Year

import org.quantlib.time.calendars.China.market
import org.quantlib.time.calendars.China.market._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

object China {

  sealed trait market

  object market {

    case object SSE extends market

    case object IB extends market

  }


}

final case class China[D: DateOps](market: market = SSE) extends WeekendSatSun[D] with BusinessCalendar[D] {

  private val workingWeekends = List(
    DateOps.from(5, FEBRUARY, Year.of(2005)),
    DateOps.from(6, FEBRUARY, Year.of(2005)),
    DateOps.from(30, APRIL, Year.of(2005)),
    DateOps.from(8, MAY, Year.of(2005)),
    DateOps.from(8, OCTOBER, Year.of(2005)),
    DateOps.from(9, OCTOBER, Year.of(2005)),
    DateOps.from(31, DECEMBER, Year.of(2005)),
    //Year.of(2006
    DateOps.from(28, JANUARY, Year.of(2006)),
    DateOps.from(29, APRIL, Year.of(2006)),
    DateOps.from(30, APRIL, Year.of(2006)),
    DateOps.from(30, SEPTEMBER, Year.of(2006)),
    DateOps.from(30, DECEMBER, Year.of(2006)),
    DateOps.from(31, DECEMBER, Year.of(2006)),
    // Year.of(2007
    DateOps.from(17, FEBRUARY, Year.of(2007)),
    DateOps.from(25, FEBRUARY, Year.of(2007)),
    DateOps.from(28, APRIL, Year.of(2007)),
    DateOps.from(29, APRIL, Year.of(2007)),
    DateOps.from(29, SEPTEMBER, Year.of(2007)),
    DateOps.from(30, SEPTEMBER, Year.of(2007)),
    DateOps.from(29, DECEMBER, Year.of(2007)),
    // Year.of(2008
    DateOps.from(2, FEBRUARY, Year.of(2008)),
    DateOps.from(3, FEBRUARY, Year.of(2008)),
    DateOps.from(4, MAY, Year.of(2008)),
    DateOps.from(27, SEPTEMBER, Year.of(2008)),
    DateOps.from(28, SEPTEMBER, Year.of(2008)),
    // Year.of(2009
    DateOps.from(4, JANUARY, Year.of(2009)),
    DateOps.from(24, JANUARY, Year.of(2009)),
    DateOps.from(1, FEBRUARY, Year.of(2009)),
    DateOps.from(31, MAY, Year.of(2009)),
    DateOps.from(27, SEPTEMBER, Year.of(2009)),
    DateOps.from(10, OCTOBER, Year.of(2009)),
    // Year.of(2010
    DateOps.from(20, FEBRUARY, Year.of(2010)),
    DateOps.from(21, FEBRUARY, Year.of(2010)),
    DateOps.from(12, JUNE, Year.of(2010)),
    DateOps.from(13, JUNE, Year.of(2010)),
    DateOps.from(19, SEPTEMBER, Year.of(2010)),
    DateOps.from(25, SEPTEMBER, Year.of(2010)),
    DateOps.from(26, SEPTEMBER, Year.of(2010)),
    DateOps.from(9, OCTOBER, Year.of(2010)),
    // Year.of(2011
    DateOps.from(30, JANUARY, Year.of(2011)),
    DateOps.from(12, FEBRUARY, Year.of(2011)),
    DateOps.from(2, APRIL, Year.of(2011)),
    DateOps.from(8, OCTOBER, Year.of(2011)),
    DateOps.from(9, OCTOBER, Year.of(2011)),
    DateOps.from(31, DECEMBER, Year.of(2011)),
    // Year.of(2012
    DateOps.from(21, JANUARY, Year.of(2012)),
    DateOps.from(29, JANUARY, Year.of(2012)),
    DateOps.from(31, MARCH, Year.of(2012)),
    DateOps.from(1, APRIL, Year.of(2012)),
    DateOps.from(28, APRIL, Year.of(2012)),
    DateOps.from(29, SEPTEMBER, Year.of(2012)),
    // Year.of(2013
    DateOps.from(5, JANUARY, Year.of(2013)),
    DateOps.from(6, JANUARY, Year.of(2013)),
    DateOps.from(16, FEBRUARY, Year.of(2013)),
    DateOps.from(17, FEBRUARY, Year.of(2013)),
    DateOps.from(7, APRIL, Year.of(2013)),
    DateOps.from(27, APRIL, Year.of(2013)),
    DateOps.from(28, APRIL, Year.of(2013)),
    DateOps.from(8, JUNE, Year.of(2013)),
    DateOps.from(9, JUNE, Year.of(2013)),
    DateOps.from(22, SEPTEMBER, Year.of(2013)),
    DateOps.from(29, SEPTEMBER, Year.of(2013)),
    DateOps.from(12, OCTOBER, Year.of(2013)),
    // Year.of(2014
    DateOps.from(26, JANUARY, Year.of(2014)),
    DateOps.from(8, FEBRUARY, Year.of(2014)),
    DateOps.from(4, MAY, Year.of(2014)),
    DateOps.from(28, SEPTEMBER, Year.of(2014)),
    DateOps.from(11, OCTOBER, Year.of(2014)),
    // Year.of(2015
    DateOps.from(4, JANUARY, Year.of(2015)),
    DateOps.from(15, FEBRUARY, Year.of(2015)),
    DateOps.from(28, FEBRUARY, Year.of(2015)),
    DateOps.from(6, SEPTEMBER, Year.of(2015)),
    DateOps.from(10, OCTOBER, Year.of(2015)),
    // Year.of(2016
    DateOps.from(6, FEBRUARY, Year.of(2016)),
    DateOps.from(14, FEBRUARY, Year.of(2016)),
    DateOps.from(12, JUNE, Year.of(2016)),
    DateOps.from(18, SEPTEMBER, Year.of(2016)),
    DateOps.from(8, OCTOBER, Year.of(2016)),
    DateOps.from(9, OCTOBER, Year.of(2016)),
    // Year.of(2017
    DateOps.from(22, JANUARY, Year.of(2017)),
    DateOps.from(4, FEBRUARY, Year.of(2017)),
    DateOps.from(1, APRIL, Year.of(2017)),
    DateOps.from(27, MAY, Year.of(2017)),
    DateOps.from(30, SEPTEMBER, Year.of(2017))
  )

  override val toString: String = market match {
    case SSE => "Shanghai stock exchange"
    case IB => "China inter bank market"
  }

  private val SSEHolidays =
    List[D => Boolean](
      isWeekend,
      isChinaNewYear, // not the Chinese New year, but New Year for China
      isChineseNewYear,
      isChingming,
      isLaborDay,
      isTuenNg,
      ismidAutumn,
      isNationalDay,
      isAntiJapanAnniversory)




  override def considerBusinessDay(date: D): Boolean = market match {
    case SSE => !SSEHolidays.exists(f => f(date))
    case IB => !SSEHolidays.exists(f => f(date)) || workingWeekends.contains(date)
  }


  private def isChinaNewYear(date: D) = {
    val (y, m, dd) = date.YMD
    (dd == 1 && m == JANUARY) ||
      (y === 2005 && dd == 3 && m == JANUARY) ||
      (y === 2006 && (dd == 2 || dd == 3) && m == JANUARY) ||
      (y === 2007 && dd <= 3 && m == JANUARY) ||
      (y === 2007 && dd == 31 && m == DECEMBER) ||
      (y === 2009 && dd == 2 && m == JANUARY) ||
      (y === 2011 && dd == 3 && m == JANUARY) ||
      (y === 2012 && (dd == 2 || dd == 3) && m == JANUARY) ||
      (y === 2013 && dd <= 3 && m == JANUARY) ||
      (y === 2014 && dd == 1 && m == JANUARY) ||
      (y === 2015 && dd <= 3 && m == JANUARY) ||
      (y === 2017 && dd == 2 && m == JANUARY)
  }

  private def isChineseNewYear(date: D) = {
    val (y, m, dd) = date.YMD
    (y === 2004 && dd >= 19 && dd <= 28 && m == JANUARY) ||
      (y === 2005 && dd >= 7 && dd <= 15 && m == FEBRUARY) ||
      (y === 2006 && ((dd >= 26 && m == JANUARY) || (dd <= 3 && m == FEBRUARY))) ||
      (y === 2007 && dd >= 17 && dd <= 25 && m == FEBRUARY) ||
      (y === 2008 && dd >= 6 && dd <= 12 && m == FEBRUARY) ||
      (y === 2009 && dd >= 26 && dd <= 30 && m == JANUARY) ||
      (y === 2010 && dd >= 15 && dd <= 19 && m == FEBRUARY) ||
      (y === 2011 && dd >= 2 && dd <= 8 && m == FEBRUARY) ||
      (y === 2012 && dd >= 23 && dd <= 28 && m == JANUARY) ||
      (y === 2013 && dd >= 11 && dd <= 15 && m == FEBRUARY) ||
      (y === 2014 && dd >= 31 && m == JANUARY) ||
      (y === 2014 && dd <= 6 && m == FEBRUARY) ||
      (y === 2015 && dd >= 18 && dd <= 24 && m == FEBRUARY) ||
      (y === 2016 && dd >= 8 && dd <= 12 && m == FEBRUARY) ||
      (y === 2017 && ((dd >= 27 && m == JANUARY) || (dd <= 2 && m == FEBRUARY)))
  }

  private def isChingming(date: D) = {
    val (y, m, dd) = date.YMD
    (y.getValue <= 2008 && dd == 4 && m == APRIL) ||
      (y === 2009 && dd == 6 && m == APRIL) ||
      (y === 2010 && dd == 5 && m == APRIL) ||
      (y === 2011 && dd >= 3 && dd <= 5 && m == APRIL) ||
      (y === 2012 && dd >= 2 && dd <= 4 && m == APRIL) ||
      (y === 2013 && dd >= 4 && dd <= 5 && m == APRIL) ||
      (y === 2014 && dd == 7 && m == APRIL) ||
      (y === 2015 && dd >= 5 && dd <= 6 && m == APRIL) ||
      (y === 2016 && dd == 4 && m == APRIL) ||
      (y === 2017 && dd >= 3 && dd <= 4 && m == APRIL)
  }

  private def isLaborDay(date: D) = {
    val (y, m, dd) = date.YMD
    (y.getValue <= 2007 && dd >= 1 && dd <= 7 && m == MAY) ||
      (y === 2008 && dd >= 1 && dd <= 2 && m == MAY) ||
      (y === 2009 && dd == 1 && m == MAY) ||
      (y === 2010 && dd == 3 && m == MAY) ||
      (y === 2011 && dd == 2 && m == MAY) ||
      (y === 2012 && ((dd == 30 && m == APRIL) || (dd == 1 && m == MAY))) ||
      (y === 2013 && ((dd >= 29 && m == APRIL) || (dd == 1 && m == MAY))) ||
      (y === 2017 && dd == 1 && m == MAY)
  }

  private def isTuenNg(date: D) = {
    val (y, m, dd) = date.YMD
    (y.getValue <= 2008 && dd == 9 && m == JUNE) ||
      (y === 2009 && (dd == 28 || dd == 29) && m == MAY) ||
      (y === 2010 && dd >= 14 && dd <= 16 && m == JUNE) ||
      (y === 2011 && dd >= 4 && dd <= 6 && m == JUNE) ||
      (y === 2012 && dd >= 22 && dd <= 24 && m == JUNE) ||
      (y === 2013 && dd >= 10 && dd <= 12 && m == JUNE) ||
      (y === 2014 && dd == 2 && m == JUNE) ||
      (y === 2015 && dd == 22 && m == JUNE) ||
      (y === 2016 && dd >= 9 && dd <= 10 && m == JUNE) ||
      (y === 2017 && dd >= 29 && dd <= 30 && m == MAY)
  }

  private def isAntiJapanAnniversory(date: D) = {
    val (y, m, dd) = date.YMD

    y === 2015 && dd >= 3 && dd <= 4 && m == SEPTEMBER
  }

  private def ismidAutumn(date: D) = {
    val (y, m, dd) = date.YMD
    (y.getValue <= 2008 && dd == 15 && m == SEPTEMBER) ||
      (y === 2010 && dd >= 22 && dd <= 24 && m == SEPTEMBER) ||
      (y === 2011 && dd >= 10 && dd <= 12 && m == SEPTEMBER) ||
      (y === 2012 && dd == 30 && m == SEPTEMBER) ||
      (y === 2013 && dd >= 19 && dd <= 20 && m == SEPTEMBER) ||
      (y === 2014 && dd == 8 && m == SEPTEMBER) ||
      (y === 2015 && dd == 27 && m == SEPTEMBER) ||
      (y === 2016 && dd >= 15 && dd <= 16 && m == SEPTEMBER)
  }

  private def isNationalDay(date: D) = {
    val (y, m, dd) = date.YMD
    (y.getValue <= 2007 && dd >= 1 && dd <= 7 && m == OCTOBER) ||
      (y === 2008 && ((dd >= 29 && m == SEPTEMBER) || (dd <= 3 && m == OCTOBER))) ||
      (y === 2009 && dd >= 1 && dd <= 8 && m == OCTOBER) ||
      (y === 2010 && dd >= 1 && dd <= 7 && m == OCTOBER) ||
      (y === 2011 && dd >= 1 && dd <= 7 && m == OCTOBER) ||
      (y === 2012 && dd >= 1 && dd <= 7 && m == OCTOBER) ||
      (y === 2013 && dd >= 1 && dd <= 7 && m == OCTOBER) ||
      (y === 2014 && dd >= 1 && dd <= 7 && m == OCTOBER) ||
      (y === 2015 && dd >= 1 && dd <= 7 && m == OCTOBER) ||
      (y === 2016 && dd >= 3 && dd <= 7 && m == OCTOBER) ||
      (y === 2017 && dd >= 2 && dd <= 6 && m == OCTOBER)
  }
}
