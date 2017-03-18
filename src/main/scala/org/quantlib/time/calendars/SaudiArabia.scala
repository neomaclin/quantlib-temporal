package org.quantlib.time.calendars

import java.time._

import org.quantlib.time.calendars.SaudiArabia.Market
import org.quantlib.time.calendars.SaudiArabia.Market.Tadawul
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object SaudiArabia {

  sealed trait Market

  object Market {

    case object Tadawul extends Market

  }

}

final case class SaudiArabia[D: DateOps](market: Market = Tadawul) extends BusinessCalendar[D] {
  private val weekEndSwitch = DateOps.from(29, Month.JUNE, Year.of(2013))
  override val toString: String = "Tadawul"

  import java.time.Month._

  private val EidalAdhaDates = List[D](
    DateOps.from(7, APRIL, Year.of(1998)),
    DateOps.from(27, MARCH, Year.of(1999)),
    DateOps.from(16, MARCH, Year.of(2000)),
    DateOps.from(5, MARCH, Year.of(2001)),
    DateOps.from(23, FEBRUARY, Year.of(2002)),
    DateOps.from(12, FEBRUARY, Year.of(2003)),
    DateOps.from(1, FEBRUARY, Year.of(2004)),
    DateOps.from(21, JANUARY, Year.of(2005)),
    DateOps.from(10, JANUARY, Year.of(2006)),
    DateOps.from(31, DECEMBER, Year.of(2006)),
    DateOps.from(20, DECEMBER, Year.of(2007)),
    DateOps.from(8, DECEMBER, Year.of(2008)),
    DateOps.from(27, NOVEMBER, Year.of(2009)),
    DateOps.from(16, NOVEMBER, Year.of(2010)),
    DateOps.from(6, NOVEMBER, Year.of(2011)),
    DateOps.from(26, OCTOBER, Year.of(2012)),
    DateOps.from(15, OCTOBER, Year.of(2013)),
    DateOps.from(4, OCTOBER, Year.of(2014)),
    DateOps.from(24, SEPTEMBER, Year.of(2015)),
    DateOps.from(11, SEPTEMBER, Year.of(2016)),
    DateOps.from(1, SEPTEMBER, Year.of(2017)),
    DateOps.from(23, AUGUST, Year.of(2018)),
    DateOps.from(12, AUGUST, Year.of(2019)),
    DateOps.from(31, JULY, Year.of(2020)),
    DateOps.from(20, JULY, Year.of(2021)),
    DateOps.from(10, JULY, Year.of(2022))
  )
  private val EidAlFitrDates = List[D](
    DateOps.from(16, DECEMBER, Year.of(2001)),
    DateOps.from(5, DECEMBER, Year.of(2002)),
    DateOps.from(25, NOVEMBER, Year.of(2003)),
    DateOps.from(13, NOVEMBER, Year.of(2004)),
    DateOps.from(3, NOVEMBER, Year.of(2005)),
    DateOps.from(23, OCTOBER, Year.of(2006)),
    DateOps.from(12, OCTOBER, Year.of(2007)),
    DateOps.from(30, SEPTEMBER, Year.of(2008)),
    DateOps.from(20, SEPTEMBER, Year.of(2009)),
    DateOps.from(10, SEPTEMBER, Year.of(2010)),
    DateOps.from(30, AUGUST, Year.of(2011)),
    DateOps.from(19, AUGUST, Year.of(2012)),
    DateOps.from(8, AUGUST, Year.of(2013)),
    DateOps.from(28, JULY, Year.of(2014)),
    DateOps.from(17, JULY, Year.of(2015)),
    DateOps.from(6, JULY, Year.of(2016)),
    DateOps.from(25, JUNE, Year.of(2017)),
    DateOps.from(15, JUNE, Year.of(2018)),
    DateOps.from(4, JUNE, Year.of(2019)),
    DateOps.from(24, MAY, Year.of(2020)),
    DateOps.from(13, MAY, Year.of(2021)),
    DateOps.from(2, MAY, Year.of(2022)),
    DateOps.from(21, APRIL, Year.of(2023)),
    DateOps.from(10, APRIL, Year.of(2024)),
    DateOps.from(30, MARCH, Year.of(2025)),
    DateOps.from(20, MARCH, Year.of(2026)),
    DateOps.from(9, MARCH, Year.of(2027)),
    DateOps.from(26, FEBRUARY, Year.of(2028)),
    DateOps.from(14, FEBRUARY, Year.of(2029))
  )

  private def isEidAlAdha(date: D) = EidalAdhaDates.exists(p => date >= p - 1 && date <= p + 4)

  private def isEidAlFitr(date: D) = EidAlFitrDates.exists(p => date >= p - 1 && date <= p + 4)

  private def isNationalDay(date: D) = date.dom == 23 && date.inSeptember

  private def isRandomHolidays(date: D) = {
    val (y, m, d) = date.YMD
    (d == 26 && m == FEBRUARY && y === 2011) || (d == 19 && m == MARCH && y === 2011)
  }

  private val holidays = List[D => Boolean](
    isWeekend,
    isEidAlAdha,
    isEidAlFitr,
    isNationalDay,
    isRandomHolidays
  )

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))

  override def isWeekend(date: D): Boolean = {

    if (date < weekEndSwitch) date.dow.isThursday || date.dow.isFriday else date.dow.isFriday || date.dow.isSaturday
  }
}