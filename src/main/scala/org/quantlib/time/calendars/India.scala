package org.quantlib.time.calendars


import org.quantlib.time.calendars.BusinessCalendar.Western
import org.quantlib.time.calendars.India.Market
import org.quantlib.time.calendars.India.Market.NSE
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */


object India {

  sealed trait Market

  object Market {

    case object NSE extends Market

  }

}

final case class India[D: DateOps](market: Market = NSE) extends WeekendSatSun[D] with BusinessCalendar[D] {

  override val toString: String = "National Stock Exchange of India"

  import BusinessCalendar.InternationalHolidays._

  private def isNationalHoliday(date: D) = {
    val (y, m, d) = date.YMD
    y.getValue match {
      case 2005 =>
        // Moharram, Holi, Maharashtra Day, and Ramzan Id fall
        // on Saturday or Sunday in 2005
        // Bakri Id
        (d == 21 && date.inJanuary) ||
          // Ganesh Chaturthi
          (d == 7 && date.inSeptember) ||
          // Dasara
          (d == 12 && date.inOctober) ||
          // Laxmi Puja
          (d == 1 && date.inNovember) ||
          // Bhaubeej
          (d == 3 && date.inNovember) ||
          // Guru Nanak Jayanti
          (d == 15 && date.inNovember)
      case 2006 =>
        // Bakri Id
        (d == 11 && date.inJanuary) ||
          // Moharram
          (d == 9 && date.inFebruary) ||
          // Holi
          (d == 15 && date.inMarch) ||
          // Ram Navami
          (d == 6 && date.inApril) ||
          // Mahavir Jayanti
          (d == 11 && date.inApril) ||
          // Maharashtra Day
          (d == 1 && date.inMay) ||
          // Bhaubeej
          (d == 24 && date.inOctober) ||
          // Ramzan Id
          (d == 25 && date.inOctober)
      case 2007 =>
        // Bakri Id
        (d == 1 && date.inJanuary) ||
          // Moharram
          (d == 30 && date.inJanuary) ||
          // Mahashivratri
          (d == 16 && date.inFebruary) ||
          // Ram Navami
          (d == 27 && date.inMarch) ||
          // Maharashtra Day
          (d == 1 && date.inMay) ||
          // Buddha Pournima
          (d == 2 && date.inMay) ||
          // Laxmi Puja
          (d == 9 && date.inNovember) ||
          // Bakri Id (again(date)) ||
          (d == 21 && date.inDecember)
      case 2008 =>
        // Mahashivratri
        (d == 6 && date.inMarch) ||
          // Id-E-Milad
          (d == 20 && date.inMarch) ||
          // Mahavir Jayanti
          (d == 18 && date.inApril) ||
          // Maharashtra Day
          (d == 1 && date.inMay) ||
          // Buddha Pournima
          (d == 19 && date.inMay) ||
          // Ganesh Chaturthi
          (d == 3 && date.inSeptember) ||
          // Ramzan Id
          (d == 2 && date.inOctober) ||
          // Dasara
          (d == 9 && date.inOctober) ||
          // Laxmi Puja
          (d == 28 && date.inOctober) ||
          // Bhau bhij
          (d == 30 && date.inOctober) ||
          // Gurunanak Jayanti
          (d == 13 && date.inNovember) ||
          // Bakri Id
          (d == 9 && date.inDecember)
      case 2009 =>
        // Moharram
        (d == 8 && date.inJanuary) ||
          // Mahashivratri
          (d == 23 && date.inFebruary) ||
          // Id-E-Milad
          (d == 10 && date.inMarch) ||
          // Holi
          (d == 11 && date.inMarch) ||
          // Ram Navmi
          (d == 3 && date.inApril) ||
          // Mahavir Jayanti
          (d == 7 && date.inApril) ||
          // Maharashtra Day
          (d == 1 && date.inMay) ||
          // Ramzan Id
          (d == 21 && date.inSeptember) ||
          // Dasara
          (d == 28 && date.inSeptember) ||
          // Bhau Bhij
          (d == 19 && date.inOctober) ||
          // Gurunanak Jayanti
          (d == 2 && date.inNovember) ||
          // Moharram (again(date)) ||
          (d == 28 && date.inDecember)
      case 2010 =>
        // New Year's Day
        (d == 1 && date.inJanuary) ||
          // Mahashivratri
          (d == 12 && date.inFebruary) ||
          // Holi
          (d == 1 && date.inMarch) ||
          // Ram Navmi
          (d == 24 && date.inMarch) ||
          // Ramzan Id
          (d == 10 && date.inSeptember) ||
          // Laxmi Puja
          (d == 5 && date.inNovember) ||
          // Bakri Id
          (d == 17 && date.inNovember) ||
          // Moharram
          (d == 17 && date.inDecember)
      case 2011 =>
        // Mahashivratri
        (d == 2 && date.inMarch) ||
          // Ram Navmi
          (d == 12 && date.inApril) ||
          // Ramzan Id
          (d == 31 && date.inAugust) ||
          // Ganesh Chaturthi
          (d == 1 && date.inSeptember) ||
          // Dasara
          (d == 6 && date.inOctober) ||
          // Laxmi Puja
          (d == 26 && date.inOctober) ||
          // Diwali - Balipratipada
          (d == 27 && date.inOctober) ||
          // Bakri Id
          (d == 7 && date.inNovember) ||
          // Gurunanak Jayanti
          (d == 10 && date.inNovember) ||
          // Moharram
          (d == 6 && date.inDecember)
      case 2012 =>
        // Mahashivratri
        (d == 20 && date.inFebruary) ||
          // Holi
          (d == 8 && date.inMarch) ||
          // Mahavir Jayanti
          (d == 5 && date.inApril) ||
          // Ramzan Id
          (d == 20 && date.inAugust) ||
          // Ganesh Chaturthi
          (d == 19 && date.inSeptember) ||
          // Dasara
          (d == 24 && date.inOctober) ||
          // Diwali - Balipratipada
          (d == 14 && date.inNovember) ||
          // Gurunanak Jayanti
          (d == 28 && date.inNovember)
      case 2013 =>
        // Holi
        (d == 27 && date.inMarch) ||
          // Ram Navmi
          (d == 19 && date.inApril) ||
          // Mahavir Jayanti
          (d == 24 && date.inApril) ||
          // Ramzan Id
          (d == 9 && date.inAugust) ||
          // Ganesh Chaturthi
          (d == 9 && date.inSeptember) ||
          // Bakri Id
          (d == 16 && date.inOctober) ||
          // Diwali - Balipratipada
          (d == 4 && date.inNovember) ||
          // Moharram
          (d == 14 && date.inNovember)
      case 2014 =>
        // Mahashivratri
        (d == 27 && date.inFebruary) ||
          // Holi
          (d == 17 && date.inMarch) ||
          // Ram Navmi
          (d == 8 && date.inApril) ||
          // Ramzan Id
          (d == 29 && date.inJuly) ||
          // Ganesh Chaturthi
          (d == 29 && date.inAugust) ||
          // Dasera
          (d == 3 && date.inOctober) ||
          // Bakri Id
          (d == 6 && date.inOctober) ||
          // Diwali - Balipratipada
          (d == 24 && date.inOctober) ||
          // Moharram
          (d == 4 && date.inNovember) ||
          // Gurunank Jayanti
          (d == 6 && date.inNovember)
    }
  }

  private def isRepublicDay(date: D) = date.dom == 26 && date.inJanuary

  private def isAmbedkarJayanti(date: D) = date.dom == 14 && date.inApril

  private def isIndependenceDay(date: D) = date.dom == 15 && date.inAugust

  private def isGandhiJayanti(date: D) = date.dom == 2 && date.inOctober

  private val holidays = List[D => Boolean](
    isWeekend,
    isRepublicDay,
    Western.isGoodFriday,
    isAmbedkarJayanti,
    isIndependenceDay,
    isGandhiJayanti,
    isNationalHoliday,
    isLabourDay,
    isChristmas
  )

  override def considerBusinessDay(date: D): Boolean = !holidays.exists(f => f(date))
}