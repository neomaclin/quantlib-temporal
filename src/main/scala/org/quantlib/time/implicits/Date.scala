package org.quantlib.time.implicits

import java.time._
import java.time.temporal.ChronoUnit

import cats.Order
import org.quantlib.time.Period
import org.quantlib.time.enums.TimeUnit
import org.quantlib.time.enums.TimeUnit._

import scala.language.implicitConversions

/**
  * Created by neo on 11/03/2017.
  */
object Date {

  val shortFormat = "mm/dd/yyyy"
  val longFormat = "Month ddth, yyyy"
  val isoFormat = "yyyy-mm-dd"

  implicit def toLocalDate(date: LocalDateTime): LocalDate = date.toLocalDate


  implicit object LocalDateOps extends DateOps[LocalDate] with Order[LocalDate]{

    val MAX = LocalDate.of(2199, 12, 31)
    val MIN = LocalDate.of(1901, 1, 1)

    override def compare(x: LocalDate, y: LocalDate): Int = x.compareTo(y)

    def fromNumber(epochDay: Long): LocalDate = LocalDate.ofEpochDay(epochDay)

    def toNumber(date: LocalDate): Long = date.toEpochDay

    def <(date1: LocalDate, date2: LocalDate): Boolean = date1.isBefore(date2)

    def >(date1: LocalDate, date2: LocalDate): Boolean = date1.isAfter(date2)

    override def YMD(date: LocalDate): (Year, Month, Int) = (Year.of(date.getYear), date.getMonth, date.getDayOfMonth)

    override def HMSN(date: LocalDate): (Int, Int, Int, Int) = ???


    override def from(day: Int, month: Month, year: Year): LocalDate = LocalDate.of(year.getValue, month, day)

    override def from(day: Int, month: Month, year: Year,
                      hours: Int, minutes: Int, seconds: Int, nanoSeconds: Int): LocalDate = {
      LocalDate.of(year.getValue, month, day)
    }

    override def +(date: LocalDate, days: Int): LocalDate = date.plusDays(days)

    override def +(date: LocalDate, period: Period): LocalDate = {
      val Period(length, unit) = period
      unit match {
        case Days => date.plusDays(length)

        case Weeks => date.plusWeeks(length)

        case Months => date.plusMonths(length)

        case Years => date.plusYears(length)

        case Hours => ???

        case Minutes => ???

        case Seconds => ???

        case Milliseconds => ???

        case Microseconds => ???
      }
    }

    override def -(date: LocalDate, days: Int): LocalDate = date.minusDays(days)

    override def -(date: LocalDate, period: Period): LocalDate = {
      val Period(length, unit) = period
      unit match {
        case Days => date.minusDays(length)

        case Weeks => date.minusWeeks(length)

        case Months => date.minusMonths(length)

        case Years => date.minusYears(length)

        case Hours => ???

        case Minutes => ???

        case Seconds => ???

        case Milliseconds => ???

        case Microseconds => ???
      }
    }

    override def monthOf(date: LocalDate): Month = date.getMonth

    override def yearOf(date: LocalDate): Year = Year.of(date.getYear)

    override def dom(date: LocalDate): Int = date.getDayOfMonth

    override def dow(date: LocalDate): DayOfWeek = date.getDayOfWeek

    override def doy(date: LocalDate): Int = date.getDayOfYear

    override def now: LocalDate = LocalDate.now()

    override def firstDayOf(month: Month, year: Year): LocalDate = {
      LocalDate.of(year.getValue, month, 1)
    }

    override def lastDayOf(month: Month, year: Year): LocalDate = {
      LocalDate.of(year.getValue, month, LocalDate.of(year.getValue, month, 1).lengthOfMonth)
    }

    override def sameYear(date1: LocalDate, date2: LocalDate): Boolean = date1.getYear == date2.getYear

    override def sameMonth(date1: LocalDate, date2: LocalDate): Boolean = date1.getMonth == date2.getMonth

    override def nthWeekday(nth: Int, dayOfWeek: DayOfWeek, month: Month, year: Year): LocalDate = {
      require(nth > 0, "zeroth day of week in a given (month, year) is undefined")
      require(nth < 6, "no more than 5 weekday in a given (month, year)")
      val first = LocalDate.of(year.getValue, month, 1).getDayOfWeek
      val skip = nth - (if (dayOfWeek.getValue >= first.getValue) 1 else 0)
      LocalDate.of(year.getValue, month, (1 + dayOfWeek.getValue + skip * 7) - first.getValue)
    }

    override def lengthBetween(date1: LocalDate, date2: LocalDate, timeUnit: TimeUnit): Long = {
      if (date1.isAfter(date2)) lengthBetween(date2, date1, timeUnit)
      else timeUnit match {
        case Days => ChronoUnit.DAYS.between(date1, date2)
        case Weeks => ChronoUnit.WEEKS.between(date1, date2)
        case Months => ChronoUnit.MONTHS.between(date1, date2)
        case Years => ChronoUnit.YEARS.between(date1, date2)
        case Hours => ChronoUnit.HOURS.between(date1, date2)
        case Minutes => ChronoUnit.MINUTES.between(date1, date2)
        case Seconds => ChronoUnit.SECONDS.between(date1, date2)
        case Milliseconds => ChronoUnit.MILLIS.between(date1, date2)
        case Microseconds => ChronoUnit.MICROS.between(date1, date2)

      }
    }

    override def dailyDifference(date1: LocalDate, date2: LocalDate): Double = ChronoUnit.DAYS.between(date1, date2).toDouble


  }

}
