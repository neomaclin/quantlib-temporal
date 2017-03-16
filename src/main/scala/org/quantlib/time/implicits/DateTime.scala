package org.quantlib.time.implicits

import java.time._
import java.time.temporal.ChronoUnit

import org.quantlib.time.Period
import org.quantlib.time.enums.TimeUnit
import org.quantlib.time.enums.TimeUnit._

import scala.language.implicitConversions

/**
  * Created by neo on 11/03/2017.
  */
object DateTime {
  val isoFormat = "YYYY-MM-DDThh:mm:ss,SSSSSS"

  implicit def toLocalDateTime(date: LocalDate): LocalDateTime = LocalDateTime.of(date, LocalTime.MIDNIGHT)

  implicit object LocalDateOrdering extends Ordering[LocalDateTime]{
    override def compare(x: LocalDateTime, y: LocalDateTime): Int = x.compareTo(y)
  }
  implicit object LocalDateTimeOps extends DateOps[LocalDateTime] {
    override def fromNumber(epochDay: Long): LocalDateTime = LocalDateTime.of(LocalDate.ofEpochDay(epochDay), LocalTime.MIDNIGHT)

    // def toNumber(date: LocalDateTime): Long = date.toEpochSecond()
    override def min(date1: LocalDateTime, date2: LocalDateTime): LocalDateTime = {
      if (date1.isEqual(date2)) {
        date1
      } else if (date1.isBefore(date2)) {
        date1
      } else {
        date2
      }
    }

    override def max(date1: LocalDateTime, date2: LocalDateTime): LocalDateTime = {
      if (date1.equals(min(date1, date2))) date2 else date1
    }

    override def YMD(date: LocalDateTime): (Year, Month, Int) = (Year.of(date.getYear), date.getMonth, date.getDayOfMonth)

    override def HMSN(date: LocalDateTime): (Int, Int, Int, Int) = (date.getHour, date.getMinute, date.getSecond, date.getNano)

    override def <(date1: LocalDateTime, date2: LocalDateTime): Boolean = date1.isBefore(date2)

    override def >(date1: LocalDateTime, date2: LocalDateTime): Boolean = date1.isAfter(date2)

    override def from(day: Int, month: Month, year: Year): LocalDateTime = {
      LocalDateTime.of(LocalDate.of(year.getValue,month,day), LocalTime.MIDNIGHT)
    }

    override def from(day: Int, month: Month, year: Year,
                      hours: Int, minutes: Int, seconds: Int, nanoSeconds: Int = 0): LocalDateTime = {
      LocalDateTime.of(LocalDate.of(year.getValue,month,day), LocalTime.of(hours,minutes,seconds,nanoSeconds))
    }


    override def MAX_VALUE(date: LocalDateTime): LocalDateTime = LocalDateTime.MAX

    override def MII_VALUE(date: LocalDateTime): LocalDateTime =  LocalDateTime.MIN

    override def monthOf(date: LocalDateTime): Month = date.getMonth

    override def yearOf(date: LocalDateTime): Year = Year.of(date.getYear)

    override def dom(date: LocalDateTime): Int = date.getDayOfMonth

    override def dow(date: LocalDateTime): DayOfWeek = date.getDayOfWeek

    override def doy(date: LocalDateTime): Int = date.getDayOfYear

    override def +(date: LocalDateTime, days: Int): LocalDateTime = date.plusDays(days)

    override def +(date: LocalDateTime, period: Period): LocalDateTime = {
      val Period(length, unit) = period
      unit match {
        case Days => date.plusDays(length)

        case Weeks => date.plusWeeks(length)

        case Months => date.plusMonths(length)

        case Years => date.plusYears(length)

        case Hours => date.plusHours(length)

        case Minutes => date.plusMinutes(length)

        case Seconds => date.plusSeconds(length)

        case Milliseconds => date.plusNanos(length*1000000)

        case Microseconds => date.plusNanos(length*1000)
      }
    }

    override def -(date: LocalDateTime, days: Int): LocalDateTime = date.minusDays(days)

    override def -(date: LocalDateTime, period: Period): LocalDateTime = {
      val Period(length, unit) = period
      unit match {
        case Days => date.minusDays(length)

        case Weeks => date.minusWeeks(length)

        case Months => date.minusMonths(length)

        case Years => date.minusYears(length)

        case Hours => date.minusHours(length)

        case Minutes => date.minusMinutes(length)

        case Seconds => date.minusSeconds(length)

        case Milliseconds => date.minusNanos(length*1000000)

        case Microseconds => date.minusNanos(length*1000)
      }
    }

    override def nextWeekday(date: LocalDateTime, w: DayOfWeek): LocalDateTime = ???

    override def nthWeekdaynth(nth: Int, dayOfWeek: DayOfWeek, month: Month, year: Year): LocalDateTime = ???

    override def firstDayOf(month: Month, year: Year): LocalDateTime = ???

    override def lastDayOf(month: Month, year: Year): LocalDateTime = ???

    override def sameYear(date1: LocalDateTime, date2: LocalDateTime): Boolean = date1.getYear == date2.getYear

    override def sameMonth(date1: LocalDateTime, date2: LocalDateTime): Boolean = date1.getMonth == date2.getMonth

    override def previousWednesday(date: LocalDateTime): LocalDateTime = ???

    override def nextWednesday(date: LocalDateTime): LocalDateTime = ???

    override def lengthBetween(date1: LocalDateTime, date2: LocalDateTime, timeUnit: TimeUnit): Long = {
      timeUnit match {
        case Days => ChronoUnit.DAYS.between(date1, date2)
        case Weeks => ChronoUnit.WEEKS.between(date1,date2)
        case Months => ChronoUnit.MONTHS.between(date1,date2)
        case Years => ChronoUnit.YEARS.between(date1,date2)
        case Hours => ChronoUnit.HOURS.between(date1,date2)
        case Minutes => ChronoUnit.MINUTES.between(date1,date2)
        case Seconds => ChronoUnit.SECONDS.between(date1,date2)
        case Milliseconds => ChronoUnit.MILLIS.between(date1,date2)
        case Microseconds => ChronoUnit.MICROS.between(date1,date2)

      }
    }

    override def dailyDifference(date1: LocalDateTime, date2: LocalDateTime): Double = {
      ChronoUnit.DAYS.between(date1, date2) + (date2.toLocalTime.toSecondOfDay -date1.toLocalTime.toSecondOfDay)/86400.0
    }
  }

}
