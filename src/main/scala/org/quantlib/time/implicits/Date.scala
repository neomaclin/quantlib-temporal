package org.quantlib.time.implicits

import java.time._
import java.time.temporal.ChronoUnit

import org.quantlib.time.Period
import org.quantlib.time.enums.TimeUnit
import org.quantlib.time.enums.TimeUnit._

import scala.collection.immutable.Range.Inclusive
import scala.language.implicitConversions

/**
  * Created by neo on 11/03/2017.
  */
object Date {

  val shortFormat = "mm/dd/yyyy"
  val longFormat = "Month ddth, yyyy"
  val isoFormat = "yyyy-mm-dd"

  implicit def toLocalDate(date: LocalDateTime): LocalDate = date.toLocalDate

  implicit object LocalDateOrdering extends Ordering[LocalDate]{
    override def compare(x: LocalDate, y: LocalDate): Int = x.compareTo(y)
  }

  implicit object LocalDateOps extends DateOps[LocalDate] {

    def fromNumber(epochDay: Long): LocalDate = LocalDate.ofEpochDay(epochDay)

    def toNumber(date: LocalDate): Long = date.toEpochDay

    def ==(date1: LocalDate, date2: LocalDate): Boolean = date1.isEqual(date2)

    def <(date1: LocalDate, date2: LocalDate): Boolean = date1.isBefore(date2)

    def >(date1: LocalDate, date2: LocalDate): Boolean = date1.isAfter(date2)


    override def YMD(date: LocalDate): (Year, Month, Int) = (Year.of(date.getYear), date.getMonth, date.getDayOfMonth)

    override def HMSN(date: LocalDate): (Int, Int, Int, Int) = ???

    override def daysBetween(date1: LocalDate, date2: LocalDate): Long = ChronoUnit.DAYS.between(date1, date2)

    def min(date1: LocalDate, date2: LocalDate): LocalDate = {
      if (date1.isEqual(date2)) {
        date1
      } else if (date1.isBefore(date2)) {
        date1
      } else {
        date2
      }
    }

    def max(date1: LocalDate, date2: LocalDate): LocalDate = {
      if (date1.equals(min(date1, date2))) date2 else date1
    }

    override def from(day: Int, month: Month, year: Year): LocalDate = LocalDate.of(year.getValue, month, day)

    override def from(day: Int, month: Month, year: Year,
                      hours: Int, minutes: Int, seconds: Int, nanoSeconds: Int): LocalDate = {
      LocalDate.of(year.getValue, month, day)
    }

    override def +(date: LocalDate, days: Long): LocalDate = date.plusDays(days)

    override def +(date: LocalDate, period: Period): LocalDate ={
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

    override def -(date: LocalDate, days: Long): LocalDate = date.minusDays(days)

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

//    override def nthWeekday(nth: Int, dayOfWeek: DayOfWeek, month: Int, year: Int): LocalDate = {
//      require( nth>0 && nth < 6, "no more than 5 weekdays for given month year")
//
//      val first = new LocalDate(year, month, 1).getDayOfWeek
//      val skip = if (dayOfWeek >= first) nth - 1 else nth
//
//      new LocalDate(year, month, (1 + dayOfWeek.getValue + skip * 7) - first.getValue )
//    }

   // override def isEndOfMonth(date: LocalDate): LocalDate = date.is

    override def MAX_VALUE(date: LocalDate): LocalDate = LocalDate.MAX

    override def MII_VALUE(date: LocalDate): LocalDate = LocalDate.MIN

    override def monthOf(date: LocalDate): Month = date.getMonth

    override def yearOf(date: LocalDate): Year =  Year.of(date.getYear)

    override def dom(date: LocalDate): Int = date.getDayOfMonth

    override def dow(date: LocalDate): DayOfWeek = date.getDayOfWeek

    override def doy(date: LocalDate): Int = date.getDayOfYear

    override def nextWeekday(date: LocalDate, w: DayOfWeek): LocalDate = ???

    override def firstDayOf(month: Month, year: Year): LocalDate = ???

    override def lastDayOf(month: Month, year: Year): LocalDate = ???

    override def sameYear(date1: LocalDate, date2: LocalDate): Boolean = date1.getYear == date2.getYear

    override def sameMonth(date1: LocalDate, date2: LocalDate): Boolean = date1.getMonth == date2.getMonth

    override def previousWednesday(date: LocalDate): LocalDate = ???

    override def nextWednesday(date: LocalDate): LocalDate = ???


    override def nthWeekdaynth(nth: Int, dayOfWeek: DayOfWeek, month: Month, year: Year): LocalDate = ???

    override def lengthBetween(date1: LocalDate, date2: LocalDate, timeUnit: TimeUnit): Long = ???
  }

  //  def firstDayOf(month: Int, year: Int): LocalDate = {
  //    LocalDate.now.withDayOfMonth(1).withMonthOfYear(month).withYear(year)
  //  }
  //
  //  def lastDayOf(month: Int, year: Int): LocalDate = {
  //    firstDayOf(month,year).plusMonths(1).plusDays(-1)
  //  }

}
