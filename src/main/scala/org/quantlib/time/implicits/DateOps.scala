package org.quantlib.time.implicits

import java.time.DayOfWeek._
import java.time._

import org.quantlib.time.Period
import org.quantlib.time.enums.TimeUnit

trait DateOps[D] {

  def from(day: Int, month: Month, year: Year): D

  def from(day: Int, month: Month, year: Year, hours: Int, minutes: Int, seconds: Int, nanoSeconds: Int): D

  def MAX_VALUE(date: D): D

  def MII_VALUE(date: D): D

  def fromNumber(epochDay: Long): D

  def YMD(date: D): (Year, Month, Int)

  def monthOf(date: D): Month

  def yearOf(date: D): Year

  def dom(date: D): Int

  def doy(date: D): Int

  def dow(date: D): DayOfWeek

  def HMSN(date: D): (Int, Int, Int, Int)

  def daysBetween(date1: D, date2: D): Long

  def ==(date1: D, date2: D): Boolean

  def !=(date1: D, date2: D): Boolean = ! ==(date1, date2)

  def <(date1: D, date2: D): Boolean

  def <=(date1: D, date2: D): Boolean = ! >(date1, date2)

  def >(date1: D, date2: D): Boolean

  def >=(date1: D, date2: D): Boolean = ! <(date1, date2)

  def min(date1: D, date2: D): D

  def max(date1: D, date2: D): D

  def +(date: D, days: Long): D

  def +(date: D, period: Period): D

  def -(date: D, days: Long): D

  def -(date: D, period: Period): D

  def lengthBetween(date1: D, date2: D, timeUnit: TimeUnit): Long

  def nextWeekday(date: D, w: DayOfWeek): D

  def nthWeekdaynth(nth: Int, dayOfWeek: DayOfWeek, month: Month, year: Year): D

  def firstDayOf(month: Month, year: Year): D

  def lastDayOf(month: Month, year: Year): D

//  def compareTo(date1: D, date2: D): Int

  def sameYear(date1: D, date2: D): Boolean

  def sameMonth(date1: D, date2: D): Boolean

  def previousWednesday(date: D): D

  //  {
  //    val w = date.getDayOfWeek
  //    if (w >= DayOfWeek.THURSDAY) // roll back w-4 days
  //      date - Period(w - 4 , Days)
  //    else // roll forward 4-w days and back one week
  //      date + Period(-(w + 3), Days)
  //  }

  def nextWednesday(date: D): D //= previousWednesday(date.plusDays(7))


}


object DateOps {

  def from[D: DateOps](day: Int, month: Month, year: Year): D ={
    implicitly[DateOps[D]].from(day,month,year)
  }

  def from[D: DateOps](day: Int, month: Month, year: Year,
                       hours: Int, minutes: Int, seconds: Int, nanoSeconds: Int): D ={
    implicitly[DateOps[D]].from(day,month,year,hours,minutes,seconds,nanoSeconds)
  }


  implicit class YearClass(val year: Year) extends AnyVal {
    def -(other: Year): Int = year.getValue - other.getValue

    def ===(number: Int): Boolean = year.getValue == number
  }

  implicit class MonthClass(val month: Month) extends AnyVal {
    def -(other: Month): Int = month.getValue - other.getValue
  }

  implicit class DateOpsClass[D: DateOps](val date: D)  {

    private def impl = implicitly[DateOps[D]]

    def MAX_VALUE: D = impl.MAX_VALUE(date)

    def MII_VALUE: D = impl.MAX_VALUE(date)

    def from(day: Int, month: Month, year: Year): D = impl.from(day, month, year)

    def from(day: Int, month: Month, year: Year, hours: Int, minutes: Int, seconds: Int, nanoSeconds: Int): D = {
      impl.from(day, month, year, hours, minutes, seconds, nanoSeconds)
    }

    def fromNumber(epochDay: Long): D = impl.fromNumber(epochDay)

    def YMD: (Year, Month, Int) = impl.YMD(date)

    def dom: Int = impl.dom(date)

    def dow: DayOfWeek = impl.dow(date)

    def doy: Int = impl.doy(date)

    def HMSN: (Int, Int, Int, Int) = impl.HMSN(date)

    def month: Month = impl.monthOf(date)

    def year: Year = impl.yearOf(date)

    def daysBetween(other: D): Long = impl.daysBetween(date, other)

   // def ==(other: D): Boolean = impl.==(date, other)

   // def !=(other: D): Boolean = ! ==(other)

    def <(other: D): Boolean = impl.<(date, other)

    def <=(other: D): Boolean = ! >(other)

    def >(other: D): Boolean = impl.>(date, other)

    def >=(other: D): Boolean = ! <(other)

    def min(other: D): D = impl.min(date, other)

    def max(other: D): D = impl.max(date, other)

    def +(days: Long): D = impl.+(date, days)

    def +(period: Period): D = impl.+(date, period)

    def -(days: Long): D = impl.-(date, days)

    def -(period: Period): D = impl.-(date, period)

    def to(other: D, timeUnit: TimeUnit): Long = impl.lengthBetween(date, other, timeUnit)

    def nextWeekday(w: DayOfWeek): D = impl.nextWeekday(date, w)

    def nthWeekdaynth(nth: Int, dayOfWeek: DayOfWeek, month: Month, year: Year): D = impl.nthWeekdaynth(nth,dayOfWeek,month,year)

    def firstDayOf(month: Month, year: Year): D = impl.firstDayOf(month, year)

    def lastDayOf(month: Month, year: Year): D = impl.lastDayOf(month, year)

    def isSameYearOf(other: D): Boolean = impl.sameYear(date,other)

    def isSameMonthOf(other: D): Boolean = impl.sameMonth(date, other)
  }

  implicit class WeekdayWrapper(val dayOfWeek: DayOfWeek) extends AnyVal {

    def >=(other: DayOfWeek): Boolean = dayOfWeek.getValue >= other.getValue

    def >=(other: Int): Boolean = dayOfWeek.getValue >= other


    def -(other: Int): Int = dayOfWeek.getValue - other

    def +(other: Int): Int = dayOfWeek.getValue + other

    def -:(other: Int): Int = other - dayOfWeek.getValue

    def +:(other: Int): Int = dayOfWeek.getValue + other

    def asLongWeekDay: String = dayOfWeek match {
      case SUNDAY => "Sunday"
      case MONDAY => "Monday"
      case TUESDAY => "Tuesday"
      case WEDNESDAY => "Wednesday"
      case THURSDAY => "Thursday"
      case FRIDAY => "Friday"
      case SATURDAY => "Saturday"
    }

    def asShortWeekDay: String = dayOfWeek match {
      case SUNDAY => "Sun"
      case MONDAY => "Mon"
      case TUESDAY => "Tue"
      case WEDNESDAY => "Wed"
      case THURSDAY => "Thu"
      case FRIDAY => "Fri"
      case SATURDAY => "Sat"
    }

    def asShortestWeekDay: String = dayOfWeek match {
      case SUNDAY => "Su"
      case MONDAY => "Mo"
      case TUESDAY => "Tu"
      case WEDNESDAY => "We"
      case THURSDAY => "Th"
      case FRIDAY => "Fr"
      case SATURDAY => "Sa"
    }

    override def toString: String = asLongWeekDay
  }

}

