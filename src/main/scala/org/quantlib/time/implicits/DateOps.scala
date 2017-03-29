package org.quantlib.time.implicits

import java.time.DayOfWeek._
import java.time._

import cats.Order
import org.quantlib.time.Period
import org.quantlib.time.enums.TimeUnit


trait DateOps[D] extends Order[D]{

  def now: D

  val MAX: D

  val MIN: D

  def from(day: Int, month: Month, year: Year): D

  def from(day: Int, month: Month, year: Year, hours: Int, minutes: Int, seconds: Int, nanoSeconds: Int): D

  def fromNumber(epochDay: Long): D

  def YMD(date: D): (Year, Month, Int)

  def monthOf(date: D): Month

  def yearOf(date: D): Year

  def dom(date: D): Int

  def doy(date: D): Int

  def dow(date: D): DayOfWeek

  def HMSN(date: D): (Int, Int, Int, Int)


  def +(date: D, days: Int): D

  def +(date: D, period: Period): D

  def -(date: D, days: Int): D

  def -(date: D, period: Period): D

  def lengthBetween(date1: D, date2: D, timeUnit: TimeUnit): Long

  def nthWeekday(nth: Int, dayOfWeek: DayOfWeek, month: Month, year: Year): D

  def firstDayOf(month: Month, year: Year): D

  def lastDayOf(month: Month, year: Year): D

  def dailyDifference(date1: D, date2: D): Double

  def sameYear(date1: D, date2: D): Boolean

  def sameMonth(date1: D, date2: D): Boolean

}

object DateOps extends scala.AnyRef with cats.syntax.AllSyntax with cats.instances.AllInstances{

  def from[D: DateOps](day: Int, month: Month, year: Year): D = implicitly[DateOps[D]].from(day, month, year)

  def now[D: DateOps]: D = implicitly[DateOps[D]].now

  def MAX[D: DateOps]: D = implicitly[DateOps[D]].MAX

  def MIN[D: DateOps]: D = implicitly[DateOps[D]].MIN

  def from[D: DateOps](day: Int, month: Month, year: Year,
                       hours: Int, minutes: Int, seconds: Int, nanoSeconds: Int): D = {
    implicitly[DateOps[D]].from(day, month, year, hours, minutes, seconds, nanoSeconds)
  }

  def nthWeekday[D: DateOps](nth: Int, dayOfWeek: DayOfWeek, month: Month, year: Year): D = {
    implicitly[DateOps[D]].nthWeekday(nth, dayOfWeek, month, year)
  }

  def lastDayOf[D: DateOps](month: Month, year: Year): D = implicitly[DateOps[D]].lastDayOf(month, year)


  def firstDayOf[D: DateOps](month: Month, year: Year): D = implicitly[DateOps[D]].firstDayOf(month, year)

  implicit class YearClass(val year: Year) extends AnyVal {
    def -(other: Year): Int = year.getValue - other.getValue

    def -(other: Int): Year = Year.of(year.getValue - other)

    def +(other: Int): Year = Year.of(year.getValue + other)

    def >(other: Year): Boolean = year.getValue > other.getValue

    def <(other: Year): Boolean = year.getValue < other.getValue

    def <=(other: Year): Boolean = ! >(other)

    def >=(other: Year): Boolean = ! <(other)

    def >(other: Int): Boolean = year.getValue > other

    def <(other: Int): Boolean = year.getValue < other

    def <=(other: Int): Boolean = ! >(other)

    def >=(other: Int): Boolean = ! <(other)

    def ===(number: Int): Boolean = year.getValue == number
  }

  implicit class MonthClass(val month: Month) extends AnyVal {
    def -(other: Month): Int = month.getValue - other.getValue

    def >(other: Month): Boolean = month.getValue > other.getValue

    def <(other: Month): Boolean = month.getValue < other.getValue

    def <=(other: Month): Boolean = ! >(other)

    def >=(other: Month): Boolean = ! <(other)

  }

  implicit class DateOpsClass[D: DateOps](val date: D) {

    private def impl = implicitly[DateOps[D]]

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

    def dailyDifference(other: D): Double = impl.dailyDifference(date, other)

    def +(days: Int): D = impl.+(date, days)

    def +(period: Period): D = impl.+(date, period)

    def -(days: Int): D = impl.-(date, days)

    def -(period: Period): D = impl.-(date, period)

    def to(other: D, timeUnit: TimeUnit): Long = impl.lengthBetween(date, other, timeUnit)

    // def nextWeekday(w: DayOfWeek): D = impl.nextWeekday(date, w)
    def endOfMonth: D = impl.lastDayOf(date.month, date.year)

    def isSameYearOf(other: D): Boolean = impl.sameYear(date, other)

    def isSameMonthOf(other: D): Boolean = impl.sameMonth(date, other)

    import java.time.Month._

    def inDecember: Boolean = date.month == DECEMBER

    def inJanuary: Boolean = date.month == JANUARY

    def inFebruary: Boolean = date.month == FEBRUARY

    def inMarch: Boolean = date.month == MARCH

    def inApril: Boolean = date.month == APRIL

    def inMay: Boolean = date.month == MAY

    def inJune: Boolean = date.month == JUNE

    def inJuly: Boolean = date.month == JULY

    def inAugust: Boolean = date.month == AUGUST

    def inSeptember: Boolean = date.month == SEPTEMBER

    def inOctober: Boolean = date.month == OCTOBER

    def inNovember: Boolean = date.month == NOVEMBER

  }

  implicit class WeekdayClass(val dayOfWeek: DayOfWeek) extends AnyVal {

    def >=(other: DayOfWeek): Boolean = dayOfWeek.getValue >= other.getValue

    def -(other: Int): Int = dayOfWeek.getValue - other

    def +(other: Int): Int = dayOfWeek.getValue + other

    def -:(other: Int): Int = other - dayOfWeek.getValue

    def +:(other: Int): Int = dayOfWeek.getValue + other

    def isMonday: Boolean = dayOfWeek == MONDAY

    def isTuesDay: Boolean = dayOfWeek == TUESDAY

    def isWednesday: Boolean = dayOfWeek == WEDNESDAY

    def isThursday: Boolean = dayOfWeek == THURSDAY

    def isFriday: Boolean = dayOfWeek == FRIDAY

    def isSaturday: Boolean = dayOfWeek == SATURDAY

    def isSunday: Boolean = dayOfWeek == SUNDAY


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

