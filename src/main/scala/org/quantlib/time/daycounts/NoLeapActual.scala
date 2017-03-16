package org.quantlib.time.daycounts

import java.time._

import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

object NoLeapActual{
  private val monthOffset = List(  0,  31,  59,  90, 120, 151,  // Jan - Jun
    181, 212, 243, 273, 304, 334)   // Jun - Dec
}

final case class NoLeapActual[D: DateOps](daysOfYear: Int = 365) extends DayCountBasis[D] {
  require(daysOfYear == 360 || daysOfYear == 365 || daysOfYear == 364)

  import NoLeapActual._

  private def s(date: D) = {
    val (y,m,d) = date.YMD

    val n = date.dom + monthOffset(date.dom - 1) + (y.getValue * daysOfYear)

    if (m == Month.FEBRUARY && d == 29) n - 1 else n
  }

  def dayCount(date1: D, date2: D): Int = s(date2) - s(date1)

  def yearFraction(date1: D, date2: D,
                   refDate1: Option[D] = None,
                   refDate2: Option[D] = None): Double = {
    dayCount(date1,date2)/daysOfYear.toDouble
  }

  override val toString = s"Actual/$daysOfYear (NoLeap)"
}