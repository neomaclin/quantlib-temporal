package org.quantlib.time.daycounts

import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

final case class Actual[D: DateOps](daysOfYear: Int = 360) extends DayCountBasis[D] {

  require(daysOfYear == 360 || daysOfYear == 365 || daysOfYear == 364)

  override def dayCount(date1: D, date2: D): Long = date2.daysBetween(date1)

  override def yearFraction(date1: D, date2: D,
                            refDate1: Option[D] = None,
                            refDate2: Option[D] = None): Double = {
    dayCount(date1, date2) / daysOfYear.toDouble
  }

  override val toString = s"Actual/$daysOfYear"
}