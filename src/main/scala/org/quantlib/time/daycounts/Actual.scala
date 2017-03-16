package org.quantlib.time.daycounts

import org.quantlib.time.enums.TimeUnit.Days
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

final case class Actual[D: DateOps](daysOfYear: Int = 360) extends DayCountBasis[D] {

  require(daysOfYear == 360 || daysOfYear == 365 || daysOfYear == 364)

  override def dayCount(date1: D, date2: D): Int = date1.to(date2,Days).toInt

  override def yearFraction(date1: D, date2: D,
                            refDate1: Option[D] = None,
                            refDate2: Option[D] = None): Double = {
    date1.dailyDifference(date2) / daysOfYear.toDouble
  }

  override val toString = s"Actual/$daysOfYear"
}