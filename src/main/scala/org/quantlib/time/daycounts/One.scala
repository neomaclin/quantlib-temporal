package org.quantlib.time.daycounts

import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

final case class One[D: DateOps]() extends DayCountBasis[D] {

  override def dayCount(date1: D, date2: D): Long = if (date1 > date2) -1 else 1

  override def yearFraction(date1: D, date2: D,
                   refDate1: Option[D] = None,
                   refDate2: Option[D] = None): Double = dayCount(date1, date2).toDouble

  override val toString = "1/1"
}
