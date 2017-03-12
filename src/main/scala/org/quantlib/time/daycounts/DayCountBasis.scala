package org.quantlib.time.daycounts

/**
  * Created by neo on 11/03/2017.
  */
trait DayCountBasis[D] {

  def dayCount(date1: D, date2: D): Long

  def yearFraction(date1: D, date2: D,  refDate1: Option[D], refDate2: Option[D]): Double

}
