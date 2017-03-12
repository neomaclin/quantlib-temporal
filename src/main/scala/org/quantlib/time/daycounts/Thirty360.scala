package org.quantlib.time.daycounts


import java.time.Month

import org.quantlib.time.daycounts.Thirty360._
import org.quantlib.time.daycounts.Thirty360.Convention._

import scala.language.implicitConversions
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._


object Thirty360 {

  sealed trait Convention

  object Convention {

    case object USA extends Convention

    case object BondBasis extends Convention

    case object European extends Convention

    case object EurobondBasis extends Convention

    case object Italian extends Convention

  }

}

final case class Thirty360[D: DateOps](c: Convention = BondBasis) extends DayCountBasis[D] {

  private def dayCountUS(date1: D, date2: D) = {
    var (yy1, mm1, dd1) = date1.YMD
    var (yy2, mm2, dd2) = date2.YMD

    if (dd2 == 31 && dd1 < 30) {
      dd2 = 1; mm2 = mm2.plus(1)
    }

    360 * (yy2 - yy1) + 30 * (mm2 - mm1 - 1) + Math.max(0, 30 - dd1) + Math.min(30, dd2)

  }

  private def dayCountEU(date1: D, date2: D) = {
    var (yy1, mm1, dd1) = date1.YMD
    var (yy2, mm2, dd2) = date2.YMD

    360 * (yy2 - yy1) + 30 * (mm2 - mm1 - 1) + Math.max(0, 30 - dd1) + Math.min(30, dd2)

  }

  private def dayCountIT(date1: D, date2: D) = {
    var (yy1, mm1, dd1) = date1.YMD
    var (yy2, mm2, dd2) = date2.YMD

    if (mm1 == Month.FEBRUARY && dd1 > 27) dd1 = 30
    if (mm2 == Month.FEBRUARY && dd2 > 27) dd2 = 30

    360 * (yy2 - yy1) + 30 * (mm2 - mm1 - 1) + Math.max(0, 30 - dd1) + Math.min(30, dd2)
  }

  private val dayCountImpl: (D, D) => Long = c match {
    case USA | BondBasis => dayCountUS
    case European | EurobondBasis => dayCountEU
    case Italian => dayCountIT
  }

  def dayCount(date1: D, date2: D): Long = dayCountImpl(date1, date2)


  def yearFraction(date1: D, date2: D,
                   refDate1: Option[D] = None,
                   refDate2: Option[D] = None): Double = dayCountImpl(date1, date2) / 360.0

  override val toString: String = c match {
    case USA | BondBasis => "30/360 (Bond Basis)"
    case European | EurobondBasis => "30E/360 (Eurobond Basis)"
    case Italian => "30/360 (Italian)"
  }
}
