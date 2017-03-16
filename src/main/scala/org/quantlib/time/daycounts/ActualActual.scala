package org.quantlib.time.daycounts

import java.time._

import org.quantlib.time.Period
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
import org.quantlib.time.daycounts.ActualActual.Convention
import org.quantlib.time.daycounts.ActualActual.Convention._
import org.quantlib.time.enums.TimeUnit._

import scala.annotation.tailrec

//! Actual/Actual day count
/*! The day count can be calculated according to:

    - the ISDA convention, also known as "Actual/Actual (Historical)",
      "Actual/Actual", "Act/Act", and according to ISDA also "Actual/365",
      "Act/365", and "A/365";
    - the ISMA and US Treasury convention, also known as
      "Actual/Actual (Bond)";
    - the AFB convention, also known as "Actual/Actual (Euro)".

    For more details, refer to
    http://www.isda.org/publications/pdf/Day-Count-Fracation1999.pdf

    \test the correctness of the results is checked against known good values.
*/


object ActualActual {

  sealed trait Convention

  object Convention {

    case object ISMA extends Convention

    case object Bond extends Convention

    case object ISDA extends Convention

    case object Historical extends Convention

    case object Actual365 extends Convention

    case object AFB extends Convention

    case object Euro extends Convention

  }

}


final case class ActualActual[D: DateOps](convention: Convention = ISDA) extends DayCountBasis[D] {

  def dayCount(date1: D, date2: D): Int = date1.to(date2, Days).toInt

  private def yearFractionISMA(date1: D, date2: D,
                               refDate1: Option[D],
                               refDate2: Option[D]): Double = {
    if (date1 == date2) {
      0.0
    } else if (date1 > date2) {
      -yearFractionISMA(date2, date1, refDate1, refDate2)
    }
    else {
      val refPeriodStart = refDate1 getOrElse date1
      val refPeriodEnd = refDate2 getOrElse date2

      require(refPeriodEnd > refPeriodStart && refPeriodEnd > date1,
        s"invalid reference period: date 1: $date1, date 2: $date2" +
          s", reference period start: $refPeriodStart, reference period end: $refPeriodEnd")

      val duration = refPeriodStart.dailyDifference(refPeriodEnd)
      val (months, refStart, refEnd) = (0.5 + 12 * duration / 365).toInt match {
        case 0 => (12, date1, date1 + Period(1, Years))
        case i => (i, refPeriodStart, refPeriodEnd)
      }

      val period = months.toDouble / 12.0

      if (date2 <= refEnd) {
        if (date1 >= refStart) {
          period * date1.dailyDifference(date2) / refStart.dailyDifference(refEnd)
        } else {
          val previousRef = refStart - Period(months, Months)
          if (date2 > refStart)
            yearFractionISMA(date1, refStart, Some(previousRef), Some(refStart)) +
              yearFractionISMA(refStart, date2, Some(refStart), Some(refEnd))
          else
            yearFractionISMA(date1, date2, Some(previousRef), Some(refStart))
        }
      } else {
        require(refStart <= date1, "invalid dates: date1 < refPeriodStart < refPeriodEnd < date2")
        val date1ToRefEnd = yearFractionISMA(date1, refEnd, Some(refStart), Some(refEnd))

        @tailrec
        def sumUp(i: Int, sum: Double): (Double, D, D) = {
          val newRefStart = refEnd + Period(months * i, Months)
          val newRefEnd = refEnd + Period(months * (i + 1), Months)
          if (date2 < newRefEnd) (sum, newRefStart, newRefEnd) else sumUp(i + 1, sum + period)
        }

        val (remaining, newRefStart, newRefEnd) = sumUp(0, date1ToRefEnd)

        remaining + yearFractionISMA(newRefStart, date2, Some(newRefStart), Some(newRefEnd))
      }
    }


  }

  private def yearFractionISDA(date1: D, date2: D,
                               refDate1: Option[D],
                               refDate2: Option[D]): Double = {

    def base(date: D): Double = if (date.year.isLeap) 366.0 else 365.0

    if (date1 == date2) {
      0.0
    } else if (date1 > date2) {
      -yearFractionISDA(date2, date1, None, None)
    } else {
      val year1 = date1.year
      val year2 = date2.year
      var sum = year2 - year1 - 1.0
      sum = sum + date1.dailyDifference(from(1, Month.JANUARY, Year.of(year1.getValue + 1))) / base(date1)
      sum = sum + from(1, Month.JANUARY, year2).dailyDifference(date2) / base(date2)
      sum
    }

  }

  private def yearFractionAFB(date1: D, date2: D,
                              refDate1: Option[D],
                              refDate2: Option[D]): Double = {
    if (date1 == date2) {
      0.0
    } else if (date1 > date2) {
      yearFractionAFB(date2, date1, refDate1, refDate2)
    } else {

      val period = Period(1, Years)
      var temp = date2
      var newD2 = date2
      var sum = 0.0
      while (temp > date1) {
        temp = newD2 - period
        val (yy, mm, dom) = temp.YMD
        temp = if (dom == 28 && mm == Month.FEBRUARY && yy.isLeap) temp + 1 else temp

        if (temp >= date1) {
          sum += 1.0
          newD2 = temp
        }
      }

      val (yy2, mm2, dd) = newD2.YMD

      var den = 365.0
      if (yy2.isLeap) {
        val temp = from(29, Month.FEBRUARY, yy2)
        if (newD2 > temp && date1 <= temp) den += 1.0
      } else if (date1.year.isLeap) {
        val temp = from(29, Month.FEBRUARY, date1.year)
        if (newD2 > temp && date1 <= temp) den += 1.0
      }
      sum + (date1.dailyDifference(newD2) / den)

    }
  }

  override def yearFraction(date1: D, date2: D,
                            refDate1: Option[D] = None,
                            refDate2: Option[D] = None): Double = convention match {
    case ISMA | Bond => yearFractionISMA(date1, date2, refDate1, refDate2)
    case ISDA | Historical | Actual365 => yearFractionISDA(date1, date2, refDate1, refDate2)
    case AFB | Euro => yearFractionAFB(date1, date2, refDate1, refDate2)
  }

  override val toString: String = convention match {
    case ISMA | Bond => "Actual/Actual (ISMA)"
    case ISDA | Historical | Actual365 => "Actual/Actual (ISDA)"
    case AFB | Euro => "Actual/Actual (AFB)"
  }
}

