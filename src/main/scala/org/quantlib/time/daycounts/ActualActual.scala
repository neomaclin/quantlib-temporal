package org.quantlib.time.daycounts

import java.time._

import org.quantlib.time.Period
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
import org.quantlib.time.daycounts.ActualActual.Convention
import org.quantlib.time.daycounts.ActualActual.Convention._
import org.quantlib.time.enums.TimeUnit
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


object ActualActual{

  sealed trait Convention

  object Convention{
    case object ISMA extends Convention
    case object Bond extends Convention
    case object ISDA extends Convention
    case object Historical extends Convention
    case object Actual365 extends Convention
    case object AFB extends Convention
    case object Euro extends Convention
  }
}


final case class ActualActual[D: DateOps](convention: Convention = ISDA) extends DayCountBasis[D]{

  def dayCount(date1: D, date2: D): Long = if (date2 >= date1) 1 else -1

  private def yearFractionISMA(date1: D, date2: D,
                               refDate1: Option[D],
                               refDate2: Option[D]): Double  = {
    if (date1 == date2) {
      0.0
    } else if (date1 > date2) {
      -yearFractionISMA(date2,date1,refDate1,refDate2)
    }
    else {
      val refPeriodStart = refDate1 getOrElse date1
      val refPeriodEnd = refDate2 getOrElse date2

      require(refPeriodEnd > refPeriodStart && refPeriodEnd > date1,
        s"invalid reference period: date 1: $date1, date 2: $date2" +
          s", reference period start: $refPeriodStart, reference period end: $refPeriodEnd")

      val duration = refPeriodEnd.daysBetween(refPeriodStart)
      val (months, refStart, refEnd) = (0.5 + 12 * duration.toDouble / 365).toInt match {
        case 0 => (12, date1, date1 + Period(1, Years))
        case i => (i, refPeriodStart, refPeriodEnd)
      }

      val period = months / 12.0

      if (date2 <= refEnd) {
        // here refPeriodEnd is a future (notional?) payment date
        if (date1 >= refStart) {
          // here refPeriodStart is the last (maybe notional) payment date.
          // refPeriodStart <= date1 <= date2 <= refPeriodEnd
          // [ maybe the equality should be enforced,
          //   since refPeriodStart < date1 <= date2 < refPeriodEnd could give wrong results ] ???
          period * dayCount(date1,date2) / dayCount(refPeriodStart,refPeriodEnd)
        } else {
          // here refPeriodStart is the next (maybe notional) payment date and refPeriodEnd is the second next
          // (maybe notional) payment date. date1 < refPeriodStart < refPeriodEnd AND date2 <= refPeriodEnd
          // this case is long first coupon

          // the last notional payment date
          val previousRef = refStart - Period(months, Months)
          if (date2 > refStart)
            yearFractionISMA(date1, refStart, Some(previousRef), Some(refStart)) +
              yearFractionISMA(refStart, date2, Some(refStart), Some(refEnd))
          else
            yearFractionISMA(date1,date2,Some(previousRef),Some(refStart))
        }
      } else {
        // here refPeriodEnd is the last (notional?) payment date
        // date1 < refPeriodEnd < date2 AND refPeriodStart < refPeriodEnd
        require(refStart <= date1, "invalid dates: date1 < refPeriodStart < refPeriodEnd < date2")
        // now it is: refPeriodStart <= date1 < refPeriodEnd < date2

        // the part from date1 to refPeriodEnd
        val date1ToRefEnd = yearFractionISMA(date1, refEnd, Some(refStart), Some(refEnd))

        // the part from refPeriodEnd to date2
        // count how many regular periods are in [refPeriodEnd, date2],
        // then add the remaining org.quantlib.time

        @tailrec
        def sumUp(i:Int, sum: Double): (Double, D, D) = {
          val newRefStart = refEnd + Period(months*i, Months)
          val newRefEnd = refEnd + Period(months*(i+1), Months)
          if (date2 < newRefEnd) (sum, newRefStart, newRefEnd) else sumUp(i+1, sum+period)
        }
        val (remaining, newRefStart, newRefEnd) = sumUp(0, date1ToRefEnd)

        remaining + yearFractionISMA(newRefStart, date2, Some(newRefStart), Some(newRefEnd))
      }
    }


  }

  private def yearFractionISDA(date1: D, date2: D,
                               refDate1: Option[D],
                               refDate2: Option[D]): Double = {
    def base(date: D):Double = if (date.year.isLeap) 366.0 else 365.0
    if (date1 == date2) {
      0.0
    } else if (date1 > date2) {
      -yearFractionISDA(date2, date1, None, None)
    } else {
        val years = date1 to (date2, Years)
        val nextDay = date1 + Period(1, Days)

        val fractionOfYearStart = dayCount(date1, from(1,  Month.JANUARY, date1.year.plusYears(1))) / base(date1)
        val fractionOfYearEnd = dayCount(from(1, Month.JANUARY, date2.year), date2) / base(date2)

        years + fractionOfYearStart + fractionOfYearEnd
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
        val (yy1, mm1, ddate1) = date1.YMD
        val (yy2, mm2, ddate2) = date2.YMD

        val years = date1 to (date2, TimeUnit.Years)
        val febInStart = from(29, Month.FEBRUARY, yy1) >= date1

        if (years > 1) {
          val fractionOfYearEndDate = from(ddate2, mm2, yy2.minusYears(years))
          val yearBase = if (febInStart) 366 else 365

          years.toDouble + (date1 to (fractionOfYearEndDate, TimeUnit.Days)).toDouble/ yearBase
        } else {
          val febInEnd = from(29, Month.FEBRUARY, yy2) < date2
          val yearBase = if (febInStart || febInEnd) 366 else 365

          (date1 to (date2, TimeUnit.Days)).toDouble / yearBase
        }
    }
  }

  private val yearFractionImpl:(D,D,Option[D],Option[D]) => Double = convention match {
    case ISMA | Bond => yearFractionISMA
    case ISDA | Historical | Actual365 => yearFractionISDA
    case AFB | Euro => yearFractionAFB
  }

  override def yearFraction(date1: D, date2: D,
                            refDate1: Option[D],
                            refDate2: Option[D]): Double = yearFractionImpl(date1,date2,refDate1,refDate2)

  override val toString:String = convention match {
    case ISMA | Bond => "Actual/Actual (ISMA)"
    case ISDA | Historical | Actual365 => "Actual/Actual (ISDA)"
    case AFB | Euro => "Actual/Actual (AFB)"
  }
}

