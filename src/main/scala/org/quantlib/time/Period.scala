package org.quantlib.time

import org.quantlib.time.enums.{Frequency, TimeUnit}
import org.quantlib.time.enums.Frequency._
import org.quantlib.time.enums.TimeUnit._

object Period {

  val Empty = Period()
  val Unknown = Period(999)

  val daysMinMax: Period => (Long, Long) = {
    case Period(length, Days) => (length, length)
    case Period(length, Weeks) => (7 * length, 7 * length)
    case Period(length, Months) => (28 * length, 31 * length)
    case Period(length, Years) => (365 * length, 366 * length)
    case _ => (Long.MinValue, Long.MaxValue)
  }

  implicit object PeriodOrder extends Ordering[Period] {

    def <(p1: Period, p2: Period): Boolean = (p1, p2) match {
      case (Period(0, _), Period(y, _)) => y > 0
      case (Period(x, _), Period(0, _)) => x < 0
      case (Period(x, xunit), Period(y, yunit)) if xunit == yunit => x < y
      case (Period(x, Months), Period(y, Years)) => x < 12 * y
      case (Period(x, Years), Period(y, Months)) => 12 * x < y
      case (Period(x, Days), Period(y, Weeks)) => x < 7 * y
      case (Period(x, Weeks), Period(y, Days)) => 7 * x < y
      case _ =>
        val (xmin, xmax) = daysMinMax(p1)
        val (ymin, ymax) = daysMinMax(p2)

        xmax < ymin

    }


    def >(p1: Period, p2: Period): Boolean = (p1, p2) match {
      case (Period(0, _), Period(y, _)) => y < 0
      case (Period(x, _), Period(0, _)) => x > 0
      case (Period(x, xunit), Period(y, yunit)) if xunit == yunit => x > y
      case (Period(x, Months), Period(y, Years)) => x > 12 * y
      case (Period(x, Years), Period(y, Months)) => 12 * x > y
      case (Period(x, Days), Period(y, Weeks)) => x > 7 * y
      case (Period(x, Weeks), Period(y, Days)) => 7 * x > y
      case _ =>
        val (xmin, xmax) = daysMinMax(p1)
        val (ymin, ymax) = daysMinMax(p2)

        xmax > ymin

    }


    override def compare(x: Period, y: Period): Int = if (<(x, y)) -1 else if (>(x, y)) 1 else 0
  }

  def from(freq: Frequency): Period = freq match {
    case NoFrequency => Empty
    case Once => Period(0, Years)
    case Annual => Period(1, Years)
    case Semiannual | EveryFourthMonth | Quarterly | Bimonthly | Monthly => Period(12 / freq.value, Months)
    case EveryFourthWeek | Biweekly | Weekly => Period(52 / freq.value, Weeks)
    case Daily => Period(1)
    case _ => Unknown
  }

}

final case class Period(length: Long = 0, unit: TimeUnit = Days) {

  def normalize: Period = if (length != 0) {
    unit match {
      case Days => if (length % 7 == 0) Period(length / 7, Weeks) else this
      case Months => if (length % 12 == 0) Period(length / 12, Years) else this
      case _ => this
    }
  } else {
    this
  }

  def unary_- : Period = Period(-length, unit)

  def *(n: Int): Period = Period(length*n, unit)

  def *:(n: Int): Period = *(n)

  def *(n: Long): Period = Period(length*n, unit)

  def *:(n: Long): Period = *(n)


  def /(n: Long): Period = {
    require(n != 0, "cannot be divided by zero")
    if (length % n == 0) {
      Period(length / n, unit)
    } else {
      unit match {
        case Years => Period( (length * 12 ) / n, Months)
        case Weeks => Period( (length * 7 ) /n, Days)
        case _ => Period(length / n, unit)
      }
    }
  }
  
  def +(p: Period): Period = {
    if (length == 0) {
      Period(p.length,p.unit)
    } else if (unit == p.unit ) {
      Period( length + p.length, p.unit)
    } else {
      (unit, p.unit) match {
        case (Years, Months) => Period(length * 12 + p.length, Months)
        case (Months, Years) => Period(length + p.length*12, Months)
        case (Weeks, Days) => Period(length * 7 + p.length, Days)
        case (Days, Weeks) => Period(length + p.length * 7 , Days)
        case (_,_) => ???
      }
    }
  }

  def -(p: Period): Period = this.+( -p )


  private def descriptions(toDays: => String)(toWeeks: => String)(toMonths: => String)(toYears: => String)(toDayFraction: => String): String = {
    unit match {
      case Days => toDays
      case Weeks => toWeeks
      case Months => toMonths
      case Years => toYears
      case _ => toDayFraction
    }
  }

  def shortDescription: String =
    descriptions {
      //toDays
      val week = length / 7 match {
        case 0 => ""
        case x => x + "W"
      }
      val day = length % 7 match {
        case 0 => ""
        case x => x + "D"
      }
      week + day
    } {
      //toWeeks
      length + "W"
    } {
      //toMonths
      val year = length / 12 match {
        case 0 => ""
        case x => x + "Y"
      }
      val month = length % 12 match {
        case 0 => ""
        case x => x + "M"
      }
      year + month
    } {
      //toYears
      length + "Y"
    } {
      ""
    }

  def longDescription: String =
    descriptions {
      //toDays
      val week = length / 7 match {
        case 0 => ""
        case 1 => "1 week "
        case x => x + " weeks "
      }
      val day = length % 7 match {
        case 0 => ""
        case 1 => "1 day"
        case x => x + " days"
      }
      (week + day).trim
    } {
      //toWeeks
      if (length == 1) s"1 week" else length + " weeks"
    } {
      //toMonths
      val year = length / 12 match {
        case 0 => ""
        case 1 => "1 year "
        case x => x + " years "
      }
      val month = length % 12 match {
        case 0 => ""
        case 1 => "1 month"
        case x => x + " months"
      }
      (year + month).trim
    } {
      //toYears
      if (length == 1) "1 year" else length + " years"
    }{
      ""
    }


  def frequency: Frequency =
    Math.abs(length) match {
      case 0 =>
        unit match {
          case Years => Once
          case _ => NoFrequency
        }
      case l =>
        unit match {
          case Years =>
            l match {
              case 1 => Annual
              case _ => OtherFrequency
            }
          case Months =>
            l match {
              case 6 => Semiannual
              case 4 => EveryFourthMonth
              case 3 => Quarterly
              case 2 => Bimonthly
              case 1 => Monthly
              case _ => OtherFrequency
            }
          case Weeks =>
            l match {
              case 1 => Weekly
              case 2 => Biweekly
              case 4 => EveryFourthWeek
              case _ => OtherFrequency
            }
          case Days =>
            l match {
              case 1 => Daily
              case _ => OtherFrequency
            }
          case _ => OtherFrequency
        }
    }


  private def unitMatching(matches: TimeUnit => Double): Double = if (length == 0) 0.0 else matches(unit)

  def years: Double = unitMatching {
    case Days => Double.NaN
    case Weeks => Double.NaN
    case Months => length / 12.0
    case Years => length
    case _ => Double.NaN
  }

  def months: Double = unitMatching {
    case Days => Double.NaN
    case Weeks => Double.NaN
    case Months => length
    case Years => length * 12.0
    case _ => Double.NaN
  }


  def weeks: Double = unitMatching {
    case Days => length / 7.0
    case Weeks => length
    case Months => Double.NaN
    case Years => Double.NaN
    case _ => Double.NaN
  }

  def days: Double = unitMatching {
    case Days => length
    case Weeks => length * 7.0
    case Months => Double.NaN
    case Years => Double.NaN
    case _ => Double.NaN
  }

  override val toString: String = longDescription
}

