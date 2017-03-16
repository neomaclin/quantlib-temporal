package org.quantlib.time.standards

import java.time._

import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

import scala.language.implicitConversions

object IMM{

  private val MonthCodes = "FGHJKMNQUVXZ"
  private val Months = MonthCodes.toList.map(_.toString)
  private val codeRegex = s"""([$MonthCodes])(\d)""".r
  private val mainCycleRegex = """([HMZU])(\d)""".r

}
/** Main cycle of the International %Money Market (a.k.a. %IMM) months */
final case class IMM[D: DateOps]() extends StandardFormat[D] {
  import IMM._

  def confirm(date: D, mainCycle: Boolean): Boolean = {
    date.dow match {
      case DayOfWeek.WEDNESDAY =>
        date.dom match {
          case d if d < 15 || d > 21 => false
          case _ =>
            if (!mainCycle) true
            else date.month match {
              case Month.MARCH | Month.JUNE | Month.SEPTEMBER | Month.DECEMBER => true
              case _ => false
            }
        }
      case _ => false
    }
  }

  def confirm(code: String, mainCycle: Boolean): Boolean = {
    val matching = if (mainCycle) mainCycleRegex else codeRegex
    code.toUpperCase.matches(matching.regex)
  }

  def toCode(date: D): String = {
      val (y,m,d) = date.YMD
      if (!confirm(date, mainCycle = false)) "" else (m.getValue - 1 + (y.getValue % 10)).toString
    }

  def toDate(immCode: String, refDate: D): Option[D] = ???

  //  {
  //    if (isIMMCode(immCode, mainCycle = false))
  //      immCode.toUpperCase match {
  //        case codeRegex(c, y) =>
  //          val month = Months.indexOf(c) + 1
  //          val refYear = refDate.year
  //          val year = refYear + (if (y.toInt == 0 && refYear <= 1909) y + 10 else y).toInt - (refYear % 10)
  //          val result = nextDate(new D(year, month, 1), mainCycle = false)
  //          Some(if (result < refDate) nextDate(new D(year + 10, month, 1), mainCycle = false) else result)
  //        case _ => None
  //      }
  //    else None
  //  }


  def nextDate(date: D, mainCycle: Boolean = true): Option[D] = ???

  //  {
  //    val (refYear, refYearMonth, refDay) = date.YMD
  //
  //    val offset = if (mainCycle) 3 else 1
  //    val skipping = offset - (refYearMonth % offset)
  //
  //    val (year, month) = if (skipping != offset || refDay > 21) {
  //      val nextCycleMonth = refYearMonth + skipping
  //      if (nextCycleMonth <= 12) (refYear, nextCycleMonth) else (refYear + 1, nextCycleMonth - 11)
  //    } else {
  //      (refYear, refYearMonth)
  //    }
  //
  //    val result = nthWeekday(3, DayOfWeek.WEDNESDAY, month, year)
  //    if (result <= date) nextDate(D.of(year, month, 22), mainCycle) else result
  //  }

  def nextDate(code: String, mainCycle: Boolean, refDate: D): Option[D] = ???

  //  {
  //    date(immCode, refDate).map{ immDate => nextDate(immDate.plusDays(1), mainCycle) }
  //  }

  def nextCode(date: D, mainCycle: Boolean): Option[String] = {
    nextDate(date, mainCycle).map{toCode}
  }

  def nextCode(code: String, mainCycle: Boolean, refDate: D): Option[String] = {
    nextDate(code, mainCycle, refDate).map{toCode}
  }

  override def addDate(date: D, mainCycle: Boolean): StandardFormat[D] = this

  override def removeDate(date: D, mainCycle: Boolean): StandardFormat[D] = this


}
