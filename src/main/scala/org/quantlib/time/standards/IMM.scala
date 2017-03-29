package org.quantlib.time.standards

import java.time._

import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/** Main cycle of the International %Money Market (a.k.a. %IMM) months */
case object IMM extends StandardFormat {
  private val MonthCodes = "FGHJKMNQUVXZ"
  private val Months = MonthCodes.toList.map(_.toString)
  private val codeRegex = s"""([$MonthCodes])([0-9])""".r
  private val mainCycleRegex = """([HMZU])([0-9])""".r

  def confirm[D: DateOps](date: D, mainCycle: Boolean): Boolean = {
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

  def confirm[D: DateOps](code: String, mainCycle: Boolean): Boolean = {
    val matching = if (mainCycle) mainCycleRegex else codeRegex
    code.toUpperCase.matches(matching.regex)
  }

  def toCode[D: DateOps](date: D): String = {
     require(confirm(date, mainCycle = false), s"$date is not an valid IMM date")
      val (y,m,d) = date.YMD
      s"${Months(m.getValue - 1)}${Math.abs(y.getValue % 10)}"
    }

  def toDate[D: DateOps](immCode: String, refDate: D): Option[D] = {
      if (confirm(immCode, mainCycle = false))
        immCode.toUpperCase match {
          case codeRegex(c, y) =>
            val month = Months.indexOf(c) + 1
            val refYear = refDate.year.getValue
            val year = refYear + (if (y.toInt == 0 && refYear <= 1909) y + 10 else y).toInt - (refYear % 10)

            nextDate(DateOps.from(1, Month.of(month), Year.of(year)), mainCycle = false).flatMap { date=>
              if (date < refDate) nextDate(DateOps.from(1, Month.of(month), Year.of(year + 10)), mainCycle = false)
              else Some(date)
            }
          case _ => None
        }
      else None
    }


  def nextDate[D: DateOps](date: D, mainCycle: Boolean = true): Option[D] = {

      val (refYear, refYearMonth, refDay) = date.YMD

      val offset = if (mainCycle) 3 else 1
      val skipping = offset - (refYearMonth.getValue % offset)

      val (year, month) = if (skipping != offset || refDay > 21) {
        val nextCycleMonth = refYearMonth.getValue + skipping
        if (nextCycleMonth <= 12) (refYear, Month.of(nextCycleMonth)) else (refYear + 1, Month.of(nextCycleMonth - 11))
      } else {
        (refYear, refYearMonth)
      }

      val result = DateOps.nthWeekday(3, DayOfWeek.WEDNESDAY, month, year)
      if (result <= date) nextDate(DateOps.from(22, month, year), mainCycle) else Some(result)
    }

  def nextDate[D: DateOps](code: String, mainCycle: Boolean, refDate: D): Option[D] = {
     toDate(code, refDate).flatMap{ immDate => nextDate(immDate + 1, mainCycle) }
  }

  def nextCode[D: DateOps](date: D, mainCycle: Boolean): String = nextDate(date, mainCycle) map toCode[D] getOrElse ""

  def nextCode[D: DateOps](code: String, mainCycle: Boolean, refDate: D): String = {
    nextDate(code, mainCycle, refDate) map toCode[D] getOrElse ""
  }

  override def addDate[D: DateOps](date: D, mainCycle: Boolean): StandardFormat  = this

  override def removeDate[D: DateOps](date: D, mainCycle: Boolean): StandardFormat = this


}
