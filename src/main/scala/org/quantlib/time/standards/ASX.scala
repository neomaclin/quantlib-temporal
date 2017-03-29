package org.quantlib.time.standards

import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

import java.time.Month._
import java.time.DayOfWeek._
import java.time.{DayOfWeek, Month, Year}

/**
  * Created by neo on 09/03/2017.
  */

case object ASX extends StandardFormat {
  private val MonthCodes = "FGHJKMNQUVXZ"
  private val Months = MonthCodes.toList.map(_.toString)
  private val codeRegex = s"""([$MonthCodes])([0-9])""".r
  private val mainCycleRegex = """([HMZU])([0-9])""".r

  def confirm[D: DateOps](date: D, mainCycle: Boolean): Boolean = {
    if (date.dow != FRIDAY) {
      false
    } else {
      val dom = date.dom
      if (dom < 8 || dom > 14) {
        false
      } else {
        if (!mainCycle) {
          true
        } else {
          date.month match {
            case MARCH | JUNE | SEPTEMBER | DECEMBER => true
            case _ => false
          }
        }
      }
    }
  }


  def confirm[D: DateOps](code: String, mainCycle: Boolean): Boolean = {
    val matching = if (mainCycle) mainCycleRegex else codeRegex
    code.toUpperCase.matches(matching.regex)
  }

  def toCode[D: DateOps](date: D): String = {
    require(confirm(date, mainCycle = false), s" date is not an valid ASX date")
    val (y, m, d) = date.YMD
    s"${Months(m.getValue - 1)}${Math.abs(y.getValue) % 10}"
  }

  def toDate[D: DateOps](code: String, refDate: D): Option[D] = {
    require(confirm(code, mainCycle = false), s" is not an valid ASX Code")
    if (confirm(code, mainCycle = false))
      code.toUpperCase match {
        case codeRegex(c, y) =>
          val month = Month.of(Months.indexOf(c) + 1)
          val refYear = refDate.year.getValue
          val year = refYear + (if (y.toInt == 0 && refYear <= 1909) y + 10 else y).toInt - (refYear % 10)

          nextDate(DateOps.from(1, month, Year.of(year)), mainCycle = false).flatMap { date =>
            if (date < refDate) nextDate(DateOps.from(1, month, Year.of(year + 10)), mainCycle = false)
            else Some(date)
          }
        case _ => None
      }
    else None

  }

  def nextDate[D: DateOps](date: D, mainCycle: Boolean): Option[D] = {

    val (refYear, refYearMonth, refDay) = date.YMD

    val offset = if (mainCycle) 3 else 1
    val skipping = offset - (refYearMonth.getValue % offset)

    val (year, month) = if (skipping != offset || refDay > 14) {
      val nextCycleMonth = refYearMonth.getValue + skipping
      if (nextCycleMonth <= 12) (refYear, Month.of(nextCycleMonth)) else (refYear + 1, Month.of(nextCycleMonth - 11))
    } else {
      (refYear, refYearMonth)
    }

    val result = DateOps.nthWeekday(2, DayOfWeek.FRIDAY, month, year)
    if (result <= date) nextDate(DateOps.from(15, month, year), mainCycle) else Some(result)
  }

  def nextDate[D: DateOps](code: String, mainCycle: Boolean, refDate: D): Option[D] = {
    toDate(code, refDate).flatMap(date => nextDate(date, mainCycle))
  }

  def nextCode[D: DateOps](d: D, mainCycle: Boolean): String = nextDate(d, mainCycle) map toCode[D] getOrElse ""

  def nextCode[D: DateOps](code: String, mainCycle: Boolean, refDate: D): String = {
    nextDate(code, mainCycle, refDate) map toCode[D] getOrElse ""
  }

  def addDate[D: DateOps](date: D, mainCycle: Boolean): StandardFormat = this

  def removeDate[D: DateOps](date: D, mainCycle: Boolean): StandardFormat = this

}
