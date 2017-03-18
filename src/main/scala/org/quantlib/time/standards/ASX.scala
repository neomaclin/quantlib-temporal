package org.quantlib.time.standards

import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
import java.time.Month._
import java.time.DayOfWeek._
import java.time.Year

/**
  * Created by neo on 09/03/2017.
  */
object ASX{
  private val num = "0123456789".toList
  private val codeShort = "hmzuHMZU".toList
  private val codeLong = "fghjkmnquvxzFGHJKMNQUVXZ".toList

}
final case class ASX[D: DateOps]() extends StandardFormat[D] {
  import ASX._

  def confirm(date: D, mainCycle: Boolean): Boolean = {
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


  def confirm(code: String, mainCycle: Boolean): Boolean = {
    if (code.size != 2) {
      false
    } else {
      val monthNum = code.head
      val monthCode = code.tail
      if (num.contains(monthNum)) {
        if (mainCycle) codeShort.contains(monthCode) else codeLong.contains(monthCode)
      } else {
        false
      }
    }
  }

  def toCode(date: D): String = {
    require(confirm(date, mainCycle = false), s" is not an valid ASX date")
    val monthNum = date.year.getValue % 10
    val monthCode = date.month match {
      case JANUARY => 'F'
      case FEBRUARY => 'G'
      case MARCH => 'H'
      case APRIL => 'J'
      case MAY => 'K'
      case JUNE => 'M'
      case JULY => 'N'
      case AUGUST => 'Q'
      case SEPTEMBER => 'U'
      case OCTOBER => 'V'
      case NOVEMBER => 'X'
      case DECEMBER => 'Z'
    }
    s"$monthNum$monthCode"
  }

  def toDate(code: String, refDate: D): Option[D] = {
    require(confirm(code, mainCycle = false), s" is not an valid ASX Code")
    val monthCode = code.tail

    val monthOption = monthCode match {
      case "F" => Some(JANUARY)
      case "G" => Some(FEBRUARY)
      case "H" => Some(MARCH)
      case "J" => Some(APRIL)
      case "K" => Some(MAY)
      case "M" => Some(JUNE)
      case "N" => Some(JULY)
      case "Q" => Some(AUGUST)
      case "U" => Some(SEPTEMBER)
      case "V" => Some(OCTOBER)
      case "X" => Some(NOVEMBER)
      case "Z" => Some(DECEMBER)
      case _ => None
    }
    monthOption.flatMap{ month =>

      val monthNum = code.head.toInt
      val year = if (monthNum == 0 && refDate.year.getValue <= 1909) monthNum else monthNum + 10
      val refYear = refDate.year.getValue  + year - (refDate.year.getValue % 10)
      val result = nextDate(DateOps.from(1, month, Year.of(refYear)), mainCycle = false)
      if (result.exists(_ < refDate)) nextDate(DateOps.from(1, month, Year.of(refYear + 10)), mainCycle = false) else result
    }

  }

  def nextDate(d: D, mainCycle: Boolean): Option[D] = ???

  def nextDate(code: String, mainCycle: Boolean, refDate: D): Option[D] = {
    toDate(code, refDate).flatMap(date => nextDate(date, mainCycle))
  }

  def nextCode(d: D, mainCycle: Boolean): String = nextDate(d, mainCycle) map toCode getOrElse ""

  def nextCode(code: String, mainCycle: Boolean, refDate: D): String = nextDate(code, mainCycle, refDate) map toCode getOrElse ""

  def addDate(date: D, mainCycle: Boolean): StandardFormat[D] = this

  def removeDate(date: D, mainCycle: Boolean): StandardFormat[D] = this

}
