package org.quantlib.time.standards

/**
  * Created by neo on 13/03/2017.
  */
abstract class StandardFormat[D] {
  
  def confirm(date: D, mainCycle: Boolean): Boolean

  def confirm(code: String, mainCycle: Boolean): Boolean

  def addDate(date: D, mainCycle: Boolean): StandardFormat[D]

  def removeDate(date: D,mainCycle: Boolean): StandardFormat[D]

  // def nextDates(code: String): List[D]

//  def nextDates(date: D): List[D]

  def toCode(date: D): String

  def toDate(code: String, refDate: D): Option[D]

  def nextCode(code: String, mainCycle: Boolean, refDate: D): Option[String]

  def nextCode(date: D,mainCycle: Boolean): Option[String]

  def nextDate(code: String, mainCycle: Boolean, refDate: D): Option[D]

  def nextDate(date: D, mainCycle: Boolean): Option[D]

}
