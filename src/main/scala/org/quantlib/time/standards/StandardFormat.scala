package org.quantlib.time.standards

import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 13/03/2017.
  */
abstract class StandardFormat {
  
  def confirm[D: DateOps](date: D, mainCycle: Boolean): Boolean

  def confirm[D: DateOps](code: String, mainCycle: Boolean): Boolean

  def addDate[D: DateOps](date: D, mainCycle: Boolean): StandardFormat

  def removeDate[D: DateOps](date: D,mainCycle: Boolean): StandardFormat

  // def nextDates(code: String): List[D]

//  def nextDates(date: D): List[D]

  def toCode[D: DateOps](date: D): String

  def toDate[D: DateOps](code: String, refDate: D): Option[D]

  def nextCode[D: DateOps](code: String, mainCycle: Boolean, refDate: D): String

  def nextCode[D: DateOps](date: D,mainCycle: Boolean): String

  def nextDate[D: DateOps](code: String, mainCycle: Boolean, refDate: D): Option[D]

  def nextDate[D: DateOps](date: D, mainCycle: Boolean): Option[D]

}
