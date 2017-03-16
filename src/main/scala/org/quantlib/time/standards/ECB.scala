package org.quantlib.time.standards

import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._
/** European Central Bank reserve maintenance dates */
object ECB {
  private val knownDates = List(
    38371, 38391, 38420, 38455, 38483, 38511, 38546, 38574, 38602, 38637, 38665, 38692 // 2005
    , 38735, 38756, 38784, 38819, 38847, 38883, 38910, 38938, 38966, 39001, 39029, 39064 // 2006
    , 39099, 39127, 39155, 39190, 39217, 39246, 39274, 39302, 39337, 39365, 39400, 39428 // 2007
    , 39463, 39491, 39519, 39554, 39582, 39610, 39638, 39673, 39701, 39729, 39764, 39792 // 2008
    , 39834, 39855, 39883, 39911, 39946, 39974, 40002, 40037, 40065, 40100, 40128, 40155 // 2009
    , 40198, 40219, 40247, 40282, 40310, 40345, 40373, 40401, 40429, 40464, 40492, 40520 // 2010
    , 40562, 40583, 40611, 40646, 40674, 40709, 40737, 40765, 40800, 40828, 40856, 40891 // 2011
    // http://www.ecb.europa.eu/press/pr/date/2011/html/pr110520.en.html
    , 40926, 40954, 40982, 41010, 41038, 41073, 41101, 41129, 41164, 41192, 41227, 41255 // 2012
    , 41290, 41318, 41346, 41374, 41402, 41437, 41465, 41493, 41528, 41556, 41591, 41619 // 2013
    // http://www.ecb.europa.eu/press/pr/date/2013/html/pr130610.en.html
    , 41654, 41682, 41710, 41738, 41773, 41801, 41829, 41864, 41892, 41920, 41955, 41983 // 2014
    // http://www.ecb.europa.eu/press/pr/date/2014/html/pr140717_1.en.html
    , 42032, 42074, 42116, 42165, 42207, 42256, 42305, 42347 // 2015
    // https://www.ecb.europa.eu/press/pr/date/2015/html/pr150622.en.html
    , 42396, 42445, 42487, 42529, 42578, 42627, 42669, 42718 // 2016
    , 42760, /*source ICAP */ 42802, 42844, 42893, 42942 // 2017
  )
  private val num: List[String] = "0123456789".toList.map(_.toString)
  private val codeShort = "hmzuHMZU".toList
  private val codeLong = "fghjkmnquvxzFGHJKMNQUVXZ".toList

}

final case class ECB[D: DateOps](dates: List[Int]) extends StandardFormat[D] {

  import ECB._

  override def confirm(date: D, mainCycle: Boolean): Boolean = nextDate(date - 1, false).contains(date)

  override def confirm(code: String, mainCycle: Boolean): Boolean = {
    if (code.length != 5) {
      false
    } else {
      if (!num.contains(code.substring(3, 1))) {
        false
      } else {
        if (!num.contains(code.substring(4, 1))) {
          false
        } else {
          code.toUpperCase.substring(0, 3) match {
            case "JAN" | "FEB" | "MAR" | "APR" | "MAY" | "JUN" | "JUL" | "AUG" | "SEP" | "OCT" | "NOV" | "DEC" => true
            case _ => false
          }
        }
      }

    }
  }

  override def addDate(date: D, mainCycle: Boolean): StandardFormat[D] = ???

  override def removeDate(date: D, mainCycle: Boolean): StandardFormat[D] = ???

  override def nextDate(code: String, mainCycle: Boolean, refDate: D): Option[D] = ???

  override def nextDate(date: D, mainCycle: Boolean): Option[D] = ???

  override def toCode(date: D): String = ???

  override def toDate(code: String, refDate: D): Option[D] = ???

  override def nextCode(code: String, mainCycle: Boolean, refDate: D): Option[String] = ???

  override def nextCode(date: D, mainCycle: Boolean): Option[String] = ???
}
