package org.quantlib.time.calendars

import org.quantlib.time.calendars.Russia.Market
import org.quantlib.time.calendars.Russia.Market._
import org.quantlib.time.implicits.DateOps
import org.quantlib.time.implicits.DateOps._

/**
  * Created by neo on 11/03/2017.
  */
object Russia {

  sealed trait Market

  object Market {

    case object Settlement extends Market

    case object MOEX extends Market

  }

}

final case class Russia[D: DateOps](market: Market = Settlement) extends WeekendSatSun[D] with BusinessCalendar[D] {

  private def isRussiaNewYears(date: D) = {
    val dom = date.dom
    dom >= 1 && dom <= 8 && date.inJanuary
  }

  private def isRussiaNewYearsEve(date: D) = date.dom == 31 && date.inDecember


  private def isDefenderoftheFatherlandDay(date: D) = {
    val d = date.dom
    (d == 23 || ((d == 24 || d == 25) && date.dow.isMonday)) && date.inFebruary
  }

  private def isDefenderoftheFatherlandDayMOEX(date: D) = {
    date.dom == 23 && date.inFebruary
  }

  private def isInternationalWomensDay(date: D) = {
    val d = date.dom
    (d == 8 || ((d == 9 || d == 10) && date.dow.isMonday)) && date.inMarch
  }

  private def isLabourDay(date: D) = {
    val d = date.dom
    (d == 1 || ((d == 2 || d == 3) && date.dow.isMonday)) && date.inMay
  }

  private def isVictoryDay(date: D) = {
    val d = date.dom
    (d == 9 || ((d == 10 || d == 11) && date.dow.isMonday)) && date.inMay
  }

  private def isRussiaDay(date: D) = {
    val d = date.dom
    (d == 12 || ((d == 13 || d == 14) && date.dow.isMonday)) && date.inJune
  }

  private def isUnityDay(date: D) = {
    val d = date.dom
    (d == 4 || ((d == 5 || d == 6) && date.dow.isMonday)) && date.inNovember
  }

  private val settlementHolidays = List[D => Boolean](
    isWeekend,
    isRussiaNewYears,
    isDefenderoftheFatherlandDay,
    isInternationalWomensDay,
    isLabourDay,
    isVictoryDay,
    isRussiaDay,
    isUnityDay
  )
  private val moexHolidays = List[D => Boolean](
    isWeekend,
    isDefenderoftheFatherlandDayMOEX,
    isInternationalWomensDay,
    isLabourDay,
    isVictoryDay,
    isRussiaDay,
    isUnityDay,
    isRussiaNewYearsEve,
    isExtraHoliday
  )

  override val toString = market match {
    case Settlement => "Russian settlement"
    case MOEX => "Moscow exchange"
  }

  private def isWorkingWeekend(date: D) = {
    val (y, m, d) = date.YMD
    import java.time.Month._
    y.getValue match {
      case 2012 =>
        m match {
          case MARCH => d == 11
          case APRIL => d == 28
          case MAY => d == 5 || d == 12
          case JUNE => d == 9
          case _ => false
        }
      case 2016 =>
        m match {
          case FEBRUARY => d == 20
          case _ => false
        }
      case _ => false
    }
  }

  private def isExtraHoliday(date: D) = {
    val (y, m, d) = date.YMD
    import java.time.Month._
    y.getValue match {
      case 2012 =>
        m match {
          case JANUARY => d == 2
          case MARCH => d == 9
          case APRIL => d == 30
          case JUNE => d == 11
          case _ => false
        }
      case 2013 =>
        m match {
          case JANUARY => d == 1 || d == 2 || d == 3 || d == 4 || d == 7
          case _ => false
        }
      case 2014 =>
        m match {
          case JANUARY => d == 1 || d == 2 || d == 3 || d == 7
          case _ => false
        }
      case 2015 =>
        m match {
          case JANUARY => d == 1 || d == 2 || d == 7
          case _ => false
        }
      case 2016 =>
        m match {
          case JANUARY => d == 1 || d == 7 || d == 8
          case MAY => d == 2 || d == 3
          case JUNE => d == 13
          case DECEMBER => d == 30
          case _ => false
        }
      case _ => false
    }
  }

  override def considerBusinessDay(date: D): Boolean = {
    market match {
      case Settlement => !settlementHolidays.exists(f => f(date))
      case MOEX =>
        require(date.year >= 2012, s"MOEX calendar for the year ${date.year} does not exist.")
        val isWorking = isWorkingWeekend(date)
        //||
        val isBusinsay = (!moexHolidays.exists(f => f(date)))

        isWorking || !moexHolidays.exists(f => f(date))
    }
  }
}
