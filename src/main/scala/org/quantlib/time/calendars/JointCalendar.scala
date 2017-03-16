package org.quantlib.time.calendars

import java.time.LocalDate

import org.quantlib.time.calendars.JointCalendar.JointCalendarRule
import org.quantlib.time.calendars.JointCalendar.JointCalendarRule._
import org.quantlib.time.implicits.DateOps

/**
  * Created by neo on 11/03/2017.
  */
object JointCalendar {

  sealed trait JointCalendarRule

  object JointCalendarRule {

    case object JoinHolidays extends JointCalendarRule

    // A date is a holiday for the joint calendar if it is a holiday for any of the given calendars

    case object JoinBusinessDays extends JointCalendarRule

    // A date is a business day  for the joint calendar if it is a business day for any of the given calendars

  }

}

final case class JointCalendar[D: DateOps](calendars: List[BusinessCalendar[D]],
                               rule: JointCalendarRule = JoinHolidays) extends BusinessCalendar[D] {

  def isWeekend(date: D): Boolean = rule match {
    case JoinHolidays => calendars.exists(_.isWeekend(date))
    case JoinBusinessDays => !calendars.exists(!_.isWeekend(date))
  }

  def considerBusinessDay(date: D): Boolean = rule match {
    case JoinHolidays => !calendars.exists(!_.considerBusinessDay(date))
    case JoinBusinessDays => calendars.exists(_.considerBusinessDay(date))
  }

  override val toString: String = s"$rule(${calendars.mkString(", ")})"
}