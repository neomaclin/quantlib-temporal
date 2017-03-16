package org.quantlib.time.calendars

import org.quantlib.time.implicits.DateOps


/**
  * Created by neo on 13/03/2017.
  */
final case class AdHodCalendar[D: DateOps](baseCalendar: Option[BusinessCalendar[D]] = None,
                                           additionalWeekends: List[D],
                                           additionalHolidays: List[D]
                                          ) extends BusinessCalendar[D] with Modification[D] {


  override val toString: String = baseCalendar.map {
    calendar => s"$calendar :with additional weekends (${additionalWeekends.mkString(",")}) and holidays ${additionalHolidays.mkString(",")})"
  }.getOrElse("NullCalendar")


  override def isWeekend(date: D): Boolean = baseCalendar.exists {
    calendar => calendar.isWeekend(date) || additionalWeekends.contains(date)
  }

  override def considerBusinessDay(date: D): Boolean = baseCalendar.exists {
    calendar => calendar.considerBusinessDay(date) || !isWeekend(date) || !additionalHolidays.contains(date)
  }

  override def addHoliday(date: D): BusinessCalendar[D] with Modification[D] = baseCalendar match {
    case None => this //does not make sense to modify Null Calendar
    case Some(calendar) =>
      calendar match {
        case asAdoc: AdHodCalendar[D] => copy(baseCalendar = asAdoc.baseCalendar, additionalHolidays = date :: additionalHolidays) //avoid deep nesting od AdhocCalendar
        case _ => copy(additionalHolidays = date :: additionalHolidays)
      }
  }

  override def addWeekend(date: D): BusinessCalendar[D] with Modification[D] = baseCalendar match {
    case None => this //does not make sense to modify Null Calendar
    case Some(calendar) =>
      calendar match {
        case asAdoc: AdHodCalendar[D] => copy(baseCalendar = asAdoc.baseCalendar, additionalWeekends = date :: additionalWeekends) //avoid deep nesting od AdhocCalendar
        case _ => copy(additionalWeekends = date :: additionalWeekends)
      }
  }

  override def removeWeekend(date: D): BusinessCalendar[D] with Modification[D] = baseCalendar match {
    case None => this //does not make sense to modify Null Calendar
    case Some(calendar) =>
      calendar match {
        case asAdoc: AdHodCalendar[D] => copy(baseCalendar = asAdoc.baseCalendar, additionalWeekends = additionalWeekends.filterNot(_ == date)) //avoid deep nesting od AdhocCalendar
        case _ => copy(additionalWeekends = additionalWeekends.filterNot(_ == date))
      }
  }

  override def removeHoliday(date: D): BusinessCalendar[D] with Modification[D] = baseCalendar match {
    case None => this //does not make sense to modify Null Calendar
    case Some(calendar) =>
      calendar match {
        case asAdoc: AdHodCalendar[D] => copy(baseCalendar = asAdoc.baseCalendar, additionalHolidays = additionalHolidays.filterNot(_ == date)) //avoid deep nesting od AdhocCalendar
        case _ => copy(additionalHolidays = additionalHolidays.filterNot(_ == date))
      }
  }
}
