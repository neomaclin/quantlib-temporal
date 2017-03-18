package org.quantlib.time.calendars

import org.quantlib.time.implicits.DateOps


/**
  * Created by neo on 13/03/2017.
  */
final case class AdHodCalendar[D: DateOps](baseCalendar: Option[BusinessCalendar[D]] = None,
                                           name: String,
                                           holidays: List[D]
                                          ) extends BusinessCalendar[D] with Modification[D] {


  override val toString: String = baseCalendar.map {
    calendar => s"$calendar :with additional holidays ${holidays.mkString(",")})"
  }.getOrElse{
    if (name.trim.isEmpty) "NullCalendar" else s"$name with ${holidays.mkString(",")})"
  }

  override def isWeekend(date: D): Boolean = baseCalendar.exists( _.isWeekend(date) )

  override def considerBusinessDay(date: D): Boolean = baseCalendar.exists {
    calendar => calendar.considerBusinessDay(date) || !(isWeekend(date) || holidays.contains(date))
  }

  override def addHoliday(date: D): BusinessCalendar[D] with Modification[D] = baseCalendar match {
    case None =>
      if (name.trim.isEmpty) this //does not make sense to modify Null Calendar
      else copy(holidays = date :: holidays)
    case Some(calendar) =>
      calendar match {
        case asAdoc: AdHodCalendar[D] => copy(baseCalendar = asAdoc.baseCalendar, holidays = date :: holidays) //avoid deep nesting od AdhocCalendar
        case _ => copy(holidays = date :: holidays)
      }
  }


  override def removeHoliday(date: D): BusinessCalendar[D] with Modification[D] = baseCalendar match {
    case None =>
      if (name.trim.isEmpty) this //does not make sense to modify Null Calendar
      else copy(holidays = holidays.filterNot(_ == date))
    case Some(calendar) =>
      calendar match {
        case asAdoc: AdHodCalendar[D] => copy(baseCalendar = asAdoc.baseCalendar, holidays = holidays.filterNot(_ == date)) //avoid deep nesting od AdhocCalendar
        case _ => copy(holidays = holidays.filterNot(_ == date))
      }
  }
}
